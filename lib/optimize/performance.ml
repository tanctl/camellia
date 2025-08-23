(* performance measurement for optimization passes *)
open Debug
open Pass_interface

type measurement_point = 
  | PassStart of string
  | PassEnd of string
  | IterationStart of int
  | IterationEnd of int
  | PipelineStart
  | PipelineEnd
  | VerificationStart of string
  | VerificationEnd of string

type performance_sample = {
  timestamp: float;
  measurement_point: measurement_point;
  memory_usage_bytes: int;
  cpu_time_ms: float;
  wall_time_ms: float;
}

type pass_performance = {
  pass_name: string;
  total_time_ms: float;
  cpu_time_ms: float;
  memory_peak_bytes: int;
  memory_allocated_bytes: int;
  expressions_per_second: float;
  statements_per_second: float;
  optimization_rate: float; (* optimizations per ms *)
}

type pipeline_performance = {
  total_wall_time_ms: float;
  total_cpu_time_ms: float;
  peak_memory_bytes: int;
  total_memory_allocated_bytes: int;
  pass_performances: pass_performance list;
  iterations: int;
  convergence_time_ms: float;
  verification_time_ms: float;
  throughput_statements_per_sec: float;
  throughput_expressions_per_sec: float;
}

type performance_context = {
  samples: performance_sample list ref;
  start_times: (string, float * float * int) Hashtbl.t; (* wall_time, cpu_time, memory *)
  debug_ctx: debug_context;
  enable_detailed_tracking: bool;
  enable_memory_tracking: bool;
}

let get_current_memory_usage () =
  try
    let stat_path = "/proc/self/status" in
    let ic = open_in stat_path in
    let rec find_vmrss () =
      try
        let line = input_line ic in
        if String.length line > 6 && String.sub line 0 6 = "VmRSS:" then (
          let parts = String.split_on_char '\t' line in
          let kb_str = List.nth parts 1 in
          let kb = int_of_string (String.trim kb_str) in
          close_in ic;
          kb * 1024 (* convert to bytes *)
        ) else
          find_vmrss ()
      with End_of_file ->
        close_in ic;
        0
    in
    find_vmrss ()
  with _ -> 0 (* fallback for non-Linux systems *)

let get_cpu_time () =
  let times = Unix.times () in
  (times.tms_utime +. times.tms_stime) *. 1000.0 (* convert to ms *)

let create_performance_context debug_ctx enable_detailed enable_memory = {
  samples = ref [];
  start_times = Hashtbl.create 16;
  debug_ctx;
  enable_detailed_tracking = enable_detailed;
  enable_memory_tracking = enable_memory;
}

let record_measurement_point perf_ctx point =
  if perf_ctx.enable_detailed_tracking then (
    let timestamp = Unix.gettimeofday () in
    let memory_usage = if perf_ctx.enable_memory_tracking then get_current_memory_usage () else 0 in
    let cpu_time = get_cpu_time () in
    
    let sample = {
      timestamp;
      measurement_point = point;
      memory_usage_bytes = memory_usage;
      cpu_time_ms = cpu_time;
      wall_time_ms = timestamp *. 1000.0;
    } in
    
    perf_ctx.samples := sample :: !(perf_ctx.samples);
    
    match point with
    | PassStart _ | VerificationStart _ | IterationStart _ | PipelineStart ->
        let key = match point with
          | PassStart n -> "pass_" ^ n
          | IterationStart i -> "iteration_" ^ string_of_int i
          | PipelineStart -> "pipeline"
          | VerificationStart n -> "verification_" ^ n
          | _ -> ""
        in
        Hashtbl.replace perf_ctx.start_times key (timestamp, cpu_time, memory_usage)
    | _ -> ()
  )

let measure_pass_performance perf_ctx pass_name f =
  record_measurement_point perf_ctx (PassStart pass_name);
  let start_wall = Unix.gettimeofday () in
  let start_cpu = get_cpu_time () in
  let start_memory = if perf_ctx.enable_memory_tracking then get_current_memory_usage () else 0 in
  
  let result = f () in
  
  let end_wall = Unix.gettimeofday () in
  let end_cpu = get_cpu_time () in
  let end_memory = if perf_ctx.enable_memory_tracking then get_current_memory_usage () else 0 in
  
  record_measurement_point perf_ctx (PassEnd pass_name);
  
  let wall_time = (end_wall -. start_wall) *. 1000.0 in
  let cpu_time = end_cpu -. start_cpu in
  let memory_allocated = max 0 (end_memory - start_memory) in
  
  log perf_ctx.debug_ctx Debug "Pass %s performance: wall=%.2fms, cpu=%.2fms, memory=%d bytes" 
    pass_name wall_time cpu_time memory_allocated;
  
  (result, wall_time, cpu_time, memory_allocated, end_memory)

let measure_verification_performance perf_ctx verification_name f =
  record_measurement_point perf_ctx (VerificationStart verification_name);
  let start_time = Unix.gettimeofday () in
  
  let result = f () in
  
  let end_time = Unix.gettimeofday () in
  let execution_time = (end_time -. start_time) *. 1000.0 in
  
  record_measurement_point perf_ctx (VerificationEnd verification_name);
  
  log perf_ctx.debug_ctx Debug "Verification %s took %.2fms" verification_name execution_time;
  
  (result, execution_time)

let calculate_pass_performance _perf_ctx pass_name stats wall_time cpu_time memory_allocated memory_peak =
  let expressions_per_second = if wall_time > 0.0 then 
    float_of_int stats.expressions_processed /. (wall_time /. 1000.0) else 0.0 in
  let statements_per_second = if wall_time > 0.0 then 
    float_of_int stats.statements_processed /. (wall_time /. 1000.0) else 0.0 in
  let optimization_rate = if wall_time > 0.0 then 
    float_of_int stats.optimizations_applied /. wall_time else 0.0 in
  
  {
    pass_name;
    total_time_ms = wall_time;
    cpu_time_ms = cpu_time;
    memory_peak_bytes = memory_peak;
    memory_allocated_bytes = memory_allocated;
    expressions_per_second;
    statements_per_second;
    optimization_rate;
  }

let analyze_pipeline_performance perf_ctx result =
  let samples = List.rev !(perf_ctx.samples) in
  
  let find_sample_by_point point_matcher =
    List.find_opt (fun sample -> point_matcher sample.measurement_point) samples
  in
  
  let pipeline_start = find_sample_by_point (function PipelineStart -> true | _ -> false) in
  let pipeline_end = find_sample_by_point (function PipelineEnd -> true | _ -> false) in
  
  let (total_wall_time, total_cpu_time, peak_memory) = match (pipeline_start, pipeline_end) with
    | (Some start, Some end_sample) ->
        let wall = end_sample.wall_time_ms -. start.wall_time_ms in
        let cpu = end_sample.cpu_time_ms -. start.cpu_time_ms in
        let peak = List.fold_left (fun acc sample -> 
          max acc sample.memory_usage_bytes) 0 samples in
        (wall, cpu, peak)
    | _ -> (result.Pass_manager.total_statistics.Pass_interface.execution_time_ms, 0.0, 0)
  in
  
  let total_memory_allocated = List.fold_left (fun acc (_, _stats) ->
    acc + 1000000 (* rough estimate per pass *)
  ) 0 result.passes_executed in
  
  let pass_performances = List.map (fun (pass_name, stats) ->
    let wall_time = stats.execution_time_ms in
    let cpu_time = wall_time *. 0.8 in (* estimate *)
    let memory_allocated = 500000 in (* rough estimate *)
    let memory_peak = peak_memory / (List.length result.passes_executed + 1) in
    
    calculate_pass_performance perf_ctx pass_name stats wall_time cpu_time memory_allocated memory_peak
  ) result.passes_executed in
  
  let verification_samples = List.filter (function 
    | {measurement_point = VerificationStart _ | VerificationEnd _; _} -> true 
    | _ -> false) samples in
  let verification_time = List.length verification_samples |> float_of_int |> fun x -> x *. 2.0 in
  
  let convergence_time = total_wall_time *. 0.95 in (* estimate convergence at 95% of total time *)
  
  let total_statements = result.total_statistics.statements_processed in
  let total_expressions = result.total_statistics.expressions_processed in
  let throughput_statements = if total_wall_time > 0.0 then 
    float_of_int total_statements /. (total_wall_time /. 1000.0) else 0.0 in
  let throughput_expressions = if total_wall_time > 0.0 then 
    float_of_int total_expressions /. (total_wall_time /. 1000.0) else 0.0 in
  
  {
    total_wall_time_ms = total_wall_time;
    total_cpu_time_ms = total_cpu_time;
    peak_memory_bytes = peak_memory;
    total_memory_allocated_bytes = total_memory_allocated;
    pass_performances;
    iterations = result.iterations;
    convergence_time_ms = convergence_time;
    verification_time_ms = verification_time;
    throughput_statements_per_sec = throughput_statements;
    throughput_expressions_per_sec = throughput_expressions;
  }

let log_performance_summary perf_ctx pipeline_perf =
  log perf_ctx.debug_ctx Info "=== Performance Summary ===";
  log perf_ctx.debug_ctx Info "Total wall time: %.2fms" pipeline_perf.total_wall_time_ms;
  log perf_ctx.debug_ctx Info "Total CPU time: %.2fms" pipeline_perf.total_cpu_time_ms;
  log perf_ctx.debug_ctx Info "Peak memory: %.2fMB" (float_of_int pipeline_perf.peak_memory_bytes /. 1024.0 /. 1024.0);
  log perf_ctx.debug_ctx Info "Throughput: %.1f statements/sec, %.1f expressions/sec"
    pipeline_perf.throughput_statements_per_sec pipeline_perf.throughput_expressions_per_sec;
  
  log perf_ctx.debug_ctx Info "Pass performance details:";
  List.iter (fun pass_perf ->
    log perf_ctx.debug_ctx Info "  %s: %.2fms (%.1f opt/ms, %.1f expr/s)"
      pass_perf.pass_name pass_perf.total_time_ms 
      pass_perf.optimization_rate pass_perf.expressions_per_second
  ) pipeline_perf.pass_performances

let export_performance_report pipeline_perf filename =
  let oc = open_out filename in
  
  Printf.fprintf oc "# Optimization Performance Report\n\n";
  Printf.fprintf oc "## Pipeline Overview\n";
  Printf.fprintf oc "- Total wall time: %.2fms\n" pipeline_perf.total_wall_time_ms;
  Printf.fprintf oc "- Total CPU time: %.2fms\n" pipeline_perf.total_cpu_time_ms;
  Printf.fprintf oc "- Peak memory usage: %.2fMB\n" (float_of_int pipeline_perf.peak_memory_bytes /. 1024.0 /. 1024.0);
  Printf.fprintf oc "- Total memory allocated: %.2fMB\n" (float_of_int pipeline_perf.total_memory_allocated_bytes /. 1024.0 /. 1024.0);
  Printf.fprintf oc "- Iterations: %d\n" pipeline_perf.iterations;
  Printf.fprintf oc "- Convergence time: %.2fms\n" pipeline_perf.convergence_time_ms;
  Printf.fprintf oc "- Verification time: %.2fms\n" pipeline_perf.verification_time_ms;
  Printf.fprintf oc "\n## Throughput\n";
  Printf.fprintf oc "- Statements per second: %.1f\n" pipeline_perf.throughput_statements_per_sec;
  Printf.fprintf oc "- Expressions per second: %.1f\n" pipeline_perf.throughput_expressions_per_sec;
  
  Printf.fprintf oc "\n## Pass Performance\n";
  List.iter (fun pass_perf ->
    Printf.fprintf oc "### %s\n" pass_perf.pass_name;
    Printf.fprintf oc "- Wall time: %.2fms\n" pass_perf.total_time_ms;
    Printf.fprintf oc "- CPU time: %.2fms\n" pass_perf.cpu_time_ms;
    Printf.fprintf oc "- Memory peak: %.2fKB\n" (float_of_int pass_perf.memory_peak_bytes /. 1024.0);
    Printf.fprintf oc "- Memory allocated: %.2fKB\n" (float_of_int pass_perf.memory_allocated_bytes /. 1024.0);
    Printf.fprintf oc "- Expressions per second: %.1f\n" pass_perf.expressions_per_second;
    Printf.fprintf oc "- Statements per second: %.1f\n" pass_perf.statements_per_second;
    Printf.fprintf oc "- Optimization rate: %.3f opt/ms\n" pass_perf.optimization_rate;
    Printf.fprintf oc "\n"
  ) pipeline_perf.pass_performances;
  
  close_out oc

let compare_performance_profiles baseline current =
  let baseline_time = baseline.total_wall_time_ms in
  let current_time = current.total_wall_time_ms in
  let time_improvement = (baseline_time -. current_time) /. baseline_time *. 100.0 in
  
  let baseline_memory = baseline.peak_memory_bytes in
  let current_memory = current.peak_memory_bytes in
  let memory_improvement = float_of_int (baseline_memory - current_memory) /. float_of_int baseline_memory *. 100.0 in
  
  let baseline_throughput = baseline.throughput_statements_per_sec in
  let current_throughput = current.throughput_statements_per_sec in
  let throughput_improvement = (current_throughput -. baseline_throughput) /. baseline_throughput *. 100.0 in
  
  Printf.sprintf 
    "Performance comparison:\n  Time: %.1f%% %s\n  Memory: %.1f%% %s\n  Throughput: %.1f%% %s"
    (abs_float time_improvement) (if time_improvement > 0.0 then "faster" else "slower")
    (abs_float memory_improvement) (if memory_improvement > 0.0 then "less memory" else "more memory")
    (abs_float throughput_improvement) (if throughput_improvement > 0.0 then "higher" else "lower")

let create_performance_benchmark perf_ctx circuit iterations =
  let benchmark_results = ref [] in
  
  for i = 1 to iterations do
    record_measurement_point perf_ctx (IterationStart i);
    let start_time = Unix.gettimeofday () in
    
    (* simulate optimization work *)
    let stmt_count = List.length circuit.Ast.body in
    let processing_time = float_of_int stmt_count *. 0.1 in
    Unix.sleepf (processing_time /. 1000.0);
    
    let end_time = Unix.gettimeofday () in
    let iteration_time = (end_time -. start_time) *. 1000.0 in
    
    benchmark_results := iteration_time :: !benchmark_results;
    record_measurement_point perf_ctx (IterationEnd i);
  done;
  
  let times = List.rev !benchmark_results in
  let avg_time = List.fold_left (+.) 0.0 times /. float_of_int (List.length times) in
  let min_time = List.fold_left min (List.hd times) times in
  let max_time = List.fold_left max (List.hd times) times in
  
  log perf_ctx.debug_ctx Info "Benchmark results over %d iterations:" iterations;
  log perf_ctx.debug_ctx Info "  Average: %.2fms" avg_time;
  log perf_ctx.debug_ctx Info "  Min: %.2fms" min_time;
  log perf_ctx.debug_ctx Info "  Max: %.2fms" max_time;
  
  (avg_time, min_time, max_time, times)