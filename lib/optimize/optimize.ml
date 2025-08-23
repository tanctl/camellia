open Error
open Debug
open Pass_interface
open Pass_manager
(* verification module not implemented *)
open Performance

(* main optimization module that integrates all components *)

type optimization_config = {
  level: optimization_level;
  pipeline_config: pipeline_config;
  (* verification_config: verification_config; *)
  performance_tracking: bool;
  debug_optimization: bool;
}

type optimization_result = {
  optimized_circuit: Ast.circuit;
  pipeline_result: pipeline_result;
  (* verification_result: comprehensive_verification option; *)
  performance_data: pipeline_performance option;
  debug_info: string list;
}

let create_default_config level = {
  level;
  pipeline_config = Pass_manager.create_default_config level;
  (* verification_config = default_verification_config; *)
  performance_tracking = true;
  debug_optimization = false;
}

let create_debug_optimized_config level = {
  level;
  pipeline_config = { (Pass_manager.create_default_config level) with debug_output = true };
  (* verification_config = { default_verification_config with strict_mode = true }; *)
  performance_tracking = true;
  debug_optimization = true;
}

let setup_optimization_registry () =
  let registry = create_registry () in
  
  (* register basic optimization passes - implemented below *)
  register_pass registry (Constant_folding.create_pass ());
  register_pass registry (Dead_code_elimination.create_pass ());
  register_pass registry (Algebraic_simplification.create_pass ());
  
  registry

let integrate_with_debug_system debug_ctx config =
  log debug_ctx Info "Initializing optimization system";
  log debug_ctx Info "Optimization level: %s" (optimization_level_to_string config.level);
  log debug_ctx Debug "Pipeline config: max_iterations=%d, convergence_threshold=%d" 
    config.pipeline_config.max_iterations config.pipeline_config.convergence_threshold;
  (* log debug_ctx Debug "Verification enabled: %b, methods=%d" 
    config.verification_config.strict_mode (List.length config.verification_config.methods); *)
  log debug_ctx Debug "Performance tracking: %b" config.performance_tracking

let run_optimization_with_debug circuit config debug_ctx =
  integrate_with_debug_system debug_ctx config;
  
  let registry = setup_optimization_registry () in
  create_standard_pipeline registry config.level;
  
  let perf_ctx = if config.performance_tracking then 
    Some (create_performance_context debug_ctx true config.performance_tracking)
  else None in
  
  (* record optimization start *)
  (match perf_ctx with 
   | Some ctx -> record_measurement_point ctx PipelineStart 
   | None -> ());
  
  log debug_ctx Info "Starting optimization pipeline for circuit: %s" circuit.Ast.name;
  
  let* pipeline_result = optimize_circuit registry config.pipeline_config circuit debug_ctx in
  
  log debug_ctx Info "Optimization pipeline completed: %d iterations, converged=%b" 
    pipeline_result.iterations pipeline_result.converged;
  
  (* verification disabled - not implemented *)
  let _verification_result = None in
  
  (* analyze performance if tracking enabled *)
  let performance_data = match perf_ctx with
    | Some ctx -> 
        record_measurement_point ctx PipelineEnd;
        let perf_data = analyze_pipeline_performance ctx pipeline_result in
        log_performance_summary ctx perf_data;
        Some perf_data
    | None -> None
  in
  
  (* collect debug information *)
  let debug_info = [
    Printf.sprintf "Optimization level: %s" (optimization_level_to_string config.level);
    Printf.sprintf "Passes executed: %d" (List.length pipeline_result.passes_executed);
    Printf.sprintf "Total optimizations: %d" pipeline_result.total_statistics.optimizations_applied;
    Printf.sprintf "Execution time: %.2fms" pipeline_result.total_statistics.execution_time_ms;
  ] in
  
  (* log final summary *)
  log debug_ctx Info "=== Optimization Summary ===";
  List.iter (log debug_ctx Info "%s") debug_info;
  
  Ok {
    optimized_circuit = pipeline_result.final_ast;
    pipeline_result;
    (* verification_result; *)
    performance_data;
    debug_info;
  }

let optimize_circuit_simple circuit level debug_ctx =
  let config = create_default_config level in
  run_optimization_with_debug circuit config debug_ctx

let optimize_circuit_thorough circuit level debug_ctx =
  let config = create_debug_optimized_config level in
  (* let enhanced_config = { config with 
    verification_config = { config.verification_config with 
      methods = [StructuralEquivalence; SemanticEquivalence; R1CSEquivalence] 
    }
  } in *)
  run_optimization_with_debug circuit config debug_ctx

let export_optimization_results result base_filename =
  (* export pipeline results *)
  let pipeline_file = base_filename ^ "_pipeline.md" in
  export_optimization_report result.pipeline_result pipeline_file;
  
  (* verification export disabled (implement later) *)
  
  (* export performance results if available *)
  (match result.performance_data with
   | Some performance ->
       let performance_file = base_filename ^ "_performance.md" in
       export_performance_report performance performance_file
   | None -> ());
  
  (* export debug information *)
  let debug_file = base_filename ^ "_debug.txt" in
  let oc = open_out debug_file in
  Printf.fprintf oc "# Optimization Debug Information\n\n";
  List.iter (Printf.fprintf oc "%s\n") result.debug_info;
  close_out oc

let validate_optimization_correctness _original _optimized _debug_ctx =
  let _ctx = create_pass_context _debug_ctx O1 in
  Result.Error (Error.circuit_error "Quick verification not implemented" ())

let benchmark_optimization_performance circuit level iterations debug_ctx =
  let _perf_ctx = create_performance_context debug_ctx true true in
  let config = create_default_config level in
  
  log debug_ctx Info "Starting optimization benchmark: %d iterations" iterations;
  
  let results = ref [] in
  
  for i = 1 to iterations do
    log debug_ctx Debug "Benchmark iteration %d/%d" i iterations;
    
    match run_optimization_with_debug circuit config debug_ctx with
    | Ok result -> 
        results := result :: !results;
        log debug_ctx Trace "Iteration %d completed in %.2fms" 
          i result.pipeline_result.total_statistics.execution_time_ms
    | Error err ->
        log debug_ctx Warning "Iteration %d failed: %s" i (error_to_string err)
  done;
  
  let successful_results = !results in
  let times = List.map (fun r -> r.pipeline_result.total_statistics.execution_time_ms) successful_results in
  
  if List.length times > 0 then (
    let avg_time = List.fold_left (+.) 0.0 times /. float_of_int (List.length times) in
    let min_time = List.fold_left min (List.hd times) times in
    let max_time = List.fold_left max (List.hd times) times in
    
    log debug_ctx Info "Benchmark completed:";
    log debug_ctx Info "  Successful runs: %d/%d" (List.length successful_results) iterations;
    log debug_ctx Info "  Average time: %.2fms" avg_time;
    log debug_ctx Info "  Min time: %.2fms" min_time;
    log debug_ctx Info "  Max time: %.2fms" max_time;
    
    Ok (avg_time, min_time, max_time, successful_results)
  ) else (
    Error (circuit_error "All benchmark iterations failed" ())
  )