open Error
open Debug
open Pass_interface

type pipeline_config = {
  optimization_level: optimization_level;
  max_iterations: int;
  convergence_threshold: int; (* minimum optimizations per iteration to continue *)
  enable_verification: bool;
  parallel_execution: bool;
  debug_output: bool;
}

type pipeline_result = {
  final_ast: Ast.circuit;
  total_statistics: pass_statistics;
  passes_executed: (string * pass_statistics) list;
  iterations: int;
  converged: bool;
  verification_passed: bool;
}

type pass_registry = {
  mutable registered_passes: (string, optimization_pass) Hashtbl.t;
  mutable enabled_passes: string list;
  mutable pass_order: string list;
}

let create_default_config level = {
  optimization_level = level;
  max_iterations = 10;
  convergence_threshold = 1;
  enable_verification = true;
  parallel_execution = false;
  debug_output = false;
}

let create_registry () = {
  registered_passes = Hashtbl.create 32;
  enabled_passes = [];
  pass_order = [];
}

let register_pass registry pass =
  Hashtbl.replace registry.registered_passes pass.name pass;
  log_level_priority Debug |> ignore

let get_pass registry name =
  try Some (Hashtbl.find registry.registered_passes name)
  with Not_found -> None

let enable_pass registry name =
  match get_pass registry name with
  | Some _ -> 
      if not (List.mem name registry.enabled_passes) then
        registry.enabled_passes <- name :: registry.enabled_passes
  | None -> () (* silently ignore unknown passes *)

let disable_pass registry name =
  registry.enabled_passes <- List.filter ((<>) name) registry.enabled_passes

let set_pass_order registry order =
  registry.pass_order <- order

let resolve_dependencies registry pass_names =
  let rec resolve_pass name visited =
    if List.mem name visited then
      Result.Error (Error.cyclic_dependency (name :: visited) ())
    else
      match get_pass registry name with
      | None -> Result.Error (Error.circuit_error ("Unknown pass: " ^ name) ())
      | Some pass ->
          let* deps = collect_results (List.map (resolve_pass visited) pass.dependencies) in
          Ok (List.flatten deps @ [name])
  in
  
  let* resolved_lists = collect_results (List.map (resolve_pass []) pass_names) in
  let flattened = List.flatten resolved_lists in
  let unique_ordered = List.fold_left (fun acc name ->
    if List.mem name acc then acc else acc @ [name]
  ) [] flattened in
  Ok unique_ordered

let check_conflicts registry pass_names =
  let conflicts = ref [] in
  List.iter (fun name ->
    match get_pass registry name with
    | Some pass ->
        List.iter (fun conflict ->
          if List.mem conflict pass_names then
            conflicts := (name, conflict) :: !conflicts
        ) pass.conflicts
    | None -> ()
  ) pass_names;
  !conflicts

let filter_enabled_passes registry config =
  List.filter (fun name ->
    match get_pass registry name with
    | Some pass -> 
        List.mem name registry.enabled_passes && 
        should_run_pass pass config.optimization_level
    | None -> false
  ) registry.pass_order

let execute_single_pass ctx pass circuit =
  let start_time = Unix.gettimeofday () in
  
  let* result = measure_pass_execution ctx pass circuit in
  
  if ctx.enable_verification && result.changed then (
    let* verification_ok = pass.verify_correctness ctx circuit result.ast in
    if not verification_ok then
      Error (circuit_error ("Optimization pass failed verification: " ^ pass.name) ())
    else
      Ok { result with verification_hash = Some (compute_ast_hash result.ast) }
  ) else
    Ok result

let execute_pass_iteration registry config ctx circuit iteration =
  let enabled_passes = filter_enabled_passes registry config in
  let* resolved_order = resolve_dependencies registry enabled_passes in
  
  let conflicts = check_conflicts registry resolved_order in
  if List.length conflicts > 0 then (
    let conflict_str = String.concat ", " (List.map (fun (a, b) -> a ^ "<->" ^ b) conflicts) in
    Error (circuit_error ("Pass conflicts detected: " ^ conflict_str) ())
  ) else (
    log ctx.debug_ctx Info "Iteration %d: executing %d passes" iteration (List.length resolved_order);
    
    let* (final_circuit, total_stats, executed_passes) = 
      List.fold_left (fun acc_result pass_name ->
        let* (current_circuit, acc_stats, acc_passes) = acc_result in
        match get_pass registry pass_name with
        | None -> acc_result (* should not happen due to earlier filtering *)
        | Some pass ->
            let* result = execute_single_pass ctx pass current_circuit in
            let combined_stats = combine_statistics acc_stats result.statistics in
            let pass_record = (pass_name, result.statistics) in
            Ok (result.ast, combined_stats, pass_record :: acc_passes)
      ) (Ok (circuit, empty_statistics, [])) resolved_order
    in
    
    let any_changes = List.exists (fun (_, stats) -> stats.optimizations_applied > 0) executed_passes in
    
    Ok (final_circuit, total_stats, List.rev executed_passes, any_changes)
  )

let run_optimization_pipeline registry config circuit debug_ctx =
  log debug_ctx Info "Starting optimization pipeline with level %s" 
    (optimization_level_to_string config.optimization_level);
  
  let ctx = create_pass_context debug_ctx config.optimization_level in
  let ctx = { ctx with 
    enable_verification = config.enable_verification;
    max_iterations = config.max_iterations 
  } in
  
  let rec iterate_until_convergence current_circuit iteration total_stats all_executed_passes =
    if iteration >= config.max_iterations then (
      log debug_ctx Info "Reached maximum iterations (%d)" config.max_iterations;
      Ok (current_circuit, total_stats, all_executed_passes, iteration, false, true)
    ) else (
      let* (new_circuit, iter_stats, iter_passes, changed) = 
        execute_pass_iteration registry config ctx current_circuit iteration in
      
      let combined_stats = combine_statistics total_stats iter_stats in
      let all_passes = all_executed_passes @ iter_passes in
      
      if not changed || iter_stats.optimizations_applied < config.convergence_threshold then (
        log debug_ctx Info "Optimization pipeline converged after %d iterations" iteration;
        Ok (new_circuit, combined_stats, all_passes, iteration, true, true)
      ) else (
        log debug_ctx Debug "Iteration %d applied %d optimizations, continuing" 
          iteration iter_stats.optimizations_applied;
        iterate_until_convergence new_circuit (iteration + 1) combined_stats all_passes
      )
    )
  in
  
  let start_time = Unix.gettimeofday () in
  let* (final_circuit, final_stats, all_passes, iterations, converged, verification_passed) = 
    iterate_until_convergence circuit 1 empty_statistics [] in
  let end_time = Unix.gettimeofday () in
  
  let pipeline_time = (end_time -. start_time) *. 1000.0 in
  let final_stats_with_time = { final_stats with execution_time_ms = pipeline_time } in
  
  log_optimization_summary ctx final_stats_with_time (List.length all_passes);
  
  Ok {
    final_ast = final_circuit;
    total_statistics = final_stats_with_time;
    passes_executed = all_passes;
    iterations;
    converged;
    verification_passed;
  }

let create_standard_pipeline registry level =
  match level with
  | O0 -> 
      (* no optimizations *)
      set_pass_order registry []
  | O1 ->
      (* basic optimizations *)
      set_pass_order registry ["constant_folding"; "dead_code_elimination"];
      enable_pass registry "constant_folding";
      enable_pass registry "dead_code_elimination"
  | O2 ->
      (* aggressive optimizations *)
      set_pass_order registry ["constant_folding"; "algebraic_simplification"; "dead_code_elimination"];
      enable_pass registry "constant_folding";
      enable_pass registry "algebraic_simplification";
      enable_pass registry "dead_code_elimination"
  | O3 ->
      (* maximum optimizations *)
      set_pass_order registry ["constant_folding"; "algebraic_simplification"; "dead_code_elimination"; "constant_folding"];
      enable_pass registry "constant_folding";
      enable_pass registry "algebraic_simplification";
      enable_pass registry "dead_code_elimination"

let optimize_circuit registry config circuit debug_ctx =
  start_phase debug_ctx Optimize;
  let result = run_optimization_pipeline registry config circuit debug_ctx in
  end_phase debug_ctx;
  result

let print_pipeline_summary result debug_ctx =
  log debug_ctx Info "=== Optimization Pipeline Summary ===";
  log debug_ctx Info "Final statistics: %s" (pass_statistics_to_string result.total_statistics);
  log debug_ctx Info "Iterations: %d" result.iterations;
  log debug_ctx Info "Converged: %b" result.converged;
  log debug_ctx Info "Verification passed: %b" result.verification_passed;
  
  if List.length result.passes_executed > 0 then (
    log debug_ctx Info "Pass execution details:";
    List.iter (fun (pass_name, stats) ->
      log debug_ctx Info "  %s: %s" pass_name (pass_statistics_to_string stats)
    ) result.passes_executed
  );
  
  log debug_ctx Info "Total optimization time: %.2fms" result.total_statistics.execution_time_ms

let get_optimization_metrics result = {
  Pass_interface.expressions_processed = result.total_statistics.expressions_processed;
  statements_processed = result.total_statistics.statements_processed;
  circuits_processed = result.total_statistics.circuits_processed;
  optimizations_applied = result.total_statistics.optimizations_applied;
  constants_folded = result.total_statistics.constants_folded;
  dead_code_eliminated = result.total_statistics.dead_code_eliminated;
  algebraic_simplifications = result.total_statistics.algebraic_simplifications;
  execution_time_ms = result.total_statistics.execution_time_ms;
}

let export_optimization_report result filename =
  let oc = open_out filename in
  Printf.fprintf oc "# Optimization Report\n\n";
  Printf.fprintf oc "## Summary\n";
  Printf.fprintf oc "- Iterations: %d\n" result.iterations;
  Printf.fprintf oc "- Converged: %b\n" result.converged;
  Printf.fprintf oc "- Verification: %b\n" result.verification_passed;
  Printf.fprintf oc "- Total time: %.2fms\n" result.total_statistics.execution_time_ms;
  Printf.fprintf oc "\n## Statistics\n";
  Printf.fprintf oc "- Optimizations applied: %d\n" result.total_statistics.optimizations_applied;
  Printf.fprintf oc "- Constants folded: %d\n" result.total_statistics.constants_folded;
  Printf.fprintf oc "- Dead code eliminated: %d\n" result.total_statistics.dead_code_eliminated;
  Printf.fprintf oc "- Algebraic simplifications: %d\n" result.total_statistics.algebraic_simplifications;
  Printf.fprintf oc "\n## Pass Details\n";
  List.iter (fun (name, stats) ->
    Printf.fprintf oc "### %s\n" name;
    Printf.fprintf oc "- Optimizations: %d\n" stats.optimizations_applied;
    Printf.fprintf oc "- Time: %.2fms\n" stats.execution_time_ms;
    Printf.fprintf oc "\n"
  ) result.passes_executed;
  close_out oc