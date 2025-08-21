open Error
open Debug

type optimization_level = 
  | O0  (* no optimizations *)
  | O1  (* basic optimizations *)
  | O2  (* aggressive optimizations *)
  | O3  (* maximum optimizations with potential code expansion *)

type pass_statistics = {
  expressions_processed: int;
  statements_processed: int;
  circuits_processed: int;
  optimizations_applied: int;
  constants_folded: int;
  dead_code_eliminated: int;
  algebraic_simplifications: int;
  execution_time_ms: float;
}

type pass_result = {
  ast: Ast.circuit;
  statistics: pass_statistics;
  changed: bool;
  verification_hash: string option;
}

type pass_context = {
  debug_ctx: debug_context;
  optimization_level: optimization_level;
  preserve_semantics: bool;
  enable_verification: bool;
  max_iterations: int;
  current_iteration: int;
}

type optimization_pass = {
  name: string;
  description: string;
  enabled_at_level: optimization_level -> bool;
  transform_circuit: pass_context -> Ast.circuit -> pass_result Error.result;
  verify_correctness: pass_context -> Ast.circuit -> Ast.circuit -> bool Error.result;
  dependencies: string list;
  conflicts: string list;
}

let empty_statistics = {
  expressions_processed = 0;
  statements_processed = 0;
  circuits_processed = 0;
  optimizations_applied = 0;
  constants_folded = 0;
  dead_code_eliminated = 0;
  algebraic_simplifications = 0;
  execution_time_ms = 0.0;
}

let combine_statistics stats1 stats2 = {
  expressions_processed = stats1.expressions_processed + stats2.expressions_processed;
  statements_processed = stats1.statements_processed + stats2.statements_processed;
  circuits_processed = stats1.circuits_processed + stats2.circuits_processed;
  optimizations_applied = stats1.optimizations_applied + stats2.optimizations_applied;
  constants_folded = stats1.constants_folded + stats2.constants_folded;
  dead_code_eliminated = stats1.dead_code_eliminated + stats2.dead_code_eliminated;
  algebraic_simplifications = stats1.algebraic_simplifications + stats2.algebraic_simplifications;
  execution_time_ms = stats1.execution_time_ms +. stats2.execution_time_ms;
}

let create_pass_context debug_ctx optimization_level = {
  debug_ctx;
  optimization_level;
  preserve_semantics = true;
  enable_verification = true;
  max_iterations = 10;
  current_iteration = 0;
}

let optimization_level_to_string = function
  | O0 -> "O0"
  | O1 -> "O1"
  | O2 -> "O2"
  | O3 -> "O3"

let pass_statistics_to_string stats =
  Printf.sprintf 
    "expressions=%d, statements=%d, circuits=%d, optimizations=%d, constants_folded=%d, dead_code=%d, algebraic=%d, time=%.2fms"
    stats.expressions_processed stats.statements_processed stats.circuits_processed 
    stats.optimizations_applied stats.constants_folded stats.dead_code_eliminated
    stats.algebraic_simplifications stats.execution_time_ms

let log_pass_start ctx pass =
  log ctx.debug_ctx Info "Starting optimization pass: %s" pass.name;
  log ctx.debug_ctx Debug "Pass description: %s" pass.description

let log_pass_end ctx pass result =
  log ctx.debug_ctx Info "Completed optimization pass: %s (changed=%b)" pass.name result.changed;
  log ctx.debug_ctx Debug "Pass statistics: %s" (pass_statistics_to_string result.statistics);
  if result.changed then
    log ctx.debug_ctx Info "Pass %s applied %d optimizations" pass.name result.statistics.optimizations_applied

let measure_pass_execution ctx pass circuit =
  log_pass_start ctx pass;
  let start_time = Unix.gettimeofday () in
  
  let* result = pass.transform_circuit ctx circuit in
  
  let end_time = Unix.gettimeofday () in
  let execution_time_ms = (end_time -. start_time) *. 1000.0 in
  
  let final_result = {
    result with 
    statistics = { result.statistics with execution_time_ms }
  } in
  
  log_pass_end ctx pass final_result;
  Ok final_result

let should_run_pass pass level =
  pass.enabled_at_level level

let compute_ast_hash circuit =
  let circuit_str = Ast.circuit_to_string circuit in
  let hash = Hashtbl.hash circuit_str in
  Printf.sprintf "%08x" hash

let verify_semantic_equivalence ctx original optimized =
  if not ctx.enable_verification then Ok true else
  try
    let original_hash = compute_ast_hash original in
    let optimized_hash = compute_ast_hash optimized in
    
    if original_hash = optimized_hash then (
      log ctx.debug_ctx Trace "AST hashes match - no changes detected";
      Ok true
    ) else (
      log ctx.debug_ctx Debug "AST hashes differ - verifying semantic equivalence";
      let original_vars = Ast.get_all_circuit_vars original in
      let optimized_vars = Ast.get_all_circuit_vars optimized in
      
      if List.length original_vars <> List.length optimized_vars then (
        log ctx.debug_ctx Warning "Variable count mismatch after optimization";
        Ok false
      ) else if original.name <> optimized.name then (
        log ctx.debug_ctx Warning "Circuit name changed during optimization";
        Ok false
      ) else if original.inputs <> optimized.inputs then (
        log ctx.debug_ctx Warning "Circuit inputs changed during optimization";
        Ok false
      ) else if original.private_inputs <> optimized.private_inputs then (
        log ctx.debug_ctx Warning "Circuit private inputs changed during optimization";
        Ok false
      ) else (
        log ctx.debug_ctx Info "Basic structure preserved - optimization appears valid";
        Ok true
      )
    )
  with
  | exn -> 
      log ctx.debug_ctx Error "Verification failed with exception: %s" (Printexc.to_string exn);
      Ok false

let create_basic_pass name description enabled_fn transform_fn = {
  name;
  description;
  enabled_at_level = enabled_fn;
  transform_circuit = transform_fn;
  verify_correctness = (fun ctx original optimized -> 
    verify_semantic_equivalence ctx original optimized);
  dependencies = [];
  conflicts = [];
}

let create_pass_with_deps name description enabled_fn transform_fn deps conflicts = {
  name;
  description;
  enabled_at_level = enabled_fn;
  transform_circuit = transform_fn;
  verify_correctness = (fun ctx original optimized -> 
    verify_semantic_equivalence ctx original optimized);
  dependencies = deps;
  conflicts = conflicts;
}

let increment_iteration ctx = {
  ctx with current_iteration = ctx.current_iteration + 1
}

let should_continue_iterating ctx =
  ctx.current_iteration < ctx.max_iterations

let log_optimization_summary ctx total_stats passes_run =
  log ctx.debug_ctx Info "Optimization summary:";
  log ctx.debug_ctx Info "  Passes executed: %d" passes_run;
  log ctx.debug_ctx Info "  Total optimizations: %d" total_stats.optimizations_applied;
  log ctx.debug_ctx Info "  Constants folded: %d" total_stats.constants_folded;
  log ctx.debug_ctx Info "  Dead code eliminated: %d" total_stats.dead_code_eliminated;
  log ctx.debug_ctx Info "  Algebraic simplifications: %d" total_stats.algebraic_simplifications;
  log ctx.debug_ctx Info "  Total execution time: %.2fms" total_stats.execution_time_ms