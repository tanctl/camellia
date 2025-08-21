open Alcotest

(* Test helper functions *)
let create_test_circuit name inputs private_inputs body = {
  Ast.name;
  inputs;
  private_inputs;
  body;
}

let create_debug_ctx name = 
  Debug.create_context ~level:Debug.Error name

let test_constant_folding () =
  let circuit = create_test_circuit "test_folding" ["x"] [] [
    Ast.Assign ("a", Ast.Const "5");
    Ast.Assign ("b", Ast.Const "3");
    Ast.Assign ("c", Ast.Add (Ast.Var "a", Ast.Var "b"));
    Ast.Assign ("d", Ast.Mul (Ast.Const "2", Ast.Const "4"));
    Ast.Constraint (Ast.Equal (Ast.Var "c", Ast.Const "8"));
  ] in
  
  let debug_ctx = create_debug_ctx "constant_folding_test" in
  let pass_ctx = Pass_interface.create_pass_context debug_ctx Pass_interface.O1 in
  
  match Constant_folding.apply_constant_folding pass_ctx circuit with
  | Ok result ->
      check bool "constant folding should make changes" true result.changed;
      check int "should fold some constants" 3 result.statistics.constants_folded;
      (* verify that constants were actually folded *)
      let has_folded_addition = List.exists (function
        | Ast.Assign ("c", Ast.Const "8") -> true
        | _ -> false
      ) result.ast.body in
      check bool "should fold a + b = 8" true has_folded_addition
  | Error err ->
      fail ("Constant folding failed: " ^ (Error.error_to_string err))

let test_dead_code_elimination () =
  let circuit = create_test_circuit "test_dce" ["x"] [] [
    Ast.Assign ("unused1", Ast.Const "5");
    Ast.Assign ("used", Ast.Var "x");
    Ast.Assign ("unused2", Ast.Add (Ast.Const "1", Ast.Const "2"));
    Ast.Constraint (Ast.Equal (Ast.Var "used", Ast.Const "10"));
  ] in
  
  let debug_ctx = create_debug_ctx "dce_test" in
  let pass_ctx = Pass_interface.create_pass_context debug_ctx Pass_interface.O1 in
  
  match Dead_code_elimination.apply_dead_code_elimination pass_ctx circuit with
  | Ok result ->
      check bool "DCE should make changes" true result.changed;
      check int "should eliminate dead code" 2 result.statistics.dead_code_eliminated;
      (* verify unused assignments were removed *)
      let remaining_assignments = List.filter (function
        | Ast.Assign _ -> true
        | _ -> false
      ) result.ast.body in
      check int "should have only 1 assignment left" 1 (List.length remaining_assignments)
  | Error err ->
      fail ("Dead code elimination failed: " ^ (Error.error_to_string err))

let test_algebraic_simplification () =
  let circuit = create_test_circuit "test_algebra" ["x"] [] [
    Ast.Assign ("a", Ast.Add (Ast.Var "x", Ast.Const "0"));
    Ast.Assign ("b", Ast.Mul (Ast.Var "x", Ast.Const "1"));
    Ast.Assign ("c", Ast.Mul (Ast.Const "0", Ast.Var "x"));
    Ast.Assign ("d", Ast.Equal (Ast.Var "x", Ast.Var "x"));
    Ast.Constraint (Ast.Equal (Ast.Var "d", Ast.Const "1"));
  ] in
  
  let debug_ctx = create_debug_ctx "algebra_test" in
  let pass_ctx = Pass_interface.create_pass_context debug_ctx Pass_interface.O2 in
  
  match Algebraic_simplification.apply_algebraic_simplification pass_ctx circuit with
  | Ok result ->
      check bool "algebra should make changes" true result.changed;
      check (int) "should apply simplifications" (fun x -> x > 0) result.statistics.algebraic_simplifications;
      (* verify some specific simplifications *)
      let has_simplified_add = List.exists (function
        | Ast.Assign ("a", Ast.Var "x") -> true
        | _ -> false
      ) result.ast.body in
      check bool "x + 0 should simplify to x" true has_simplified_add
  | Error err ->
      fail ("Algebraic simplification failed: " ^ (Error.error_to_string err))

let test_pass_manager_pipeline () =
  let circuit = create_test_circuit "test_pipeline" ["x"; "y"] [] [
    Ast.Assign ("a", Ast.Const "5");
    Ast.Assign ("b", Ast.Add (Ast.Var "a", Ast.Const "0"));
    Ast.Assign ("unused", Ast.Const "42");
    Ast.Assign ("c", Ast.Mul (Ast.Var "b", Ast.Const "1"));
    Ast.Constraint (Ast.Equal (Ast.Var "c", Ast.Const "5"));
  ] in
  
  let debug_ctx = create_debug_ctx "pipeline_test" in
  let registry = Pass_manager.create_registry () in
  
  (* register passes *)
  Pass_manager.register_pass registry (Constant_folding.create_pass ());
  Pass_manager.register_pass registry (Dead_code_elimination.create_pass ());
  Pass_manager.register_pass registry (Algebraic_simplification.create_pass ());
  
  Pass_manager.create_standard_pipeline registry Pass_interface.O2;
  
  let config = Pass_manager.create_default_config Pass_interface.O2 in
  
  match Pass_manager.run_optimization_pipeline registry config circuit debug_ctx with
  | Ok result ->
      check bool "pipeline should converge" true result.converged;
      check bool "pipeline should make changes" true (result.total_statistics.optimizations_applied > 0);
      check bool "should apply multiple types of optimizations" true 
        (result.total_statistics.constants_folded > 0 && result.total_statistics.dead_code_eliminated > 0);
      check int "should execute multiple passes" (fun x -> x >= 2) (List.length result.passes_executed)
  | Error err ->
      fail ("Pipeline execution failed: " ^ (Error.error_to_string err))

let test_optimization_verification () =
  let original = create_test_circuit "original" ["x"] [] [
    Ast.Assign ("a", Ast.Add (Ast.Var "x", Ast.Const "0"));
    Ast.Constraint (Ast.Equal (Ast.Var "a", Ast.Var "x"));
  ] in
  
  let optimized = create_test_circuit "optimized" ["x"] [] [
    Ast.Assign ("a", Ast.Var "x");
    Ast.Constraint (Ast.Equal (Ast.Var "a", Ast.Var "x"));
  ] in
  
  let debug_ctx = create_debug_ctx "verification_test" in
  let ctx = Pass_interface.create_pass_context debug_ctx Pass_interface.O1 in
  
  match Verification.quick_verification ctx original optimized with
  | Ok result ->
      check bool "verification should pass" true result.overall_passed;
      check (float 0.01) "confidence should be high" 0.8 result.confidence_score;
      check int "should run multiple verification methods" (fun x -> x >= 2) (List.length result.method_results)
  | Error err ->
      fail ("Verification failed: " ^ (Error.error_to_string err))

let test_semantic_preservation () =
  (* test that optimizations preserve circuit semantics *)
  let circuit = create_test_circuit "semantic_test" ["x"; "y"] [] [
    Ast.Assign ("a", Ast.Add (Ast.Var "x", Ast.Var "y"));
    Ast.Assign ("b", Ast.Mul (Ast.Var "a", Ast.Const "2"));
    Ast.Assign ("unused", Ast.Const "999");
    Ast.Constraint (Ast.Equal (Ast.Var "b", Ast.Const "10"));
  ] in
  
  let debug_ctx = create_debug_ctx "semantic_test" in
  
  match Optimize.optimize_circuit_simple circuit Pass_interface.O2 debug_ctx with
  | Ok result ->
      (* verify that essential structure is preserved *)
      check string "circuit name preserved" circuit.name result.optimized_circuit.name;
      check (list string) "inputs preserved" circuit.inputs result.optimized_circuit.inputs;
      check (list string) "private inputs preserved" circuit.private_inputs result.optimized_circuit.private_inputs;
      
      (* verify constraints are preserved *)
      let original_constraints = List.filter (function Ast.Constraint _ -> true | _ -> false) circuit.body in
      let optimized_constraints = List.filter (function Ast.Constraint _ -> true | _ -> false) result.optimized_circuit.body in
      check int "constraints preserved" (List.length original_constraints) (List.length optimized_constraints);
      
      (* verify verification passed *)
      (match result.verification_result with
       | Some verification -> check bool "semantic verification passed" true verification.overall_passed
       | None -> fail "No verification result available")
  | Error err ->
      fail ("Semantic preservation test failed: " ^ (Error.error_to_string err))

let test_performance_measurement () =
  let circuit = create_test_circuit "perf_test" ["x"] [] [
    Ast.Assign ("a", Ast.Const "1");
    Ast.Assign ("b", Ast.Const "2");
    Ast.Assign ("c", Ast.Add (Ast.Var "a", Ast.Var "b"));
    Ast.Constraint (Ast.Equal (Ast.Var "c", Ast.Const "3"));
  ] in
  
  let debug_ctx = create_debug_ctx "perf_test" in
  
  match Optimize.optimize_circuit_simple circuit Pass_interface.O1 debug_ctx with
  | Ok result ->
      (match result.performance_data with
       | Some perf ->
           check (float 0.1) "execution time should be measured" 0.0 perf.total_wall_time_ms;
           check int "should track pass performances" (fun x -> x > 0) (List.length perf.pass_performances);
           check (float 0.1) "throughput should be calculated" 0.0 perf.throughput_statements_per_sec
       | None -> fail "No performance data available")
  | Error err ->
      fail ("Performance measurement test failed: " ^ (Error.error_to_string err))

let test_optimization_levels () =
  let circuit = create_test_circuit "level_test" ["x"] [] [
    Ast.Assign ("a", Ast.Add (Ast.Var "x", Ast.Const "0"));
    Ast.Assign ("b", Ast.Mul (Ast.Var "a", Ast.Const "1"));
    Ast.Assign ("unused", Ast.Const "42");
    Ast.Constraint (Ast.Equal (Ast.Var "b", Ast.Var "x"));
  ] in
  
  let debug_ctx = create_debug_ctx "level_test" in
  
  (* test O0 (no optimizations) *)
  (match Optimize.optimize_circuit_simple circuit Pass_interface.O0 debug_ctx with
   | Ok result ->
       check int "O0 should apply no optimizations" 0 result.pipeline_result.total_statistics.optimizations_applied
   | Error err ->
       fail ("O0 test failed: " ^ (Error.error_to_string err)));
  
  (* test O1 (basic optimizations) *)
  (match Optimize.optimize_circuit_simple circuit Pass_interface.O1 debug_ctx with
   | Ok result ->
       check bool "O1 should apply some optimizations" true (result.pipeline_result.total_statistics.optimizations_applied > 0)
   | Error err ->
       fail ("O1 test failed: " ^ (Error.error_to_string err)));
  
  (* test O2 (aggressive optimizations) *)
  (match Optimize.optimize_circuit_simple circuit Pass_interface.O2 debug_ctx with
   | Ok result ->
       check bool "O2 should apply more optimizations" true (result.pipeline_result.total_statistics.optimizations_applied > 0)
   | Error err ->
       fail ("O2 test failed: " ^ (Error.error_to_string err)))

let test_complex_circuit_optimization () =
  (* test optimization on a more complex circuit *)
  let circuit = create_test_circuit "complex" ["a"; "b"; "c"] ["secret"] [
    Ast.Assign ("x1", Ast.Add (Ast.Var "a", Ast.Const "0"));
    Ast.Assign ("x2", Ast.Mul (Ast.Var "b", Ast.Const "1"));
    Ast.Assign ("x3", Ast.Add (Ast.Var "x1", Ast.Var "x2"));
    Ast.Assign ("x4", Ast.Mul (Ast.Var "x3", Ast.Const "2"));
    Ast.Assign ("temp1", Ast.Const "5");
    Ast.Assign ("temp2", Ast.Const "10");
    Ast.Assign ("sum_temp", Ast.Add (Ast.Var "temp1", Ast.Var "temp2"));
    Ast.Assign ("unused1", Ast.Const "999");
    Ast.Assign ("unused2", Ast.Mul (Ast.Const "0", Ast.Var "c"));
    Ast.Assign ("hash_input", Ast.Add (Ast.Var "x4", Ast.Var "secret"));
    Ast.Assign ("hash_result", Ast.Poseidon [Ast.Var "hash_input"]);
    Ast.Constraint (Ast.Equal (Ast.Var "sum_temp", Ast.Const "15"));
    Ast.Constraint (Ast.Equal (Ast.Var "hash_result", Ast.Const "12345"));
  ] in
  
  let debug_ctx = create_debug_ctx "complex_test" in
  
  match Optimize.optimize_circuit_thorough circuit Pass_interface.O3 debug_ctx with
  | Ok result ->
      let original_stmt_count = List.length circuit.body in
      let optimized_stmt_count = List.length result.optimized_circuit.body in
      
      check bool "should reduce statement count" true (optimized_stmt_count < original_stmt_count);
      check bool "should apply multiple optimization types" true 
        (result.pipeline_result.total_statistics.constants_folded > 0 &&
         result.pipeline_result.total_statistics.dead_code_eliminated > 0 &&
         result.pipeline_result.total_statistics.algebraic_simplifications > 0);
      
      (* verify verification passed *)
      (match result.verification_result with
       | Some verification -> 
           check bool "complex circuit verification passed" true verification.overall_passed;
           check (float 0.01) "verification confidence high" 0.7 verification.confidence_score
       | None -> fail "No verification result for complex circuit")
  | Error err ->
      fail ("Complex circuit optimization failed: " ^ (Error.error_to_string err))

let optimization_tests = [
  "constant_folding", `Quick, test_constant_folding;
  "dead_code_elimination", `Quick, test_dead_code_elimination;
  "algebraic_simplification", `Quick, test_algebraic_simplification;
  "pass_manager_pipeline", `Quick, test_pass_manager_pipeline;
  "optimization_verification", `Quick, test_optimization_verification;
  "semantic_preservation", `Quick, test_semantic_preservation;
  "performance_measurement", `Quick, test_performance_measurement;
  "optimization_levels", `Quick, test_optimization_levels;
  "complex_circuit_optimization", `Slow, test_complex_circuit_optimization;
]

let () =
  run "Optimization Tests" [
    "optimization", optimization_tests;
  ]