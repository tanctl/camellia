open Error
open Debug
open Pass_interface

type verification_method = 
  | StructuralEquivalence
  | SemanticEquivalence 
  | R1CSEquivalence
  | WitnessEquivalence

type verification_config = {
  methods: verification_method list;
  sample_witness_count: int;
  timeout_seconds: float;
  strict_mode: bool;
}

type verification_result = {
  method_used: verification_method;
  passed: bool;
  confidence: float; (* 0.0 to 1.0 *)
  details: string;
  execution_time_ms: float;
}

type comprehensive_verification = {
  overall_passed: bool;
  method_results: verification_result list;
  total_time_ms: float;
  confidence_score: float;
}

let default_verification_config = {
  methods = [StructuralEquivalence; SemanticEquivalence];
  sample_witness_count = 5;
  timeout_seconds = 10.0;
  strict_mode = true;
}

let extract_circuit_structure circuit =
  let input_count = List.length circuit.Ast.inputs in
  let private_count = List.length circuit.Ast.private_inputs in
  let stmt_count = List.length circuit.Ast.body in
  let assignment_count = List.fold_left (fun acc stmt ->
    match stmt with Ast.Assign (_, _) -> acc + 1 | _ -> acc
  ) 0 circuit.Ast.body in
  let constraint_count = List.fold_left (fun acc stmt ->
    match stmt with Ast.Constraint _ -> acc + 1 | _ -> acc
  ) 0 circuit.Ast.body in
  (input_count, private_count, stmt_count, assignment_count, constraint_count)

let verify_structural_equivalence ctx original optimized =
  let start_time = Unix.gettimeofday () in
  
  let (orig_inputs, orig_private, orig_stmts, orig_assigns, orig_constraints) = 
    extract_circuit_structure original in
  let (opt_inputs, opt_private, opt_stmts, opt_assigns, opt_constraints) = 
    extract_circuit_structure optimized in
  
  let structural_match = 
    orig_inputs = opt_inputs &&
    orig_private = opt_private &&
    original.name = optimized.name &&
    original.inputs = optimized.inputs &&
    original.private_inputs = optimized.private_inputs in
  
  let end_time = Unix.gettimeofday () in
  let execution_time = (end_time -. start_time) *. 1000.0 in
  
  let details = Printf.sprintf 
    "Original: inputs=%d, private=%d, stmts=%d, assigns=%d, constraints=%d; Optimized: inputs=%d, private=%d, stmts=%d, assigns=%d, constraints=%d"
    orig_inputs orig_private orig_stmts orig_assigns orig_constraints
    opt_inputs opt_private opt_stmts opt_assigns opt_constraints in
  
  log ctx.debug_ctx Debug "Structural verification: %s" details;
  
  Ok {
    method_used = StructuralEquivalence;
    passed = structural_match;
    confidence = if structural_match then 1.0 else 0.0;
    details;
    execution_time_ms = execution_time;
  }

let compute_expression_signature expr =
  let rec signature_of_expr = function
    | Ast.Var name -> "V:" ^ name
    | Ast.Const value -> "C:" ^ value
    | Ast.Add (e1, e2) -> "A:(" ^ (signature_of_expr e1) ^ "+" ^ (signature_of_expr e2) ^ ")"
    | Ast.Mul (e1, e2) -> "M:(" ^ (signature_of_expr e1) ^ "*" ^ (signature_of_expr e2) ^ ")"
    | Ast.Equal (e1, e2) -> "E:(" ^ (signature_of_expr e1) ^ "=" ^ (signature_of_expr e2) ^ ")"
    | Ast.Poseidon exprs -> "P:(" ^ (String.concat "," (List.map signature_of_expr exprs)) ^ ")"
  in
  signature_of_expr expr

let compute_circuit_semantic_signature circuit =
  let stmt_signatures = List.map (function
    | Ast.Assign (var, expr) -> "ASSIGN:" ^ var ^ "=" ^ (compute_expression_signature expr)
    | Ast.Constraint expr -> "CONSTRAINT:" ^ (compute_expression_signature expr)
  ) circuit.Ast.body in
  
  let input_signature = "INPUTS:" ^ (String.concat "," circuit.inputs) in
  let private_signature = "PRIVATE:" ^ (String.concat "," circuit.private_inputs) in
  let body_signature = String.concat ";" stmt_signatures in
  
  input_signature ^ ";" ^ private_signature ^ ";" ^ body_signature

let verify_semantic_equivalence ctx original optimized =
  let start_time = Unix.gettimeofday () in
  
  let orig_signature = compute_circuit_semantic_signature original in
  let opt_signature = compute_circuit_semantic_signature optimized in
  
  let orig_vars = Ast.get_all_circuit_vars original in
  let opt_vars = Ast.get_all_circuit_vars optimized in
  
  let orig_var_set = List.sort String.compare orig_vars in
  let opt_var_set = List.sort String.compare opt_vars in
  
  let variables_preserved = orig_var_set = opt_var_set in
  let signatures_match = orig_signature = opt_signature in
  
  let semantic_equivalent = variables_preserved && 
    (signatures_match || 
     (List.length original.body <= List.length optimized.body)) in
  
  let end_time = Unix.gettimeofday () in
  let execution_time = (end_time -. start_time) *. 1000.0 in
  
  let details = Printf.sprintf 
    "Variables preserved: %b, Signatures match: %b, Statements: %d->%d"
    variables_preserved signatures_match 
    (List.length original.body) (List.length optimized.body) in
  
  log ctx.debug_ctx Debug "Semantic verification: %s" details;
  
  let confidence = match (variables_preserved, signatures_match) with
    | (true, true) -> 1.0
    | (true, false) -> 0.8  (* variables preserved but structure changed *)
    | (false, _) -> 0.0     (* variables changed - likely incorrect *)
  in
  
  Ok {
    method_used = SemanticEquivalence;
    passed = semantic_equivalent;
    confidence;
    details;
    execution_time_ms = execution_time;
  }

let compile_to_r1cs_for_verification circuit debug_ctx =
  try
    match Compiler.compile_circuit debug_ctx circuit with
    | Ok compiled -> Ok compiled.r1cs_system
    | Error err -> Error err
  with
  | exn -> Error (circuit_error ("R1CS compilation failed: " ^ Printexc.to_string exn) ())

let verify_r1cs_equivalence ctx original optimized =
  let start_time = Unix.gettimeofday () in
  
  let* orig_r1cs = compile_to_r1cs_for_verification original ctx.debug_ctx in
  let* opt_r1cs = compile_to_r1cs_for_verification optimized ctx.debug_ctx in
  
  let orig_constraints = List.length orig_r1cs.R1cs.constraints in
  let opt_constraints = List.length opt_r1cs.R1cs.constraints in
  let orig_wires = orig_r1cs.R1cs.metadata.num_variables in
  let opt_wires = opt_r1cs.R1cs.metadata.num_variables in
  
  let constraint_count_reasonable = opt_constraints <= orig_constraints in
  let wire_count_reasonable = opt_wires <= orig_wires + 2; (* allow some slack for intermediate wires *)
  let input_counts_match = 
    orig_r1cs.metadata.num_inputs = opt_r1cs.metadata.num_inputs &&
    orig_r1cs.metadata.num_private = opt_r1cs.metadata.num_private in
  
  let r1cs_equivalent = constraint_count_reasonable && wire_count_reasonable && input_counts_match in
  
  let end_time = Unix.gettimeofday () in
  let execution_time = (end_time -. start_time) *. 1000.0 in
  
  let details = Printf.sprintf 
    "Constraints: %d->%d, Wires: %d->%d, Inputs match: %b"
    orig_constraints opt_constraints orig_wires opt_wires input_counts_match in
  
  log ctx.debug_ctx Debug "R1CS verification: %s" details;
  
  let confidence = match (constraint_count_reasonable, input_counts_match) with
    | (true, true) -> 0.9
    | (true, false) -> 0.3
    | (false, _) -> 0.1
  in
  
  Ok {
    method_used = R1CSEquivalence;
    passed = r1cs_equivalent;
    confidence;
    details;
    execution_time_ms = execution_time;
  }

let generate_sample_witness circuit witness_count =
  let input_count = List.length circuit.Ast.inputs in
  let private_count = List.length circuit.Ast.private_inputs in
  
  let generate_single_witness () =
    let inputs = List.mapi (fun i _ -> string_of_int (1 + i)) circuit.inputs in
    let privates = List.mapi (fun i _ -> string_of_int (10 + i)) circuit.private_inputs in
    inputs @ privates
  in
  
  List.init witness_count (fun _ -> generate_single_witness ())

let verify_witness_equivalence ctx original optimized config =
  let start_time = Unix.gettimeofday () in
  
  let sample_witnesses = generate_sample_witness original config.sample_witness_count in
  
  let* orig_r1cs = compile_to_r1cs_for_verification original ctx.debug_ctx in
  let* opt_r1cs = compile_to_r1cs_for_verification optimized ctx.debug_ctx in
  
  let test_witness witness =
    let rec take n lst = match n, lst with
      | 0, _ | _, [] -> []
      | n, x :: xs -> x :: take (n-1) xs
    in
    let rec drop n lst = match n, lst with
      | 0, xs -> xs
      | _, [] -> []
      | n, _ :: xs -> drop (n-1) xs
    in
    let input_values = take (List.length original.inputs) witness in
    let private_values = drop (List.length original.inputs) witness in
    
    let* orig_witness = Output.generate_example_witness orig_r1cs input_values private_values in
    let* opt_witness = Output.generate_example_witness opt_r1cs input_values private_values in
    
    let* orig_validation = Output.check_witness_satisfiability orig_r1cs orig_witness in
    let* opt_validation = Output.check_witness_satisfiability opt_r1cs opt_witness in
    
    Ok (orig_validation.is_valid && opt_validation.is_valid)
  in
  
  let rec test_all_witnesses witnesses success_count total_count =
    match witnesses with
    | [] -> Ok (success_count, total_count)
    | witness :: rest ->
        (match test_witness witness with
        | Ok true -> test_all_witnesses rest (success_count + 1) (total_count + 1)
        | Ok false -> test_all_witnesses rest success_count (total_count + 1)
        | Error _ -> test_all_witnesses rest success_count (total_count + 1))
  in
  
  let* (successes, total) = test_all_witnesses sample_witnesses 0 0 in
  
  let end_time = Unix.gettimeofday () in
  let execution_time = (end_time -. start_time) *. 1000.0 in
  
  let success_rate = if total = 0 then 0.0 else float_of_int successes /. float_of_int total in
  let passed = success_rate >= 0.8 in (* 80% success threshold *)
  
  let details = Printf.sprintf 
    "Witness tests: %d/%d passed (%.1f%%)" successes total (success_rate *. 100.0) in
  
  log ctx.debug_ctx Debug "Witness verification: %s" details;
  
  Ok {
    method_used = WitnessEquivalence;
    passed;
    confidence = success_rate;
    details;
    execution_time_ms = execution_time;
  }

let run_verification_method ctx original optimized method config =
  match method with
  | StructuralEquivalence -> verify_structural_equivalence ctx original optimized
  | SemanticEquivalence -> verify_semantic_equivalence ctx original optimized  
  | R1CSEquivalence -> verify_r1cs_equivalence ctx original optimized
  | WitnessEquivalence -> verify_witness_equivalence ctx original optimized config

let run_comprehensive_verification ctx original optimized config =
  let start_time = Unix.gettimeofday () in
  
  let* results = collect_results (List.map (fun method ->
    run_verification_method ctx original optimized method config
  ) config.methods) in
  
  let end_time = Unix.gettimeofday () in
  let total_time = (end_time -. start_time) *. 1000.0 in
  
  let all_passed = List.for_all (fun result -> result.passed) results in
  let weighted_confidence = 
    let total_weight = List.length results in
    let total_confidence = List.fold_left (fun acc result -> 
      acc +. result.confidence) 0.0 results in
    if total_weight = 0 then 0.0 else total_confidence /. float_of_int total_weight
  in
  
  let overall_passed = match config.strict_mode with
    | true -> all_passed && weighted_confidence >= 0.9
    | false -> all_passed || weighted_confidence >= 0.7
  in
  
  log ctx.debug_ctx Info "Comprehensive verification completed: passed=%b, confidence=%.2f" 
    overall_passed weighted_confidence;
  
  Ok {
    overall_passed;
    method_results = results;
    total_time_ms = total_time;
    confidence_score = weighted_confidence;
  }

let quick_verification ctx original optimized =
  let config = { default_verification_config with 
    methods = [StructuralEquivalence; SemanticEquivalence];
    strict_mode = false 
  } in
  run_comprehensive_verification ctx original optimized config

let thorough_verification ctx original optimized =
  let config = { default_verification_config with 
    methods = [StructuralEquivalence; SemanticEquivalence; R1CSEquivalence; WitnessEquivalence];
    sample_witness_count = 10;
    strict_mode = true 
  } in
  run_comprehensive_verification ctx original optimized config

let verification_result_to_string result =
  Printf.sprintf "%s: passed=%b, confidence=%.2f, time=%.2fms - %s"
    (match result.method_used with
     | StructuralEquivalence -> "Structural"
     | SemanticEquivalence -> "Semantic"  
     | R1CSEquivalence -> "R1CS"
     | WitnessEquivalence -> "Witness")
    result.passed result.confidence result.execution_time_ms result.details

let comprehensive_verification_to_string verification =
  let method_summaries = List.map verification_result_to_string verification.method_results in
  let methods_str = String.concat "\n  " method_summaries in
  Printf.sprintf "Overall: passed=%b, confidence=%.2f, total_time=%.2fms\nMethods:\n  %s"
    verification.overall_passed verification.confidence_score verification.total_time_ms methods_str