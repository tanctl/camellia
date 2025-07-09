open Error
open Debug

type compilation_context = {
  debug_ctx: debug_context;
  wire_manager: R1cs.wire_manager;
  r1cs: R1cs.r1cs_system;
  variable_bindings: (string, R1cs.wire_id) Hashtbl.t;
  scope_stack: (string, R1cs.wire_id) Hashtbl.t list;
}

type compile_result = {
  wire_id: R1cs.wire_id;
  constraints: R1cs.constraint_triple list;
}

type compiled_circuit = {
  r1cs_system: R1cs.r1cs_system;
  input_assignments: (string * R1cs.wire_id) list;
  private_assignments: (string * R1cs.wire_id) list;
  output_wire: R1cs.wire_id option;
  total_constraints: int;
}

let create_compilation_context debug_ctx =
  let wire_manager = R1cs.create_wire_manager () in
  let r1cs = R1cs.create_empty_r1cs () in
  let variable_bindings = Hashtbl.create 32 in
  let scope_stack = [variable_bindings] in
  {
    debug_ctx;
    wire_manager;
    r1cs;
    variable_bindings;
    scope_stack;
  }

let push_scope ctx =
  let new_scope = Hashtbl.copy ctx.variable_bindings in
  { ctx with 
    scope_stack = new_scope :: ctx.scope_stack;
    variable_bindings = new_scope }

let pop_scope ctx =
  match ctx.scope_stack with
  | [] -> ctx
  | [_] -> ctx
  | _::parent::rest -> 
      { ctx with 
        scope_stack = parent :: rest;
        variable_bindings = parent }

let allocate_wire ctx name wire_type ?value () =
  let* wire_id = R1cs.allocate_wire ctx.wire_manager name wire_type ?value () in
  let* wire_info = R1cs.get_wire ctx.wire_manager wire_id in
  let updated_r1cs = R1cs.add_wire_to_r1cs ctx.r1cs wire_info in
  Ok (wire_id, { ctx with r1cs = updated_r1cs })

let bind_variable ctx var_name wire_id =
  Hashtbl.replace ctx.variable_bindings var_name wire_id;
  log ctx.debug_ctx Trace "Bound variable %s to wire %d" var_name wire_id;
  Ok ctx

let lookup_variable ctx var_name =
  match Hashtbl.find_opt ctx.variable_bindings var_name with
  | Some wire_id -> Ok wire_id
  | None -> Result.Error (unbound_variable var_name ())

let add_constraint_to_context ctx constraint_triple =
  let updated_r1cs = R1cs.add_constraint_to_r1cs ctx.r1cs constraint_triple in
  { ctx with r1cs = updated_r1cs }

let rec compile_expr ctx expr =
  log ctx.debug_ctx Trace "Compiling expression: %s" (Ast.expr_to_string expr);
  match expr with
  | Ast.Var var_name -> compile_var ctx var_name
  | Ast.Const value -> compile_const ctx value
  | Ast.Add (e1, e2) -> compile_add ctx e1 e2
  | Ast.Mul (e1, e2) -> compile_mul ctx e1 e2
  | Ast.Equal (e1, e2) -> compile_equal ctx e1 e2
  | Ast.Poseidon exprs -> compile_poseidon ctx exprs

and compile_var ctx var_name =
  let* wire_id = lookup_variable ctx var_name in
  log ctx.debug_ctx Trace "Variable %s maps to wire %d" var_name wire_id;
  Ok ({ wire_id; constraints = [] }, ctx)

and compile_const ctx value =
  let constant_name = "const_" ^ value in
  
  (* reuse existing constants to reduce constraint count *)
  let existing_wire = 
    try
      let wire_id = Hashtbl.find ctx.wire_manager.name_to_id constant_name in
      Some wire_id
    with Not_found -> None
  in
  
  match existing_wire with
  | Some wire_id ->
      log ctx.debug_ctx Trace "Reusing existing constant %s as wire %d" value wire_id;
      Ok ({ wire_id; constraints = [] }, ctx)
  | None ->
      (* need unique names to avoid wire allocation conflicts *)
      let unique_name = Printf.sprintf "const_%s_%d" value ctx.wire_manager.next_id in
      let* (wire_id, new_ctx) = allocate_wire ctx unique_name R1cs.Constant () in
      
      let* field_value = Field.of_string value in
      let constraint_triple = R1cs.create_constant_constraint wire_id field_value in
      let final_ctx = add_constraint_to_context new_ctx constraint_triple in
      
      (* enable reuse for future references *)
      Hashtbl.replace final_ctx.wire_manager.name_to_id constant_name wire_id;
      
      log ctx.debug_ctx Trace "Allocated new constant %s as wire %d" value wire_id;
      Ok ({ wire_id; constraints = [constraint_triple] }, final_ctx)

and compile_add ctx e1 e2 =
  let* (result1, ctx1) = compile_expr ctx e1 in
  let* (result2, ctx2) = compile_expr ctx1 e2 in
  
  let wire_name = Printf.sprintf "add_%d_%d" result1.wire_id result2.wire_id in
  let* (sum_wire, ctx3) = allocate_wire ctx2 wire_name R1cs.Intermediate () in
  
  let addition_constraint = R1cs.create_addition_constraint result1.wire_id result2.wire_id sum_wire in
  let final_ctx = add_constraint_to_context ctx3 addition_constraint in
  
  let all_constraints = result1.constraints @ result2.constraints @ [addition_constraint] in
  log ctx.debug_ctx Trace "Generated addition: wire%d + wire%d = wire%d" 
    result1.wire_id result2.wire_id sum_wire;
  
  Ok ({ wire_id = sum_wire; constraints = all_constraints }, final_ctx)

and compile_mul ctx e1 e2 =
  let* (result1, ctx1) = compile_expr ctx e1 in
  let* (result2, ctx2) = compile_expr ctx1 e2 in
  
  let wire_name = Printf.sprintf "mul_%d_%d" result1.wire_id result2.wire_id in
  let* (product_wire, ctx3) = allocate_wire ctx2 wire_name R1cs.Intermediate () in
  
  let mul_constraint = R1cs.create_multiplication_constraint result1.wire_id result2.wire_id product_wire in
  let final_ctx = add_constraint_to_context ctx3 mul_constraint in
  
  let all_constraints = result1.constraints @ result2.constraints @ [mul_constraint] in
  log ctx.debug_ctx Trace "Generated multiplication: wire%d * wire%d = wire%d" 
    result1.wire_id result2.wire_id product_wire;
  
  Ok ({ wire_id = product_wire; constraints = all_constraints }, final_ctx)

and compile_equal ctx e1 e2 =
  let* (result1, ctx1) = compile_expr ctx e1 in
  let* (result2, ctx2) = compile_expr ctx1 e2 in
  
  let wire_name = Printf.sprintf "eq_%d_%d" result1.wire_id result2.wire_id in
  let* (eq_wire, ctx3) = allocate_wire ctx2 wire_name R1cs.Intermediate () in
  
  (* simplified equality check - full R1CS equality needs more constraints *)
  let* (zero_wire, ctx4) = allocate_wire ctx3 ("zero_" ^ string_of_int eq_wire) R1cs.Constant () in
  let* zero_field = Field.of_string Field.zero in
  let zero_constraint = R1cs.create_constant_constraint zero_wire zero_field in
  
  let* neg_one = Field.neg Field.one in
  let diff_lc = R1cs.add_term result1.wire_id Field.one 
    (R1cs.add_term result2.wire_id neg_one R1cs.empty_linear_combination) in
  let one_lc = R1cs.add_term 0 Field.one R1cs.empty_linear_combination in
  let zero_lc = R1cs.add_term zero_wire Field.one R1cs.empty_linear_combination in
  let equality_constraint = R1cs.create_constraint diff_lc one_lc zero_lc in
  
  let final_ctx = add_constraint_to_context 
    (add_constraint_to_context ctx4 zero_constraint) equality_constraint in
  
  let all_constraints = result1.constraints @ result2.constraints @ [zero_constraint; equality_constraint] in
  log ctx.debug_ctx Trace "Generated equality check: wire%d == wire%d (result in wire%d)" 
    result1.wire_id result2.wire_id eq_wire;
  
  Ok ({ wire_id = eq_wire; constraints = all_constraints }, final_ctx)

and compile_poseidon ctx exprs =
  let rec compile_inputs acc_results acc_ctx = function
    | [] -> Ok (List.rev acc_results, acc_ctx)
    | expr :: rest ->
        let* (result, new_ctx) = compile_expr acc_ctx expr in
        compile_inputs (result :: acc_results) new_ctx rest
  in
  let* (input_results, ctx_after_inputs) = compile_inputs [] ctx exprs in
  
  let input_wires = List.map (fun r -> r.wire_id) input_results in
  let wire_name = Printf.sprintf "poseidon_%s" 
    (String.concat "_" (List.map string_of_int input_wires)) in
  let* (hash_wire, ctx_with_output) = allocate_wire ctx_after_inputs wire_name R1cs.Intermediate () in
  
  (* placeholder - real implementation would expand full poseidon circuit *)
  let poseidon_constraint = 
    let input_lc = List.fold_left (fun lc wire_id -> 
      R1cs.add_term wire_id Field.one lc
    ) R1cs.empty_linear_combination input_wires in
    let one_lc = R1cs.add_term 0 Field.one R1cs.empty_linear_combination in
    let output_lc = R1cs.add_term hash_wire Field.one R1cs.empty_linear_combination in
    R1cs.create_constraint input_lc one_lc output_lc
  in
  
  let final_ctx = add_constraint_to_context ctx_with_output poseidon_constraint in
  
  let all_constraints = (List.fold_left (fun acc r -> acc @ r.constraints) [] input_results) @ [poseidon_constraint] in
  log ctx.debug_ctx Trace "Generated poseidon hash with %d inputs -> wire%d" 
    (List.length exprs) hash_wire;
  
  Ok ({ wire_id = hash_wire; constraints = all_constraints }, final_ctx)

let compile_stmt ctx stmt =
  log ctx.debug_ctx Trace "Compiling statement: %s" (Ast.stmt_to_string stmt);
  match stmt with
  | Ast.Constraint expr ->
      let* (result, ctx1) = compile_expr ctx expr in
      let one_field = Field.one in
      let* (one_wire, ctx2) = allocate_wire ctx1 "constraint_one" R1cs.Constant () in
      let one_constraint = R1cs.create_constant_constraint one_wire one_field in
      
      let assertion_constraint = 
        let expr_lc = R1cs.add_term result.wire_id Field.one R1cs.empty_linear_combination in
        let one_lc = R1cs.add_term 0 Field.one R1cs.empty_linear_combination in
        let target_lc = R1cs.add_term one_wire Field.one R1cs.empty_linear_combination in
        R1cs.create_constraint expr_lc one_lc target_lc
      in
      
      let final_ctx = add_constraint_to_context 
        (add_constraint_to_context ctx2 one_constraint) assertion_constraint in
      
      log ctx.debug_ctx Trace "Added constraint assertion for wire%d" result.wire_id;
      Ok final_ctx
      
  | Ast.Assign (var_name, expr) ->
      let* (result, ctx1) = compile_expr ctx expr in
      let* final_ctx = bind_variable ctx1 var_name result.wire_id in
      
      log ctx.debug_ctx Trace "Assigned variable %s = wire%d" var_name result.wire_id;
      Ok final_ctx

let setup_circuit_inputs ctx circuit =
  let rec setup_inputs acc_assignments acc_ctx = function
    | [] -> Ok (List.rev acc_assignments, acc_ctx)
    | input_name :: rest ->
        let* (wire_id, new_ctx) = allocate_wire acc_ctx input_name R1cs.Input () in
        let* bound_ctx = bind_variable new_ctx input_name wire_id in
        setup_inputs ((input_name, wire_id) :: acc_assignments) bound_ctx rest
  in
  
  let rec setup_private_inputs acc_assignments acc_ctx = function
    | [] -> Ok (List.rev acc_assignments, acc_ctx)
    | input_name :: rest ->
        let* (wire_id, new_ctx) = allocate_wire acc_ctx input_name R1cs.Private () in
        let* bound_ctx = bind_variable new_ctx input_name wire_id in
        setup_private_inputs ((input_name, wire_id) :: acc_assignments) bound_ctx rest
  in
  
  let* (input_assignments, ctx_with_inputs) = setup_inputs [] ctx circuit.Ast.inputs in
  let* (private_assignments, final_ctx) = setup_private_inputs [] ctx_with_inputs circuit.Ast.private_inputs in
  
  Ok (input_assignments, private_assignments, final_ctx)

let compile_circuit_body ctx circuit =
  List.fold_left (fun acc_result stmt ->
    let* acc_ctx = acc_result in
    compile_stmt acc_ctx stmt
  ) (Ok ctx) circuit.Ast.body

let compile_circuit debug_ctx circuit =
  log debug_ctx Info "Starting compilation of circuit: %s" circuit.Ast.name;
  
  let ctx = create_compilation_context debug_ctx in
  
  let* (input_assignments, private_assignments, ctx_with_inputs) = setup_circuit_inputs ctx circuit in
  
  log debug_ctx Info "Setup %d inputs and %d private inputs" 
    (List.length input_assignments) (List.length private_assignments);
  
  let* final_ctx = compile_circuit_body ctx_with_inputs circuit in
  
  let stats = R1cs.get_r1cs_statistics final_ctx.r1cs in
  
  log debug_ctx Info "Compilation complete:";
  log debug_ctx Info "  Variables: %d" stats.num_variables;
  log debug_ctx Info "  Constraints: %d" stats.num_constraints;
  log debug_ctx Info "  Inputs: %d" stats.num_inputs;
  log debug_ctx Info "  Private: %d" stats.num_private;
  
  R1cs.trace_r1cs_to_debug debug_ctx final_ctx.r1cs;
  
  Ok {
    r1cs_system = final_ctx.r1cs;
    input_assignments;
    private_assignments;
    output_wire = None;  (* could be enhanced to track main output *)
    total_constraints = stats.num_constraints;
  }

let get_compilation_stats compiled =
  R1cs.get_r1cs_statistics compiled.r1cs_system

let export_compiled_circuit compiled filename =
  R1cs.export_r1cs_json compiled.r1cs_system filename

let validate_compiled_circuit compiled witness =
  R1cs.validate_r1cs compiled.r1cs_system witness

let compiled_circuit_to_string compiled =
  let stats = get_compilation_stats compiled in
  Printf.sprintf "Compiled Circuit:\n  R1CS: %s\n  Inputs: %d, Private: %d, Total constraints: %d"
    (R1cs.r1cs_to_string compiled.r1cs_system)
    stats.num_inputs stats.num_private compiled.total_constraints