open Ast

let create_initial_env circuit =
  let env0 = Types.empty_env in
  let inputs = circuit.inputs in
  let private_inputs = circuit.private_inputs in
  let env1 = List.fold_left (fun env input -> Types.add_input input env) env0 inputs in
  let env2 = List.fold_left (fun env input -> Types.add_private_input input env) env1 private_inputs in
  env2

let type_check_circuit circuit pos =
  let open Error in
  (* comprehensive circuit structure validation *)
  let* _ = Circuit_validator.validate_complete_circuit circuit pos in
  let* _ = Scope_checker.validate_scoping circuit pos in
  
  let initial_env = create_initial_env circuit in
  
  let type_check_stmts env stmts =
    List.fold_left (fun acc_result stmt ->
      let* (env, stmts_acc) = acc_result in
      let* (new_env, typed_stmt) = Type_inference.type_stmt env stmt pos in
      Ok (new_env, typed_stmt :: stmts_acc)
    ) (Ok (env, [])) stmts
  in
  
  let* (final_env, _) = type_check_stmts initial_env circuit.body in
  (* enhanced validation checks *)
  let* _ = Type_inference.validate_variable_scoping final_env circuit pos in
  let* _ = Type_inference.validate_circuit_completeness circuit pos in
  let* _ = Type_inference.check_duplicate_assignments circuit pos in
  let* _ = Type_inference.check_input_declarations circuit pos in
  
  Ok final_env

let type_check_expression env expr pos =
  Type_inference.type_expr env expr pos

let validate_circuit_semantics circuit pos =
  match type_check_circuit circuit pos with
  | Ok _ -> Ok ()
  | Error err -> Error err

let get_expression_type env expr pos =
  match type_check_expression env expr pos with
  | Ok typed_expr -> Ok typed_expr.Types.type_
  | Error err -> Error err

let check_assignment_validity env _var expr pos =
  let open Error in
  let* typed_expr = type_check_expression env expr pos in
  match typed_expr.Types.type_ with
  | Types.FieldType -> Ok ()
  | t -> Error (type_mismatch "Field" (Types.type_to_string t) ~pos ())

let check_constraint_validity env expr pos =
  let open Error in
  let* typed_expr = type_check_expression env expr pos in
  match typed_expr.Types.type_ with
  | Types.FieldType -> Ok ()
  | t -> Error (type_mismatch "Field" (Types.type_to_string t) ~pos ())

let get_available_variables env =
  List.map fst env.Types.vars

let validate_variable_reference env var pos =
  match Types.find_var var env with
  | Some type_ -> Ok type_
  | None -> 
      let available = get_available_variables env in
      Error (Semantic_error.convert_to_error_system 
        (Semantic_error.enhanced_undeclared_variable_error var available pos ()))

let type_check_with_context circuit =
  let pos = Error.dummy_pos in
  type_check_circuit circuit pos