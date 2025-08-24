open Error
open Types
open Ast

type validation_context = {
  circuit_name: string;
  defined_circuits: string list;
  current_path: string list;
}

let empty_context = {
  circuit_name = "";
  defined_circuits = [];
  current_path = [];
}

let validate_circuit_name name pos =
  if String.length name = 0 then
    Error (Types.type_error "Circuit name cannot be empty" pos)
  else if String.get name 0 >= '0' && String.get name 0 <= '9' then
    Error (Types.type_error (Printf.sprintf "Circuit name '%s' cannot start with a digit" name) pos)
  else if String.contains name ' ' then
    Error (Types.type_error (Printf.sprintf "Circuit name '%s' contains invalid characters" name) pos)
  else
    Ok pos

let validate_input_names inputs pos =
  let rec check_duplicates seen = function
    | [] -> Ok ()
    | name :: rest ->
        if List.mem name seen then
          Error (Types.type_error (Printf.sprintf "Duplicate input name: '%s'" name) pos)
        else if String.length name = 0 then
          Error (Types.type_error "Input name cannot be empty" pos)
        else if String.get name 0 >= '0' && String.get name 0 <= '9' then
          Error (Types.type_error (Printf.sprintf "Input name '%s' cannot start with a digit" name) pos)
        else if String.contains name ' ' then
          Error (Types.type_error (Printf.sprintf "Input name '%s' contains invalid characters" name) pos)
        else
          check_duplicates (name :: seen) rest
  in
  check_duplicates [] inputs

let validate_private_input_names private_inputs pos =
  let rec check_duplicates seen = function
    | [] -> Ok ()
    | name :: rest ->
        if List.mem name seen then
          Error (Types.type_error 
            (Printf.sprintf "Duplicate private input name: '%s'" name) pos)
        else if String.length name = 0 then
          Error (Types.type_error "Private input name cannot be empty" pos)
        else if String.get name 0 >= '0' && String.get name 0 <= '9' then
          Error (Types.type_error 
            (Printf.sprintf "Private input name '%s' cannot start with a digit" name) pos)
        else
          check_duplicates (name :: seen) rest
  in
  check_duplicates [] private_inputs

let validate_no_input_overlap inputs private_inputs pos =
  let rec check_overlap = function
    | [] -> Ok ()
    | input :: rest ->
        if List.mem input private_inputs then
          Error (Types.type_error 
            (Printf.sprintf "Variable '%s' declared as both public and private input" input) pos)
        else
          check_overlap rest
  in
  check_overlap inputs

let validate_circuit_has_content circuit pos =
  match circuit.body with
  | [] -> 
      Error (Types.type_error "Circuit body cannot be empty" pos)
  | stmts ->
      (* check that circuit has at least one constraint *)
      let has_constraints = List.exists (function Constraint _ -> true | _ -> false) stmts in
      if not has_constraints then
        Error (Types.type_error "Circuit has no constraints" pos)
      else
        Ok pos

let validate_poseidon_arity exprs pos =
  let arity = List.length exprs in
  if arity = 0 then
    Error (arity_mismatch_error 1 0 pos)
  else if arity > 16 then
    Error (Types.type_error 
      (Printf.sprintf "Poseidon hash supports at most 16 inputs, got %d" arity) pos)
  else
    Ok pos

let rec validate_expr_constraints expr pos =
  let open Error in
  match expr with
  | Var _ | Const _ -> Ok pos
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Equal (e1, e2) ->
      let* _ = validate_expr_constraints e1 pos in
      let* _ = validate_expr_constraints e2 pos in
      Ok pos
  | Poseidon exprs ->
      let* _ = validate_poseidon_arity exprs pos in
      let* _ = collect_results (List.map (fun e -> validate_expr_constraints e pos) exprs) in
      Ok pos

let validate_statement stmt pos =
  match stmt with
  | Constraint expr -> validate_expr_constraints expr pos
  | Assign (var, expr) ->
      if String.length var = 0 then
        Error (Types.type_error "Assignment variable name cannot be empty" pos)
      else if String.get var 0 >= '0' && String.get var 0 <= '9' then
        Error (Types.type_error 
          (Printf.sprintf "Assignment variable '%s' cannot start with a digit" var) pos)
      else
        validate_expr_constraints expr pos

let validate_circuit_structure circuit pos =
  let open Error in
  let* _ = validate_circuit_name circuit.name pos in
  let* _ = validate_input_names circuit.inputs pos in
  let* _ = validate_private_input_names circuit.private_inputs pos in
  let* _ = validate_no_input_overlap circuit.inputs circuit.private_inputs pos in
  let* _ = validate_circuit_has_content circuit pos in
  let* _ = collect_results (List.map (fun stmt -> validate_statement stmt pos) circuit.body) in
  Ok pos

let validate_circuit_semantics circuit pos =
  let open Error in
  let* _ = validate_circuit_structure circuit pos in
  Ok pos

let validate_field_operations circuit pos =
  let rec validate_field_expr = function
    | Var _ | Const _ -> Ok pos
    | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) -> 
        let* _ = validate_field_expr e1 in
        let* _ = validate_field_expr e2 in
        Ok pos
    | Equal (e1, e2) ->
        let* _ = validate_field_expr e1 in
        let* _ = validate_field_expr e2 in
        Ok pos
    | Poseidon exprs ->
        let* _ = collect_results (List.map validate_field_expr exprs) in
        Ok pos
  in
  
  let validate_field_stmt = function
    | Constraint expr -> validate_field_expr expr
    | Assign (_, expr) -> validate_field_expr expr
  in
  
  collect_results (List.map validate_field_stmt circuit.body) |>
  Result.map (fun _ -> ())

(* validate circuit inputs are reasonable *)
let validate_circuit_inputs circuit pos =
  let total_inputs = List.length circuit.inputs + List.length circuit.private_inputs in
  if total_inputs = 0 then
    Error (Types.type_error "Circuit has no inputs" pos)
  else if total_inputs > 1000 then
    Error (Types.type_error "Too many circuit inputs" pos)
  else
    Ok pos

(* validate assignment dependency order *)
let validate_assignment_dependencies circuit pos =
  
  (* collect all assigned variables in order *)
  
  (* check each assignment doesn't use variables assigned later *)
  let rec check_dependencies assigned_so_far = function
    | [] -> Ok ()
    | Assign (var, expr) :: rest ->
        let used_vars = get_vars expr in
        let invalid_refs = List.filter (fun v ->
          not (List.mem v circuit.inputs) &&
          not (List.mem v circuit.private_inputs) &&
          not (List.mem v assigned_so_far)
        ) used_vars in
        (match invalid_refs with
         | [] -> check_dependencies (var :: assigned_so_far) rest
         | _ :: _ ->
             Error (Types.type_error "Forward reference in assignment" pos))
    | Constraint _ :: rest -> check_dependencies assigned_so_far rest
  in
  
  check_dependencies [] circuit.body

(* comprehensive circuit validation *)
let validate_complete_circuit circuit pos =
  let open Error in
  let* _ = validate_circuit_structure circuit pos in
  let* _ = validate_circuit_inputs circuit pos in
  let* _ = validate_assignment_dependencies circuit pos in
  let* _ = validate_field_operations circuit pos in
  Ok pos