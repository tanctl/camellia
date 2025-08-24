open Error
open Types
open Ast

let rec type_expr env expr pos =
  let open Error in
  match expr with
  | Var name ->
      (match find_var name env with
       | Some type_ -> Ok { expr; type_; pos }
       | None -> 
           Error (Types.unbound_variable_error name pos))
  
  | Const value ->
      (* validate field constant format - for now just check it's numeric *)
      (try
         let _ = int_of_string value in
         Ok { expr; type_ = FieldType; pos }
       with Failure _ ->
         Error (Types.type_error (Printf.sprintf "Invalid field constant '%s': must be numeric" value) pos))
  
  | Add (e1, e2) ->
      let* typed_e1 = type_expr env e1 pos in
      let* typed_e2 = type_expr env e2 pos in
      (match typed_e1.type_, typed_e2.type_ with
       | FieldType, FieldType -> Ok { expr; type_ = FieldType; pos }
       | t1, t2 -> 
           Error (Types.type_mismatch_error "Field + Field" 
                   (Printf.sprintf "%s + %s" (type_to_string t1) (type_to_string t2)) 
                   pos))
  
  | Sub (e1, e2) ->
      let* typed_e1 = type_expr env e1 pos in
      let* typed_e2 = type_expr env e2 pos in
      (match typed_e1.type_, typed_e2.type_ with
       | FieldType, FieldType -> Ok { expr; type_ = FieldType; pos }
       | t1, t2 -> 
           Error (Types.type_mismatch_error "Field - Field" 
                   (Printf.sprintf "%s - %s" (type_to_string t1) (type_to_string t2)) 
                   pos))
  
  | Mul (e1, e2) ->
      let* typed_e1 = type_expr env e1 pos in
      let* typed_e2 = type_expr env e2 pos in
      (match typed_e1.type_, typed_e2.type_ with
       | FieldType, FieldType -> Ok { expr; type_ = FieldType; pos }
       | t1, t2 -> 
           Error (Types.type_mismatch_error "Field * Field" 
                   (Printf.sprintf "%s * %s" (type_to_string t1) (type_to_string t2)) 
                   pos))
  
  | Equal (e1, e2) ->
      let* typed_e1 = type_expr env e1 pos in
      let* typed_e2 = type_expr env e2 pos in
      (match typed_e1.type_, typed_e2.type_ with
       | FieldType, FieldType -> Ok { expr; type_ = FieldType; pos }
       | t1, t2 -> 
           Error (Types.type_mismatch_error "Field == Field" 
                   (Printf.sprintf "%s == %s" (type_to_string t1) (type_to_string t2)) 
                   pos))
  
  | Poseidon exprs ->
      (* validate poseidon arity *)
      let expr_count = List.length exprs in
      if expr_count = 0 then
        Error (Types.arity_mismatch_error 1 0 pos)
      else if expr_count > 16 then
        Error (Types.arity_mismatch_error 16 expr_count pos)
      else
        let check_expr _ e =
          let* typed_e = type_expr env e pos in
          match typed_e.type_ with
          | FieldType -> Ok typed_e
          | t -> 
              Error (Types.type_mismatch_error "Field" (type_to_string t) pos)
        in
        let* _typed_exprs = collect_results (List.mapi check_expr exprs) in
        Ok { expr; type_ = FieldType; pos }

let type_stmt env stmt pos =
  let open Error in
  match stmt with
  | Constraint expr ->
      let* typed_expr = type_expr env expr pos in
      (match typed_expr.type_ with
       | FieldType -> Ok (env, stmt)
       | t -> 
           Error (Types.type_mismatch_error "Field" (type_to_string t) pos))
  
  | Assign (var, expr) ->
      (* check if variable already assigned *)
      if is_assigned var env then
        Error (Types.type_error "Duplicate assignment" pos)
      else if is_input var env then
        Error (Types.type_error "Invalid assignment to input" pos)
      else if is_private_input var env then
        Error (Types.type_error "Invalid assignment to private input" pos)
      else
        let* typed_expr = type_expr env expr pos in
        (match typed_expr.type_ with
         | FieldType -> 
             let new_env = add_assigned_var var env in
             Ok (new_env, stmt)
         | t -> 
             Error (Types.type_mismatch_error "Field" (type_to_string t) pos))

let check_variable_usage env circuit pos =
  let used_vars = List.fold_left (fun acc stmt ->
    match stmt with
    | Assign (_, expr) -> (get_vars expr) @ acc
    | Constraint expr -> (get_vars expr) @ acc
  ) [] circuit.body in
  
  let unique_used_vars = List.sort_uniq String.compare used_vars in
  
  let check_var var =
    match find_var var env with
    | Some _ -> Ok pos
    | None -> 
        Error (Types.unbound_variable_error var pos)
  in
  
  collect_results (List.map check_var unique_used_vars) |> 
  Result.map (fun _ -> ())

let check_duplicate_assignments circuit pos =
  let rec check_duplicates seen = function
    | [] -> Ok ()
    | Assign (var, _) :: rest ->
        if List.mem var seen then
          Error (Types.type_error "Duplicate variable assignment" pos)
        else
          check_duplicates (var :: seen) rest
    | Constraint _ :: rest -> check_duplicates seen rest
  in
  check_duplicates [] circuit.body

let check_input_declarations circuit pos =
  let all_inputs = circuit.inputs @ circuit.private_inputs in
  let rec check_no_duplicates = function
    | [] -> Ok ()
    | x :: xs ->
        if List.mem x xs then
          Error (Types.type_error "Duplicate input declaration" pos)
        else
          check_no_duplicates xs
  in
  let* () = check_no_duplicates all_inputs in
  (* check for overlap between public and private inputs *)
  let rec check_overlap public_inputs private_inputs =
    match public_inputs with
    | [] -> Ok ()
    | var :: rest ->
        if List.mem var private_inputs then
          Error (Types.type_error "Input declaration conflict" pos)
        else
          check_overlap rest private_inputs
  in
  check_overlap circuit.inputs circuit.private_inputs

(* add comprehensive validation for variable scoping *)
let validate_variable_scoping env circuit pos =
  let open Error in
  
  (* check that all assignments reference valid variables *)
  let rec check_assignments = function
    | [] -> Ok ()
    | Assign (_, expr) :: rest ->
        (* check expression variables are in scope *)
        let expr_vars = get_vars expr in
        let* _ = collect_results (List.map (fun v ->
          match find_var v env with
          | Some _ -> Ok ()
          | None ->
              Error (Types.unbound_variable_error v pos)
        ) expr_vars) in
        check_assignments rest
    | Constraint expr :: rest ->
        (* check constraint variables are in scope *)
        let expr_vars = get_vars expr in
        let* _ = collect_results (List.map (fun v ->
          match find_var v env with
          | Some _ -> Ok ()
          | None ->
              Error (Types.unbound_variable_error v pos)
        ) expr_vars) in
        check_assignments rest
  in
  check_assignments circuit.body

(* validate that circuit has meaningful constraints *)
let validate_circuit_completeness circuit pos =
  
  if List.length circuit.body = 0 then
    Error (Types.type_error "Empty circuit body" pos)
  else
    let has_constraints = List.exists (function Constraint _ -> true | _ -> false) circuit.body in
    if not has_constraints then
      Error (Types.type_error "Circuit has no constraints" pos)
    else
      Ok pos