open Error
open Ast

type scope_info = {
  defined_vars: string list;
  current_scope: string list;
  dependencies: (string * string list) list;
}

let empty_scope = {
  defined_vars = [];
  current_scope = [];
  dependencies = [];
}

let add_definition var scope =
  { scope with defined_vars = var :: scope.defined_vars }

let add_dependency var deps scope =
  { scope with dependencies = (var, deps) :: scope.dependencies }

let enter_scope var scope =
  { scope with current_scope = var :: scope.current_scope }

let exit_scope scope =
  match scope.current_scope with
  | [] -> scope
  | _ :: rest -> { scope with current_scope = rest }

let rec collect_expr_vars = function
  | Var name -> [name]
  | Const _ -> []
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Equal (e1, e2) ->
      (collect_expr_vars e1) @ (collect_expr_vars e2)
  | Poseidon exprs ->
      List.fold_left (fun acc e -> acc @ (collect_expr_vars e)) [] exprs

let check_variable_before_definition circuit pos =
  let rec check_vars defined_vars remaining_stmts =
    match remaining_stmts with
    | [] -> Ok ()
    | stmt :: rest ->
        match stmt with
        | Assign (var, expr) ->
            let used_vars = collect_expr_vars expr in
            let undefined_vars = List.filter (fun v -> 
              not (List.mem v defined_vars) &&
              not (List.mem v circuit.inputs) &&
              not (List.mem v circuit.private_inputs)
            ) used_vars in
            (match undefined_vars with
             | [] -> check_vars (var :: defined_vars) rest
             | undefined_var :: _ -> 
                 Error (Types.unbound_variable_error undefined_var pos))
        | Constraint expr ->
            let used_vars = collect_expr_vars expr in
            let undefined_vars = List.filter (fun v -> 
              not (List.mem v defined_vars) &&
              not (List.mem v circuit.inputs) &&
              not (List.mem v circuit.private_inputs)
            ) used_vars in
            (match undefined_vars with
             | [] -> check_vars defined_vars rest
             | undefined_var :: _ -> 
                 Error (Types.unbound_variable_error undefined_var pos))
  in
  check_vars [] circuit.body

let detect_cycles dependencies =
  let rec visit_node visited path node deps_map =
    if List.mem node path then
      Error (cyclic_dependency (List.rev (node :: path)) ())
    else if List.mem node visited then
      Ok visited
    else
      let deps = try List.assoc node deps_map with Not_found -> [] in
      let* new_visited = 
        List.fold_left (fun acc_result dep ->
          let* acc = acc_result in
          visit_node acc (node :: path) dep deps_map
        ) (Ok visited) deps
      in
      Ok (node :: new_visited)
  in
  
  let nodes = List.map fst dependencies in
  let deps_map = dependencies in
  
  List.fold_left (fun acc_result node ->
    let* acc = acc_result in
    visit_node acc [] node deps_map
  ) (Ok []) nodes |> Result.map (fun _ -> ())

let build_dependency_graph circuit =
  let rec build_deps acc = function
    | [] -> acc
    | stmt :: rest ->
        match stmt with
        | Assign (var, expr) ->
            let deps = collect_expr_vars expr in
            build_deps ((var, deps) :: acc) rest
        | Constraint _ -> build_deps acc rest
  in
  build_deps [] circuit.body

let check_cyclic_dependencies circuit pos =
  let dependencies = build_dependency_graph circuit in
  detect_cycles dependencies |>
  Result.map_error (fun err ->
    match err.kind with
    | CyclicDependency cycle -> 
        Types.type_error 
          (Printf.sprintf "Cyclic dependency detected: %s" 
           (String.concat " -> " cycle)) pos
    | _ -> err)

let check_input_usage circuit pos =
  let all_inputs = circuit.inputs @ circuit.private_inputs in
  let used_vars = List.fold_left (fun acc stmt ->
    match stmt with
    | Assign (_, expr) -> (collect_expr_vars expr) @ acc
    | Constraint expr -> (collect_expr_vars expr) @ acc
  ) [] circuit.body in
  
  let unique_used_vars = List.sort_uniq String.compare used_vars in
  let unused_inputs = List.filter (fun input -> 
    not (List.mem input unique_used_vars)
  ) all_inputs in
  
  match unused_inputs with
  | [] -> Ok pos
  | unused -> 
      Error (Types.type_error (Printf.sprintf "Unused inputs: %s" (String.concat ", " unused)) pos)

(* additional validation for variable naming conflicts *)
let check_variable_naming_conflicts circuit pos =
  
  (* check for reserved keywords *)
  let reserved_words = ["poseidon"; "assert"; "circuit"; "inputs"; "private"; "public"] in
  let all_vars = circuit.inputs @ circuit.private_inputs @ (List.fold_left (fun acc stmt ->
    match stmt with
    | Assign (var, _) -> var :: acc
    | Constraint _ -> acc
  ) [] circuit.body) in
  
  let conflicts = List.filter (fun var -> List.mem var reserved_words) all_vars in
  match conflicts with
  | [] -> Ok pos
  | _ :: _ ->
      Error (Types.type_error "Reserved keyword used as variable name" pos)

(* validate variable name consistency *)
let check_variable_name_consistency circuit pos =
  
  (* collect all variable references *)
  let all_references = List.fold_left (fun acc stmt ->
    match stmt with
    | Assign (_, expr) -> (collect_expr_vars expr) @ acc
    | Constraint expr -> (collect_expr_vars expr) @ acc
  ) [] circuit.body in
  
  (* check for potential typos (variables that differ by only one character) *)
  let all_defined = circuit.inputs @ circuit.private_inputs @ (List.fold_left (fun acc stmt ->
    match stmt with
    | Assign (var, _) -> var :: acc
    | Constraint _ -> acc
  ) [] circuit.body) in
  
  let check_similar var1 var2 =
    let len1 = String.length var1 in
    let len2 = String.length var2 in
    if abs (len1 - len2) <= 1 then
      (* simple edit distance check *)
      let rec count_diffs i j diffs =
        if i >= len1 && j >= len2 then diffs
        else if i >= len1 then diffs + (len2 - j)
        else if j >= len2 then diffs + (len1 - i)
        else if String.get var1 i = String.get var2 j then
          count_diffs (i + 1) (j + 1) diffs
        else
          count_diffs (i + 1) (j + 1) (diffs + 1)
      in
      count_diffs 0 0 0 <= 1
    else false
  in
  
  (* find potential typos in references *)
  let typo_check ref_var =
    if not (List.mem ref_var all_defined) then
      let similar_vars = List.filter (check_similar ref_var) all_defined in
      match similar_vars with
      | _ :: _ when String.length ref_var > 2 ->
          Some (Types.unbound_variable_error ref_var pos)
      | _ -> None
    else None
  in
  
  let typo_errors = List.filter_map typo_check (List.sort_uniq String.compare all_references) in
  match typo_errors with
  | [] -> Ok pos
  | err :: _ -> Error err

let validate_scoping circuit pos =
  let open Error in
  let* _ = check_variable_before_definition circuit pos in
  let* _ = check_cyclic_dependencies circuit pos in
  let* _ = check_input_usage circuit pos in
  let* _ = check_variable_naming_conflicts circuit pos in
  let* _ = check_variable_name_consistency circuit pos in
  Ok pos