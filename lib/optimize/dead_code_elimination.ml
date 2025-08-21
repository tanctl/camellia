open Error
open Debug
open Pass_interface

let pass_name = "dead_code_elimination"
let pass_description = "Remove unused variable assignments and unreachable code"

type usage_context = {
  assigned_vars: string list ref;
  used_vars: string list ref;
  eliminated_count: int ref;
  debug_ctx: debug_context;
}

let create_usage_context debug_ctx = {
  assigned_vars = ref [];
  used_vars = ref [];
  eliminated_count = ref 0;
  debug_ctx;
}

let rec collect_used_variables expr used_vars =
  match expr with
  | Ast.Var name -> 
      if not (List.mem name !used_vars) then
        used_vars := name :: !used_vars
  | Ast.Const _ -> ()
  | Ast.Add (e1, e2) | Ast.Mul (e1, e2) | Ast.Equal (e1, e2) ->
      collect_used_variables e1 used_vars;
      collect_used_variables e2 used_vars
  | Ast.Poseidon exprs ->
      List.iter (fun e -> collect_used_variables e used_vars) exprs

let collect_assigned_variables stmt assigned_vars =
  match stmt with
  | Ast.Assign (var, _) ->
      if not (List.mem var !assigned_vars) then
        assigned_vars := var :: !assigned_vars
  | Ast.Constraint _ -> ()

let analyze_variable_usage circuit ctx =
  (* collect all assigned variables *)
  List.iter (fun stmt -> collect_assigned_variables stmt ctx.assigned_vars) circuit.Ast.body;
  
  (* collect all used variables from expressions *)
  List.iter (function
    | Ast.Assign (_, expr) -> collect_used_variables expr ctx.used_vars
    | Ast.Constraint expr -> collect_used_variables expr ctx.used_vars
  ) circuit.body;
  
  (* also mark circuit inputs and private inputs as used *)
  List.iter (fun input -> 
    if not (List.mem input !(ctx.used_vars)) then
      ctx.used_vars := input :: !(ctx.used_vars)
  ) (circuit.inputs @ circuit.private_inputs);
  
  log ctx.debug_ctx Debug "Variable analysis: %d assigned, %d used" 
    (List.length !(ctx.assigned_vars)) (List.length !(ctx.used_vars))

let is_variable_used var used_vars =
  List.mem var used_vars

let eliminate_dead_assignments circuit ctx used_vars =
  let filtered_body = List.filter (function
    | Ast.Assign (var, _) when not (is_variable_used var used_vars) ->
        log ctx.debug_ctx Trace "Eliminating dead assignment: %s" var;
        incr ctx.eliminated_count;
        false
    | _ -> true
  ) circuit.Ast.body in
  
  { circuit with body = filtered_body }

let detect_unreachable_code circuit ctx =
  (* simple unreachable code detection *)
  let rec scan_for_unreachable acc = function
    | [] -> List.rev acc
    | stmt :: rest ->
        match stmt with
        | Ast.Constraint (Ast.Const "0") ->
            (* constraint that's always false - code after this is unreachable *)
            log ctx.debug_ctx Trace "Found unreachable code after constraint 0";
            List.rev (stmt :: acc)
        | _ -> scan_for_unreachable (stmt :: acc) rest
  in
  
  let reachable_body = scan_for_unreachable [] circuit.Ast.body in
  let eliminated = List.length circuit.Ast.body - List.length reachable_body in
  
  if eliminated > 0 then (
    log ctx.debug_ctx Trace "Eliminated %d unreachable statements" eliminated;
    ctx.eliminated_count := !(ctx.eliminated_count) + eliminated
  );
  
  { circuit with body = reachable_body }

let eliminate_redundant_constraints circuit ctx =
  (* remove duplicate constraints *)
  let seen_constraints = ref [] in
  let filtered_body = List.filter (function
    | Ast.Constraint expr ->
        let expr_str = Ast.expr_to_string expr in
        if List.mem expr_str !seen_constraints then (
          log ctx.debug_ctx Trace "Eliminating duplicate constraint: %s" expr_str;
          incr ctx.eliminated_count;
          false
        ) else (
          seen_constraints := expr_str :: !seen_constraints;
          true
        )
    | _ -> true
  ) circuit.Ast.body in
  
  { circuit with body = filtered_body }

let eliminate_identity_assignments circuit ctx =
  (* remove assignments like x = x *)
  let filtered_body = List.filter (function
    | Ast.Assign (var, Ast.Var var2) when var = var2 ->
        log ctx.debug_ctx Trace "Eliminating identity assignment: %s = %s" var var2;
        incr ctx.eliminated_count;
        false
    | _ -> true
  ) circuit.Ast.body in
  
  { circuit with body = filtered_body }

let perform_iterative_elimination circuit ctx =
  let rec eliminate_until_stable current_circuit iteration =
    let prev_count = !(ctx.eliminated_count) in
    
    (* perform analysis *)
    ctx.assigned_vars := [];
    ctx.used_vars := [];
    analyze_variable_usage current_circuit ctx;
    
    (* apply elimination techniques *)
    let after_dead_vars = eliminate_dead_assignments current_circuit ctx !(ctx.used_vars) in
    let after_unreachable = detect_unreachable_code after_dead_vars ctx in
    let after_redundant = eliminate_redundant_constraints after_unreachable ctx in
    let after_identity = eliminate_identity_assignments after_redundant ctx in
    
    let current_count = !(ctx.eliminated_count) in
    let eliminated_this_round = current_count - prev_count in
    
    log ctx.debug_ctx Debug "Elimination iteration %d: eliminated %d statements" 
      iteration eliminated_this_round;
    
    if eliminated_this_round > 0 && iteration < 5 then
      eliminate_until_stable after_identity (iteration + 1)
    else
      after_identity
  in
  
  eliminate_until_stable circuit 1

let apply_dead_code_elimination pass_ctx circuit =
  let ctx = create_usage_context pass_ctx.debug_ctx in
  
  log pass_ctx.debug_ctx Info "Starting dead code elimination pass";
  
  let original_stmt_count = List.length circuit.Ast.body in
  
  let optimized_circuit = perform_iterative_elimination circuit ctx in
  
  let final_stmt_count = List.length optimized_circuit.body in
  let eliminated_count = !(ctx.eliminated_count) in
  
  log pass_ctx.debug_ctx Info "Dead code elimination completed: %d statements eliminated (%d -> %d)" 
    eliminated_count original_stmt_count final_stmt_count;
  
  let statistics = {
    empty_statistics with
    expressions_processed = original_stmt_count * 2; (* estimate expressions *)
    statements_processed = original_stmt_count;
    circuits_processed = 1;
    optimizations_applied = eliminated_count;
    dead_code_eliminated = eliminated_count;
  } in
  
  let changed = eliminated_count > 0 in
  
  Ok {
    ast = optimized_circuit;
    statistics;
    changed;
    verification_hash = None;
  }

let enabled_at_level = function
  | O0 -> false
  | O1 | O2 | O3 -> true

let create_pass () =
  create_basic_pass 
    pass_name 
    pass_description
    enabled_at_level
    apply_dead_code_elimination