open Error
open Debug
open Pass_interface

let pass_name = "algebraic_simplification"
let pass_description = "Apply algebraic identities and simplification rules"

type simplification_context = {
  simplifications_applied: int ref;
  debug_ctx: debug_context;
}

let create_simplification_context debug_ctx = {
  simplifications_applied = ref 0;
  debug_ctx;
}

let log_simplification ctx from_expr to_expr rule =
  log ctx.debug_ctx Trace "Algebraic simplification [%s]: %s -> %s" 
    rule (Ast.expr_to_string from_expr) (Ast.expr_to_string to_expr);
  incr ctx.simplifications_applied

let rec simplify_expression ctx expr =
  let simplified = match expr with
    (* Addition simplifications *)
    | Ast.Add (Ast.Const "0", e) ->
        log_simplification ctx expr e "0+x=x";
        simplify_expression ctx e
    | Ast.Add (e, Ast.Const "0") ->
        log_simplification ctx expr e "x+0=x";
        simplify_expression ctx e
    | Ast.Add (e1, e2) when Ast.expr_to_string e1 = Ast.expr_to_string e2 ->
        let doubled = Ast.Mul (Ast.Const "2", e1) in
        log_simplification ctx expr doubled "x+x=2*x";
        simplify_expression ctx doubled
    | Ast.Add (Ast.Const v1, Ast.Const v2) ->
        (* this should be caught by constant folding, but handle it here too *)
        (try
          let i1 = int_of_string v1 in
          let i2 = int_of_string v2 in
          let result = Ast.Const (string_of_int (i1 + i2)) in
          log_simplification ctx expr result "const+const";
          result
        with _ -> expr)
    
    (* multiplication simplifications *)
    | Ast.Mul (Ast.Const "0", _) | Ast.Mul (_, Ast.Const "0") ->
        let zero = Ast.Const "0" in
        log_simplification ctx expr zero "x*0=0";
        zero
    | Ast.Mul (Ast.Const "1", e) ->
        log_simplification ctx expr e "1*x=x";
        simplify_expression ctx e
    | Ast.Mul (e, Ast.Const "1") ->
        log_simplification ctx expr e "x*1=x";
        simplify_expression ctx e
    | Ast.Mul (Ast.Const "-1", e) ->
        (* represent as negative - this could be expanded with unary minus *)
        let simplified = simplify_expression ctx e in
        Ast.Mul (Ast.Const "-1", simplified)
    | Ast.Mul (e, Ast.Const "-1") ->
        let simplified = simplify_expression ctx e in
        Ast.Mul (simplified, Ast.Const "-1")
    | Ast.Mul (e1, e2) when Ast.expr_to_string e1 = Ast.expr_to_string e2 ->
        (* x * x -> x^2, but we dont have power operator, so keep as is for ZK *)
        Ast.Mul (simplify_expression ctx e1, simplify_expression ctx e2)
    | Ast.Mul (Ast.Const v1, Ast.Const v2) ->
        (try
          let i1 = int_of_string v1 in
          let i2 = int_of_string v2 in
          let result = Ast.Const (string_of_int (i1 * i2)) in
          log_simplification ctx expr result "const*const";
          result
        with _ -> expr)
    
    (* equality simplifications *)
    | Ast.Equal (e1, e2) when Ast.expr_to_string e1 = Ast.expr_to_string e2 ->
        let true_const = Ast.Const "1" in
        log_simplification ctx expr true_const "x==x=true";
        true_const
    | Ast.Equal (Ast.Const v1, Ast.Const v2) ->
        let result = if v1 = v2 then Ast.Const "1" else Ast.Const "0" in
        log_simplification ctx expr result "const==const";
        result
    
    (* eested expression simplifications *)
    | Ast.Add (e1, e2) ->
        let s1 = simplify_expression ctx e1 in
        let s2 = simplify_expression ctx e2 in
        if s1 != e1 || s2 != e2 then
          simplify_expression ctx (Ast.Add (s1, s2))
        else
          Ast.Add (s1, s2)
    | Ast.Mul (e1, e2) ->
        let s1 = simplify_expression ctx e1 in
        let s2 = simplify_expression ctx e2 in
        if s1 != e1 || s2 != e2 then
          simplify_expression ctx (Ast.Mul (s1, s2))
        else
          Ast.Mul (s1, s2)
    | Ast.Equal (e1, e2) ->
        let s1 = simplify_expression ctx e1 in
        let s2 = simplify_expression ctx e2 in
        if s1 != e1 || s2 != e2 then
          simplify_expression ctx (Ast.Equal (s1, s2))
        else
          Ast.Equal (s1, s2)
    
    (* poseidon hash simplifications *)
    | Ast.Poseidon exprs ->
        let simplified_exprs = List.map (simplify_expression ctx) exprs in
        Ast.Poseidon simplified_exprs
    
    (* base cases *)
    | Ast.Var _ | Ast.Const _ -> expr
  in
  simplified

let apply_distributive_law ctx expr =
  match expr with
  (* a * (b + c) -> (a * b) + (a * c) *)
  | Ast.Mul (a, Ast.Add (b, c)) ->
      let distributed = Ast.Add (Ast.Mul (a, b), Ast.Mul (a, c)) in
      log_simplification ctx expr distributed "distributive a*(b+c)";
      Some distributed
  | Ast.Mul (Ast.Add (b, c), a) ->
      let distributed = Ast.Add (Ast.Mul (b, a), Ast.Mul (c, a)) in
      log_simplification ctx expr distributed "distributive (b+c)*a";
      Some distributed
  | _ -> None

let apply_factoring ctx expr =
  match expr with
  (* (a * b) + (a * c) -> a * (b + c) *)
  | Ast.Add (Ast.Mul (a1, b), Ast.Mul (a2, c)) when Ast.expr_to_string a1 = Ast.expr_to_string a2 ->
      let factored = Ast.Mul (a1, Ast.Add (b, c)) in
      log_simplification ctx expr factored "factoring a*b+a*c";
      Some factored
  | _ -> None

let apply_advanced_simplifications ctx expr =
  match apply_distributive_law ctx expr with
  | Some simplified -> Some simplified
  | None -> apply_factoring ctx expr

let rec simplify_with_advanced_rules ctx expr =
  let basic_simplified = simplify_expression ctx expr in
  match apply_advanced_simplifications ctx basic_simplified with
  | Some advanced_simplified -> 
      (* recursively apply simplifications *)
      simplify_with_advanced_rules ctx advanced_simplified
  | None -> basic_simplified

let simplify_statement ctx stmt =
  match stmt with
  | Ast.Assign (var, expr) ->
      let simplified_expr = simplify_with_advanced_rules ctx expr in
      Ast.Assign (var, simplified_expr)
  | Ast.Constraint expr ->
      let simplified_expr = simplify_with_advanced_rules ctx expr in
      Ast.Constraint simplified_expr

let detect_contradictions circuit ctx =
  (* look for constraints that are always false *)
  let contradictions = List.filter (function
    | Ast.Constraint (Ast.Const "0") -> true
    | Ast.Constraint (Ast.Equal (Ast.Const v1, Ast.Const v2)) when v1 <> v2 -> true
    | _ -> false
  ) circuit.Ast.body in
  
  if List.length contradictions > 0 then (
    log ctx.debug_ctx Warning "Found %d contradictory constraints" (List.length contradictions);
    List.iter (function
      | Ast.Constraint expr -> 
          log ctx.debug_ctx Warning "Contradictory constraint: %s" (Ast.expr_to_string expr)
      | _ -> ()
    ) contradictions
  )

let apply_algebraic_simplification_impl pass_ctx circuit =
  let ctx = create_simplification_context pass_ctx.Pass_interface.debug_ctx in
  
  log pass_ctx.Pass_interface.debug_ctx Info "Starting algebraic simplification pass";
  
  let original_stmt_count = List.length circuit.Ast.body in
  
  (* apply simplifications to each statement *)
  let simplified_body = List.map (simplify_statement ctx) circuit.body in
  
  let optimized_circuit = { circuit with body = simplified_body } in
  
  (* detect any contradictions introduced or revealed *)
  detect_contradictions optimized_circuit ctx;
  
  let simplifications_count = !(ctx.simplifications_applied) in
  
  log pass_ctx.Pass_interface.debug_ctx Info "Algebraic simplification completed: %d simplifications applied" 
    simplifications_count;
  
  let statistics = {
    empty_statistics with
    expressions_processed = original_stmt_count * 3; (* estimate nested expressions *)
    statements_processed = original_stmt_count;
    circuits_processed = 1;
    optimizations_applied = simplifications_count;
    algebraic_simplifications = simplifications_count;
  } in
  
  let changed = simplifications_count > 0 in
  
  Ok {
    ast = optimized_circuit;
    statistics;
    changed;
    verification_hash = None;
  }

let enabled_at_level = function
  | O0 | O1 -> false
  | O2 | O3 -> true

let create_pass () =
  create_basic_pass 
    pass_name 
    pass_description
    enabled_at_level
    apply_algebraic_simplification_impl