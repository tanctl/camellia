(* constant folding optimization pass *)
open Debug
open Pass_interface

let pass_name = "constant_folding"
let pass_description = "Fold constant expressions at compile time"

type folding_context = {
  constants: (string, string) Hashtbl.t; (* variable -> constant value *)
  folded_count: int ref;
  debug_ctx: debug_context;
}

let create_folding_context debug_ctx = {
  constants = Hashtbl.create 32;
  folded_count = ref 0;
  debug_ctx;
}

let is_constant_value value =
  try
    let _ = int_of_string value in true
  with Failure _ -> false

let evaluate_constant_operation op v1 v2 =
  try
    let i1 = int_of_string v1 in
    let i2 = int_of_string v2 in
    let result = match op with
      | "add" -> i1 + i2
      | "mul" -> i1 * i2
      | "sub" -> i1 - i2
      | _ -> failwith "unsupported operation"
    in
    Some (string_of_int result)
  with _ -> None

let rec fold_expression ctx expr =
  match expr with
  | Ast.Var name ->
      (match Hashtbl.find_opt ctx.constants name with
       | Some const_val ->
           log ctx.debug_ctx Trace "Folding variable %s to constant %s" name const_val;
           incr ctx.folded_count;
           Ast.Const const_val
       | None -> expr)
  
  | Ast.Const _value -> expr (* alr a constant *)
  
  | Ast.Add (e1, e2) ->
      let folded_e1 = fold_expression ctx e1 in
      let folded_e2 = fold_expression ctx e2 in
      (match (folded_e1, folded_e2) with
       | (Ast.Const v1, Ast.Const v2) when is_constant_value v1 && is_constant_value v2 ->
           (match evaluate_constant_operation "add" v1 v2 with
            | Some result ->
                log ctx.debug_ctx Trace "Folding addition: %s + %s = %s" v1 v2 result;
                incr ctx.folded_count;
                Ast.Const result
            | None -> Ast.Add (folded_e1, folded_e2))
       | _ -> Ast.Add (folded_e1, folded_e2))
  
  | Ast.Sub (e1, e2) ->
      let folded_e1 = fold_expression ctx e1 in
      let folded_e2 = fold_expression ctx e2 in
      (match (folded_e1, folded_e2) with
       | (Ast.Const v1, Ast.Const v2) when is_constant_value v1 && is_constant_value v2 ->
           (match evaluate_constant_operation "sub" v1 v2 with
            | Some result ->
                log ctx.debug_ctx Trace "Folding subtraction: %s - %s = %s" v1 v2 result;
                incr ctx.folded_count;
                Ast.Const result
            | None -> Ast.Sub (folded_e1, folded_e2))
       | _ -> Ast.Sub (folded_e1, folded_e2))
  
  | Ast.Mul (e1, e2) ->
      let folded_e1 = fold_expression ctx e1 in
      let folded_e2 = fold_expression ctx e2 in
      (match (folded_e1, folded_e2) with
       | (Ast.Const v1, Ast.Const v2) when is_constant_value v1 && is_constant_value v2 ->
           (match evaluate_constant_operation "mul" v1 v2 with
            | Some result ->
                log ctx.debug_ctx Trace "Folding multiplication: %s * %s = %s" v1 v2 result;
                incr ctx.folded_count;
                Ast.Const result
            | None -> Ast.Mul (folded_e1, folded_e2))
       | (Ast.Const "0", _) | (_, Ast.Const "0") ->
           log ctx.debug_ctx Trace "Folding multiplication by zero to 0";
           incr ctx.folded_count;
           Ast.Const "0"
       | (Ast.Const "1", e) ->
           log ctx.debug_ctx Trace "Folding multiplication by one";
           incr ctx.folded_count;
           e
       | (e, Ast.Const "1") ->
           log ctx.debug_ctx Trace "Folding multiplication by one";
           incr ctx.folded_count;
           e
       | _ -> Ast.Mul (folded_e1, folded_e2))
  
  | Ast.Equal (e1, e2) ->
      let folded_e1 = fold_expression ctx e1 in
      let folded_e2 = fold_expression ctx e2 in
      (match (folded_e1, folded_e2) with
       | (Ast.Const v1, Ast.Const v2) when v1 = v2 ->
           log ctx.debug_ctx Trace "Folding equality: %s == %s -> true (represented as 1)" v1 v2;
           incr ctx.folded_count;
           Ast.Const "1"
       | _ -> Ast.Equal (folded_e1, folded_e2))
  
  | Ast.Poseidon exprs ->
      let folded_exprs = List.map (fold_expression ctx) exprs in
      Ast.Poseidon folded_exprs

let fold_statement ctx stmt =
  match stmt with
  | Ast.Assign (var, expr) ->
      let folded_expr = fold_expression ctx expr in
      (* track constants for future folding *)
      (match folded_expr with
       | Ast.Const value -> 
           Hashtbl.replace ctx.constants var value;
           log ctx.debug_ctx Trace "Recording constant assignment: %s = %s" var value
       | _ -> 
           Hashtbl.remove ctx.constants var);
      Ast.Assign (var, folded_expr)
  
  | Ast.Constraint expr ->
      let folded_expr = fold_expression ctx expr in
      Ast.Constraint folded_expr

let apply_constant_folding_impl pass_ctx circuit =
  let ctx = create_folding_context pass_ctx.Pass_interface.debug_ctx in
  
  log pass_ctx.Pass_interface.debug_ctx Info "Starting constant folding pass";
  
  (* first pass: identify constants from assignments *)
  List.iter (function
    | Ast.Assign (var, Ast.Const value) when is_constant_value value ->
        Hashtbl.replace ctx.constants var value;
        log pass_ctx.debug_ctx Debug "Found constant: %s = %s" var value
    | _ -> ()
  ) circuit.Ast.body;
  
  (* second pass: fold expressions *)
  let folded_body = List.map (fold_statement ctx) circuit.body in
  
  let optimized_circuit = { circuit with body = folded_body } in
  let folded_count = !(ctx.folded_count) in
  
  log pass_ctx.Pass_interface.debug_ctx Info "Constant folding completed: %d constants folded" folded_count;
  
  let statistics = {
    empty_statistics with
    expressions_processed = List.length circuit.body;
    statements_processed = List.length circuit.body;
    circuits_processed = 1;
    optimizations_applied = folded_count;
    constants_folded = folded_count;
  } in
  
  let changed = folded_count > 0 in
  
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
    apply_constant_folding_impl