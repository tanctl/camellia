open Analysis_types

(* track the multiplicative depth of expressions *)
type expr_depth = {
  additive_depth: int;    (* depth of addition operations *)
  multiplicative_depth: int; (* depth of multiplication operations *) 
  hash_depth: int;        (* depth of hash operations *)
}

let zero_depth = { additive_depth = 0; multiplicative_depth = 0; hash_depth = 0 }

let max_depth d1 d2 = {
  additive_depth = max d1.additive_depth d2.additive_depth;
  multiplicative_depth = max d1.multiplicative_depth d2.multiplicative_depth; 
  hash_depth = max d1.hash_depth d2.hash_depth;
}

(* calculate depth for AST expressions *)
let rec calculate_ast_expr_depth (expr: Ast.expr) : expr_depth =
  match expr with
  | Ast.Var _ | Ast.Const _ -> zero_depth
  | Ast.Add (e1, e2) ->
      let d1 = calculate_ast_expr_depth e1 in
      let d2 = calculate_ast_expr_depth e2 in
      let max_d = max_depth d1 d2 in
      { max_d with additive_depth = max_d.additive_depth + 1 }
  | Ast.Sub (e1, e2) ->
      let d1 = calculate_ast_expr_depth e1 in
      let d2 = calculate_ast_expr_depth e2 in
      let max_d = max_depth d1 d2 in
      { max_d with additive_depth = max_d.additive_depth + 1 }
  | Ast.Mul (e1, e2) ->
      let d1 = calculate_ast_expr_depth e1 in
      let d2 = calculate_ast_expr_depth e2 in
      let max_d = max_depth d1 d2 in
      { max_d with multiplicative_depth = max_d.multiplicative_depth + 1 }
  | Ast.Equal (e1, e2) ->
      (* equality doesn't add multiplicative depth *)
      let d1 = calculate_ast_expr_depth e1 in
      let d2 = calculate_ast_expr_depth e2 in
      max_depth d1 d2
  | Ast.Poseidon exprs ->
      let depths = List.map calculate_ast_expr_depth exprs in
      let max_input_depth = List.fold_left max_depth zero_depth depths in
      { max_input_depth with hash_depth = max_input_depth.hash_depth + 1 }

(* analyze circuit multiplicative depth *)
let analyze_circuit_depth (circuit: Ast.circuit) : expr_depth =
  let stmt_depths = List.map (function
    | Ast.Assign (_, expr) -> calculate_ast_expr_depth expr
    | Ast.Constraint expr -> calculate_ast_expr_depth expr
  ) circuit.body in
  
  List.fold_left max_depth zero_depth stmt_depths

(* calculate critical path through constraints *)
let calculate_critical_path (r1cs: R1cs.r1cs_system) : int =
  let constraints = r1cs.constraints in
  let num_constraints = List.length constraints in
  
  (* simplified critical path - actual implementation would need dependency analysis *)
  if num_constraints <= 5 then num_constraints
  else if num_constraints <= 20 then num_constraints / 2 + 3
  else num_constraints / 3 + 5

(* analyze parallelization opportunities *)
let analyze_parallelization (circuit: Ast.circuit) : int * string list =
  let independent_operations = ref 0 in
  let parallelization_opportunities = ref [] in
  
  let analyze_stmt = function
    | Ast.Assign (var, Ast.Add _) ->
        incr independent_operations;
        parallelization_opportunities := 
          (Printf.sprintf "Addition in %s can be parallelized" var) :: !parallelization_opportunities
    | Ast.Assign (var, Ast.Mul _) ->
        parallelization_opportunities := 
          (Printf.sprintf "Multiplication in %s requires sequential processing" var) :: !parallelization_opportunities
    | Ast.Assign (var, Ast.Poseidon _) ->
        parallelization_opportunities := 
          (Printf.sprintf "Hash operation in %s has internal parallelization potential" var) :: !parallelization_opportunities
    | _ -> ()
  in
  
  List.iter analyze_stmt circuit.body;
  (!independent_operations, !parallelization_opportunities)

(* estimate proving complexity based on depth *)
let estimate_proving_complexity (depth: expr_depth) : [`Low | `Medium | `High | `VeryHigh] =
  let total_complexity = 
    depth.multiplicative_depth * 3 + 
    depth.hash_depth * 5 +
    depth.additive_depth in
    
  if total_complexity <= 10 then `Low
  else if total_complexity <= 25 then `Medium  
  else if total_complexity <= 50 then `High
  else `VeryHigh

(* analyze bottlenecks related to depth *)
let analyze_depth_bottlenecks (depth: expr_depth) (_config: analysis_config) : bottleneck list =
  let bottlenecks = ref [] in
  
  if depth.multiplicative_depth > 10 then
    bottlenecks := {
      bottleneck_type = MultDepth "High multiplicative depth";
      severity = `High;
      description = Printf.sprintf "Circuit has multiplicative depth of %d, which may impact proving time" depth.multiplicative_depth;
      impact_estimate = "Proving time increases exponentially with multiplicative depth";
      suggested_optimizations = [
        "Consider factoring complex multiplications";
        "Use precomputed values where possible";
        "Explore alternative circuit representations"
      ];
    } :: !bottlenecks;
    
  if depth.hash_depth > 5 then
    bottlenecks := {
      bottleneck_type = HashOperations "Many nested hash operations";
      severity = `Medium;
      description = Printf.sprintf "Circuit contains %d levels of nested hash operations" depth.hash_depth;
      impact_estimate = "Each hash level adds significant constraint overhead";
      suggested_optimizations = [
        "Batch hash inputs where possible";
        "Consider using Merkle tree structures";
        "Evaluate alternative hash functions"
      ];
    } :: !bottlenecks;
    
  !bottlenecks

let depth_analysis_summary (depth: expr_depth) : string =
  let complexity = estimate_proving_complexity depth in
  let complexity_str = match complexity with
    | `Low -> "Low"
    | `Medium -> "Medium" 
    | `High -> "High"
    | `VeryHigh -> "Very High"
  in
  
  Printf.sprintf
    "Depth Analysis:\n\
     - Multiplicative Depth: %d\n\
     - Hash Operation Depth: %d\n\
     - Additive Depth: %d\n\
     - Overall Complexity: %s\n\
     - Critical Path Impact: %s"
    depth.multiplicative_depth
    depth.hash_depth  
    depth.additive_depth
    complexity_str
    (if depth.multiplicative_depth > 8 then "High - will significantly impact proving time"
     else if depth.multiplicative_depth > 4 then "Medium - moderate impact on proving time"
     else "Low - minimal impact on proving time")