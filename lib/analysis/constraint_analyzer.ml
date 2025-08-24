open Analysis_types

(* analyze individual constraints to categorize them *)
let categorize_constraint (constraint_triple: R1cs.constraint_triple) : constraint_category =
  let count_terms lc = List.length lc in
  let a_terms = count_terms constraint_triple.a in
  let b_terms = count_terms constraint_triple.b in
  let c_terms = count_terms constraint_triple.c in
  
  (* detect constraint patterns *)
  if a_terms <= 1 && b_terms = 1 && c_terms <= 1 then
    (* likely linear constraint: (wire + const) * 1 = result *)
    Linear
  else if a_terms = 1 && b_terms = 1 && c_terms = 1 then
    (* multiplication constraint: wire_a * wire_b = wire_c *)
    Quadratic
  else if a_terms > 2 || b_terms > 2 then
    (* complex constraint, likely hash function *)
    Hash
  else
    (* default to comparison for equality checks *)
    Comparison

(* analyze constraint complexity patterns from AST *)
let analyze_ast_constraint_patterns (circuit: Ast.circuit) : (constraint_category * int) list =
  let category_counts = ref [] in
  
  let count_category cat = 
    try 
      let current = List.assoc cat !category_counts in
      category_counts := (cat, current + 1) :: (List.remove_assoc cat !category_counts)
    with Not_found -> 
      category_counts := (cat, 1) :: !category_counts
  in
  
  let analyze_expr = function
    | Ast.Add (_, _) -> count_category Linear
    | Ast.Sub (_, _) -> count_category Linear
    | Ast.Mul (_, _) -> count_category Quadratic
    | Ast.Equal (_, _) -> count_category Comparison
    | Ast.Poseidon _ -> count_category Hash
    | Ast.Var _ | Ast.Const _ -> ()
  in
  
  let analyze_stmt = function
    | Ast.Assign (_, expr) -> analyze_expr expr
    | Ast.Constraint expr -> analyze_expr expr
  in
  
  List.iter analyze_stmt circuit.body;
  !category_counts

(* analyze R1CS constraint system *)
let analyze_r1cs_constraints (r1cs: R1cs.r1cs_system) : complexity_metrics =
  let constraints = r1cs.constraints in
  let categorized = List.map categorize_constraint constraints in
  
  let count_category target = 
    List.fold_left (fun acc cat -> if cat = target then acc + 1 else acc) 0 categorized in
    
  let linear_count = count_category Linear in
  let quadratic_count = count_category Quadratic in
  let hash_count = count_category Hash in
  let boolean_count = count_category Boolean in
  let comparison_count = count_category Comparison in
  
  (* estimate parallelizable operations *)
  let parallelizable = 
    (* linear operations can often be parallelized *)
    let parallel_linear = min linear_count 4 in
    (* some hash operations can be parallelized *)
    let parallel_hash = hash_count / 2 in
    parallel_linear + parallel_hash
  in
  
  {
    total_constraints = List.length constraints;
    linear_constraints = linear_count;
    quadratic_constraints = quadratic_count;
    hash_constraints = hash_count;
    boolean_constraints = boolean_count;
    comparison_constraints = comparison_count;
    multiplicative_depth = 0; (* calculated separately *)
    critical_path_length = List.length constraints; (* conservative estimate *)
    parallelizable_operations = parallelizable;
  }

(* calculate constraint density and complexity scores *)
let calculate_constraint_density (r1cs: R1cs.r1cs_system) : float =
  let total_vars = r1cs.metadata.num_variables in
  let total_constraints = r1cs.metadata.num_constraints in
  if total_vars = 0 then 0.0 
  else float_of_int total_constraints /. float_of_int total_vars

let calculate_complexity_score (metrics: complexity_metrics) : float =
  let linear_weight = 1.0 in
  let quadratic_weight = 2.0 in
  let hash_weight = 5.0 in
  let comparison_weight = 1.5 in
  
  let weighted_score = 
    (float_of_int metrics.linear_constraints) *. linear_weight +.
    (float_of_int metrics.quadratic_constraints) *. quadratic_weight +.
    (float_of_int metrics.hash_constraints) *. hash_weight +.
    (float_of_int metrics.comparison_constraints) *. comparison_weight
  in
  
  (* normalize by total constraints *)
  if metrics.total_constraints = 0 then 0.0
  else weighted_score /. float_of_int metrics.total_constraints

(* analyze constraint interdependencies *)
let analyze_constraint_dependencies (r1cs: R1cs.r1cs_system) : (int * int list) list =
  let constraints = List.mapi (fun i c -> (i, c)) r1cs.constraints in
  
  let get_constraint_wires (_, constraint_triple) =
    let get_wires lc = List.map fst lc in
    (get_wires constraint_triple.R1cs.a) @ 
    (get_wires constraint_triple.R1cs.b) @ 
    (get_wires constraint_triple.R1cs.c)
  in
  
  List.map (fun (i, c) ->
    let wires = get_constraint_wires (i, c) in
    let dependencies = List.fold_left (fun acc (j, other_c) ->
      if i != j then
        let other_wires = get_constraint_wires (j, other_c) in
        if List.exists (fun w -> List.mem w other_wires) wires then j :: acc
        else acc
      else acc
    ) [] constraints in
    (i, dependencies)
  ) constraints

let constraint_analysis_summary (metrics: complexity_metrics) : string =
  Printf.sprintf 
    "Constraint Analysis:\n\
     - Total: %d constraints\n\
     - Linear: %d (%.1f%%)\n\
     - Quadratic: %d (%.1f%%)\n\
     - Hash: %d (%.1f%%)\n\
     - Comparison: %d (%.1f%%)\n\
     - Parallelizable: %d operations\n\
     - Complexity Score: %.2f"
    metrics.total_constraints
    metrics.linear_constraints 
    (100.0 *. float_of_int metrics.linear_constraints /. float_of_int metrics.total_constraints)
    metrics.quadratic_constraints
    (100.0 *. float_of_int metrics.quadratic_constraints /. float_of_int metrics.total_constraints)  
    metrics.hash_constraints
    (100.0 *. float_of_int metrics.hash_constraints /. float_of_int metrics.total_constraints)
    metrics.comparison_constraints
    (100.0 *. float_of_int metrics.comparison_constraints /. float_of_int metrics.total_constraints)
    metrics.parallelizable_operations
    (calculate_complexity_score metrics)