open Analysis_types

(* security analysis for ZK circuits *)

(* analyze field security *)
let analyze_field_security () : security_level =
  (* BN254 provides ~128-bit security *)
  VeryStrong "BN254 field provides 128-bit security level"

(* analyze soundness properties *)
let analyze_soundness (metrics: complexity_metrics) (circuit: Ast.circuit) : security_level =
  let issues = ref [] in
  let score = ref 4 in (* start with strong security *)
  
  (* check for potential soundness issues *)
  
  (* excessive constraint reuse might indicate issues *)
  if metrics.total_constraints < (List.length circuit.inputs + List.length circuit.private_inputs) then (
    issues := "Very few constraints relative to inputs" :: !issues;
    decr score
  );
  
  (* check for trivial satisfiability *)
  if metrics.linear_constraints > metrics.total_constraints * 8 / 10 then (
    issues := "Circuit is mostly linear - may have weak soundness" :: !issues;
    decr score
  );
  
  (* insufficient hash constraints for claimed security *)
  if metrics.hash_constraints = 0 && List.length circuit.private_inputs > 0 then (
    issues := "No hash constraints but private inputs present" :: !issues;
    decr score
  );
  
  let security_desc = match !score with
    | 4 -> "Strong soundness - comprehensive constraint coverage"
    | 3 -> "Good soundness with minor concerns: " ^ String.concat "; " !issues
    | 2 -> "Moderate soundness with issues: " ^ String.concat "; " !issues  
    | _ -> "Weak soundness with serious issues: " ^ String.concat "; " !issues
  in
  
  match !score with
  | 4 -> VeryStrong security_desc
  | 3 -> Strong security_desc
  | 2 -> Moderate security_desc
  | _ -> Weak security_desc

(* analyze zero-knowledge properties *)
let analyze_zero_knowledge (circuit: Ast.circuit) (metrics: complexity_metrics) : security_level =
  let issues = ref [] in
  let score = ref 4 in
  
  (* check for potential ZK leakage *)
  
  (* no private inputs means no ZK needed *)
  if List.length circuit.private_inputs = 0 then (
    Strong "No private inputs - zero-knowledge properties not applicable"
  ) else (
    (* analyze potential information leakage *)
    
    (* insufficient randomness in constraints *)
    if metrics.hash_constraints = 0 then (
      issues := "No hash operations to provide cryptographic randomness" :: !issues;
      decr score
    );
    
    (* simple linear relationships might leak information *)
    let linear_ratio = float_of_int metrics.linear_constraints /. float_of_int metrics.total_constraints in
    if linear_ratio > 0.8 then (
      issues := "High proportion of linear constraints may leak information" :: !issues;
      decr score
    );
    
    (* insufficient constraint diversity *)
    if metrics.comparison_constraints = 0 && metrics.quadratic_constraints = 0 then (
      issues := "Lack of constraint diversity may reduce ZK security" :: !issues;
      decr score  
    );
    
    let security_desc = match !score with
      | 4 -> "Strong zero-knowledge properties maintained" 
      | 3 -> "Good zero-knowledge with minor issues: " ^ String.concat "; " !issues
      | 2 -> "Moderate zero-knowledge security: " ^ String.concat "; " !issues
      | _ -> "Weak zero-knowledge security: " ^ String.concat "; " !issues
    in
    
    match !score with
    | 4 -> VeryStrong security_desc
    | 3 -> Strong security_desc  
    | 2 -> Moderate security_desc
    | _ -> Weak security_desc
  )

(* identify potential vulnerabilities *)
let identify_vulnerabilities (circuit: Ast.circuit) (metrics: complexity_metrics) : string list =
  let vulnerabilities = ref [] in
  
  (* check for common ZK vulnerabilities *)
  
  (* under-constrained circuits *)
  if metrics.total_constraints < (List.length circuit.inputs + List.length circuit.private_inputs) * 2 then
    vulnerabilities := "Potentially under-constrained circuit" :: !vulnerabilities;
  
  (* lack of range constraints *)
  if metrics.comparison_constraints = 0 && List.length circuit.private_inputs > 0 then
    vulnerabilities := "No range or bounds checking on private inputs" :: !vulnerabilities;
    
  (* excessive reliance on hash functions *)
  if metrics.hash_constraints > metrics.total_constraints / 2 then
    vulnerabilities := "Heavy reliance on hash functions may impact efficiency" :: !vulnerabilities;
    
  (* potential for malformed witness *)
  if metrics.linear_constraints = metrics.total_constraints then
    vulnerabilities := "Entirely linear circuit may allow trivial witness generation" :: !vulnerabilities;
    
  (* insufficient input validation *)
  let has_input_validation = List.exists (function
    | Ast.Constraint (Ast.Equal (Ast.Var _, _)) -> true
    | _ -> false
  ) circuit.body in
  
  if not has_input_validation && List.length circuit.inputs > 0 then
    vulnerabilities := "No explicit validation of public inputs" :: !vulnerabilities;
    
  !vulnerabilities

(* generate security recommendations *)
let generate_security_recommendations 
  (circuit: Ast.circuit) 
  (metrics: complexity_metrics) 
  (vulnerabilities: string list) : string list =
  
  let recommendations = ref [] in
  
  (* based on vulnerabilities found *)
  List.iter (fun vuln ->
    match vuln with
    | s when String.contains s 'u' && String.contains s 'n' && String.contains s 'd' ->
        recommendations := "Add more constraints to ensure circuit completeness" :: !recommendations;
        recommendations := "Verify that all private inputs are properly constrained" :: !recommendations
    | s when String.contains s 'r' && String.contains s 'a' ->
        recommendations := "Add range constraints for numerical private inputs" :: !recommendations;
        recommendations := "Implement bounds checking for sensitive values" :: !recommendations
    | s when String.contains s 'h' && String.contains s 'a' ->
        recommendations := "Consider balancing hash operations with other constraint types" :: !recommendations
    | s when String.contains s 'l' && String.contains s 'i' ->
        recommendations := "Add non-linear constraints to increase circuit complexity" :: !recommendations
    | s when String.contains s 'v' && String.contains s 'a' ->
        recommendations := "Add input validation constraints for public parameters" :: !recommendations
    | _ -> ()
  ) vulnerabilities;
  
  (* general security recommendations *)
  if List.length circuit.private_inputs > 0 then (
    recommendations := "Use cryptographically secure random values for private inputs" :: !recommendations;
    recommendations := "Implement proper input sanitization before witness generation" :: !recommendations
  );
  
  if metrics.hash_constraints > 0 then
    recommendations := "Verify Poseidon hash implementation follows security best practices" :: !recommendations;
    
  (* performance vs security tradeoffs *)
  if metrics.multiplicative_depth > 8 then
    recommendations := "High multiplicative depth provides strong security but impacts performance" :: !recommendations;
    
  List.rev !recommendations

(* comprehensive security analysis *)
let analyze_security 
  (circuit: Ast.circuit) 
  (metrics: complexity_metrics) 
  (_config: analysis_config) : security_analysis =
  
  let field_security = analyze_field_security () in
  let soundness_level = analyze_soundness metrics circuit in
  let zk_level = analyze_zero_knowledge circuit metrics in
  let vulnerabilities = identify_vulnerabilities circuit metrics in
  let recommendations = generate_security_recommendations circuit metrics vulnerabilities in
  
  {
    field_security;
    soundness_level;
    zero_knowledge_level = zk_level;
    potential_vulnerabilities = vulnerabilities;
    recommendations;
  }

(* security analysis summary *)
let security_analysis_summary (security: security_analysis) : string =
  Printf.sprintf
    "Security Analysis:\n\
     - Field Security: %s\n\
     - Soundness Level: %s\n\
     - Zero-Knowledge Level: %s\n\
     - Vulnerabilities Found: %d\n\
     - Recommendations: %d\n\n\
     Potential Issues:\n%s\n\n\
     Security Recommendations:\n%s"
    (security_level_to_string security.field_security)
    (security_level_to_string security.soundness_level)  
    (security_level_to_string security.zero_knowledge_level)
    (List.length security.potential_vulnerabilities)
    (List.length security.recommendations)
    (String.concat "\n- " ("" :: security.potential_vulnerabilities))
    (String.concat "\n- " ("" :: security.recommendations))

(* security-related bottlenecks *)
let identify_security_bottlenecks (security: security_analysis) : bottleneck list =
  let bottlenecks = ref [] in
  
  (* check for critical security issues *)
  let has_weak_security = List.exists (function
    | Weak _ -> true
    | _ -> false
  ) [security.field_security; security.soundness_level; security.zero_knowledge_level] in
  
  if has_weak_security then
    bottlenecks := {
      bottleneck_type = ConstraintCount "Weak security properties";
      severity = `Critical;
      description = "Circuit has weak security properties that compromise safety";
      impact_estimate = "Circuit may be vulnerable to attacks";
      suggested_optimizations = [
        "Add additional constraints for security";
        "Review circuit design for completeness";
        "Implement proper input validation";
        "Consider security audit"
      ];
    } :: !bottlenecks;
    
  (* check for moderate security concerns *)
  if List.length security.potential_vulnerabilities >= 3 then
    bottlenecks := {
      bottleneck_type = ConstraintCount "Multiple security concerns";
      severity = `High;
      description = Printf.sprintf "Circuit has %d potential security vulnerabilities" 
        (List.length security.potential_vulnerabilities);
      impact_estimate = "Multiple vulnerabilities increase attack surface";
      suggested_optimizations = security.recommendations;
    } :: !bottlenecks;
    
  !bottlenecks