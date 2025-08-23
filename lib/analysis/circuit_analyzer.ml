open Analysis_types
open Error

(* main circuit analysis engine *)

let analyze_circuit 
  ?(config = default_analysis_config)
  (circuit: Ast.circuit) 
  (r1cs: R1cs.r1cs_system) : circuit_analysis Error.result =
  
  try
    (* step 1: constraint analysis *)
    let complexity = Constraint_analyzer.analyze_r1cs_constraints r1cs in
    
    (* step 2: depth analysis *)
    let depth = Depth_analyzer.analyze_circuit_depth circuit in
    let critical_path = Depth_analyzer.calculate_critical_path r1cs in
    let (parallelizable, _) = Depth_analyzer.analyze_parallelization circuit in
    
    (* update complexity with depth information *)
    let complexity = { 
      complexity with 
      multiplicative_depth = depth.multiplicative_depth;
      critical_path_length = critical_path;
      parallelizable_operations = parallelizable;
    } in
    
    (* step 3: performance analysis *)
    let performance = Performance_analyzer.analyze_performance 
      complexity
      (List.length circuit.inputs)
      (List.length circuit.private_inputs)
      r1cs.metadata.num_variables
    in
    
    (* step 4: security analysis *)
    let security = Security_analyzer.analyze_security circuit complexity config in
    
    (* step 5: bottleneck identification *)
    let constraint_bottlenecks = Performance_analyzer.identify_performance_bottlenecks performance complexity in
    let depth_bottlenecks = Depth_analyzer.analyze_depth_bottlenecks depth config in  
    let security_bottlenecks = Security_analyzer.identify_security_bottlenecks security in
    let all_bottlenecks = constraint_bottlenecks @ depth_bottlenecks @ security_bottlenecks in
    
    (* combine into comprehensive analysis *)
    Ok {
      circuit_name = circuit.name;
      complexity;
      performance;
      security;
      bottlenecks = all_bottlenecks;
      analysis_timestamp = Unix.time ();
      analysis_version = "1.0.0";
    }
    
  with
  | exn -> Error (circuit_error ("Analysis failed: " ^ Printexc.to_string exn) ())

(* quick analysis for basic metrics *)
let quick_analyze (_circuit: Ast.circuit) (r1cs: R1cs.r1cs_system) : complexity_metrics Error.result =
  try
    Ok (Constraint_analyzer.analyze_r1cs_constraints r1cs)
  with  
  | exn -> Error (circuit_error ("Quick analysis failed: " ^ Printexc.to_string exn) ())

(* analysis with custom configuration *)
let analyze_with_config 
  (config: analysis_config)
  (circuit: Ast.circuit) 
  (r1cs: R1cs.r1cs_system) : circuit_analysis Error.result =
  analyze_circuit ~config circuit r1cs

(* comparative analysis between circuits *)
let compare_circuits 
  (analysis1: circuit_analysis) 
  (analysis2: circuit_analysis) : string =
  
  let compare_field name get_value =
    let v1 = get_value analysis1 in
    let v2 = get_value analysis2 in
    let diff = v2 -. v1 in
    let percent_change = if v1 <> 0.0 then (diff /. v1) *. 100.0 else 0.0 in
    Printf.sprintf "%s: %.1f → %.1f (%.1f%%)" name v1 v2 percent_change
  in
  
  let compare_int_field name get_value =
    let v1 = get_value analysis1 in
    let v2 = get_value analysis2 in
    let diff = v2 - v1 in
    let percent_change = if v1 <> 0 then float_of_int diff /. float_of_int v1 *. 100.0 else 0.0 in
    Printf.sprintf "%s: %d → %d (%.1f%%)" name v1 v2 percent_change
  in
  
  String.concat "\n" [
    Printf.sprintf "Comparative Analysis: %s vs %s" analysis1.circuit_name analysis2.circuit_name;
    "";
    "Constraint Changes:";
    compare_int_field "  Total Constraints" (fun a -> a.complexity.total_constraints);
    compare_int_field "  Quadratic" (fun a -> a.complexity.quadratic_constraints);
    compare_int_field "  Hash Operations" (fun a -> a.complexity.hash_constraints);
    compare_int_field "  Multiplicative Depth" (fun a -> a.complexity.multiplicative_depth);
    "";
    "Performance Changes:";
    compare_field "  Proving Time (ms)" (fun a -> a.performance.proving_time_ms);
    compare_field "  Memory Usage (MB)" (fun a -> a.performance.memory_usage_mb);
    compare_field "  Setup Time (ms)" (fun a -> a.performance.setup_time_ms);
    "";
    "Security Changes:";
    Printf.sprintf "  Vulnerabilities: %d → %d" 
      (List.length analysis1.security.potential_vulnerabilities)
      (List.length analysis2.security.potential_vulnerabilities);
    Printf.sprintf "  Bottlenecks: %d → %d"
      (List.length analysis1.bottlenecks)
      (List.length analysis2.bottlenecks);
  ]

(* analysis trend tracking *)
type analysis_trend = {
  timestamps: float list;
  constraint_counts: int list;
  proving_times: float list;
  memory_usage: float list;
}

let track_analysis_trend (analyses: circuit_analysis list) : analysis_trend =
  let timestamps = List.map (fun a -> a.analysis_timestamp) analyses in
  let constraint_counts = List.map (fun a -> a.complexity.total_constraints) analyses in
  let proving_times = List.map (fun a -> a.performance.proving_time_ms) analyses in
  let memory_usage = List.map (fun a -> a.performance.memory_usage_mb) analyses in
  
  { timestamps; constraint_counts; proving_times; memory_usage }

(* bottleneck prioritization *)
let prioritize_bottlenecks (bottlenecks: bottleneck list) : bottleneck list =
  let severity_order = function
    | `Critical -> 4
    | `High -> 3  
    | `Medium -> 2
    | `Low -> 1
  in
  
  List.sort (fun b1 b2 -> 
    compare (severity_order b2.severity) (severity_order b1.severity)
  ) bottlenecks

(* analysis validation *)
let validate_analysis (analysis: circuit_analysis) : (bool * string list) =
  let errors = ref [] in
  let warnings = ref [] in
  
  (* sanity checks *)
  if analysis.complexity.total_constraints <= 0 then
    errors := "No constraints found in analysis" :: !errors;
    
  if analysis.performance.proving_time_ms < 0.0 then
    errors := "Negative proving time estimate" :: !errors;
    
  if analysis.performance.memory_usage_mb < 0.0 then
    errors := "Negative memory usage estimate" :: !errors;
    
  (* consistency checks *)  
  let total_categorized = 
    analysis.complexity.linear_constraints +
    analysis.complexity.quadratic_constraints +
    analysis.complexity.hash_constraints +
    analysis.complexity.boolean_constraints +
    analysis.complexity.comparison_constraints
  in
  
  if total_categorized <> analysis.complexity.total_constraints then
    warnings := "Constraint categorization doesn't sum to total" :: !warnings;
    
  (* performance reasonableness *)
  if analysis.performance.proving_time_ms > 60000.0 then
    warnings := "Very high proving time estimate (>1 minute)" :: !warnings;
    
  if analysis.performance.memory_usage_mb > 2048.0 then  
    warnings := "Very high memory usage estimate (>2GB)" :: !warnings;
    
  let is_valid = List.length !errors = 0 in
  let all_issues = !errors @ !warnings in
  
  (is_valid, all_issues)

(* export analysis for external tools *)
let export_analysis_json (analysis: circuit_analysis) : Yojson.Safe.t =
  let bottleneck_to_json bottleneck =
    let bottleneck_type_str = match bottleneck.bottleneck_type with
      | ConstraintCount _s -> "constraint_count"
      | MultDepth _s -> "multiplicative_depth"
      | HashOperations _s -> "hash_operations"  
      | MemoryUsage _s -> "memory_usage"
      | CriticalPath _s -> "critical_path"
    in
    
    `Assoc [
      ("type", `String bottleneck_type_str);
      ("severity", `String (bottleneck_severity_to_string bottleneck.severity));
      ("description", `String bottleneck.description);
      ("impact", `String bottleneck.impact_estimate);
      ("optimizations", `List (List.map (fun s -> `String s) bottleneck.suggested_optimizations));
    ]
  in
  
  `Assoc [
    ("circuit_name", `String analysis.circuit_name);
    ("timestamp", `Float analysis.analysis_timestamp);
    ("version", `String analysis.analysis_version);
    ("complexity", `Assoc [
      ("total_constraints", `Int analysis.complexity.total_constraints);
      ("linear_constraints", `Int analysis.complexity.linear_constraints);
      ("quadratic_constraints", `Int analysis.complexity.quadratic_constraints);
      ("hash_constraints", `Int analysis.complexity.hash_constraints);
      ("multiplicative_depth", `Int analysis.complexity.multiplicative_depth);
      ("critical_path_length", `Int analysis.complexity.critical_path_length);
      ("parallelizable_operations", `Int analysis.complexity.parallelizable_operations);
    ]);
    ("performance", `Assoc [
      ("setup_time_ms", `Float analysis.performance.setup_time_ms);
      ("proving_time_ms", `Float analysis.performance.proving_time_ms);
      ("verification_time_ms", `Float analysis.performance.verification_time_ms);
      ("memory_usage_mb", `Float analysis.performance.memory_usage_mb);
      ("proof_size_bytes", `Int analysis.performance.proof_size_bytes);
    ]);
    ("security", `Assoc [
      ("field_security", `String (security_level_to_string analysis.security.field_security));
      ("soundness_level", `String (security_level_to_string analysis.security.soundness_level));
      ("zero_knowledge_level", `String (security_level_to_string analysis.security.zero_knowledge_level));
      ("vulnerabilities", `List (List.map (fun s -> `String s) analysis.security.potential_vulnerabilities));
      ("recommendations", `List (List.map (fun s -> `String s) analysis.security.recommendations));
    ]);
    ("bottlenecks", `List (List.map bottleneck_to_json analysis.bottlenecks));
  ]