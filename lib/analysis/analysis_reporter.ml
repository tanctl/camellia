open Analysis_types
open Error

(* comprehensive analysis reporting *)

let generate_executive_summary (analysis: circuit_analysis) : string =
  let complexity_rating = 
    if analysis.complexity.total_constraints <= 10 then "Simple"
    else if analysis.complexity.total_constraints <= 50 then "Moderate" 
    else if analysis.complexity.total_constraints <= 200 then "Complex"
    else "Very Complex"
  in
  
  let performance_rating =
    if analysis.performance.proving_time_ms <= 100.0 then "Excellent"
    else if analysis.performance.proving_time_ms <= 1000.0 then "Good"
    else if analysis.performance.proving_time_ms <= 5000.0 then "Acceptable"
    else "Poor"
  in
  
  let security_issues = List.length analysis.security.potential_vulnerabilities in
  let security_rating = 
    if security_issues = 0 then "Excellent"
    else if security_issues <= 2 then "Good"
    else if security_issues <= 4 then "Moderate"
    else "Concerning"
  in
  
  let critical_bottlenecks = List.filter (fun b -> b.severity = `Critical) analysis.bottlenecks in
  let high_bottlenecks = List.filter (fun b -> b.severity = `High) analysis.bottlenecks in
  
  Printf.sprintf
    "[ANALYSIS] CIRCUIT ANALYSIS EXECUTIVE SUMMARY\n\
     =====================================\n\n\
     Circuit: %s\n\
     Analysis Date: %s\n\n\
     [STATS] OVERALL ASSESSMENT:\n\
     - Complexity: %s (%d constraints)\n\
     - Performance: %s (%.1fms proving time)\n\
     - Security: %s (%d issues identified)\n\
     - Critical Issues: %d\n\
     - High Priority Issues: %d\n\n\
     [TARGET] KEY METRICS:\n\
     - Multiplicative Depth: %d\n\
     - Memory Usage: %.1f MB\n\
     - Proof Size: %d bytes\n\
     - Parallelizable Operations: %d\n\n\
     WARNING: IMMEDIATE ACTIONS REQUIRED:\n\
     %s\n\n\
     RECOMMENDATIONS:\n\
     %s"
    analysis.circuit_name
    (let tm = Unix.localtime analysis.analysis_timestamp in
     Printf.sprintf "%04d-%02d-%02d %02d:%02d" 
       (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min)
    complexity_rating analysis.complexity.total_constraints
    performance_rating analysis.performance.proving_time_ms
    security_rating security_issues
    (List.length critical_bottlenecks)
    (List.length high_bottlenecks)
    analysis.complexity.multiplicative_depth
    analysis.performance.memory_usage_mb
    analysis.performance.proof_size_bytes
    analysis.complexity.parallelizable_operations
    (if List.length critical_bottlenecks > 0 then
       String.concat "\n     - " ("" :: List.map (fun b -> b.description) critical_bottlenecks)
     else "     None")
    (String.concat "\n     - " ("" :: List.take (min 3 (List.length analysis.security.recommendations)) analysis.security.recommendations))

let generate_detailed_complexity_report (complexity: complexity_metrics) : string =
  let total = complexity.total_constraints in
  let percentage category_count = 
    if total = 0 then 0.0 
    else (float_of_int category_count /. float_of_int total) *. 100.0
  in
  
  Printf.sprintf
    "[ANALYSIS] DETAILED COMPLEXITY ANALYSIS\n\
     ==============================\n\n\
     CONSTRAINT BREAKDOWN:\n\
     ┌─────────────────────┬───────┬──────────┐\n\
     │ Type                │ Count │ Percent  │\n\
     ├─────────────────────┼───────┼──────────┤\n\
     │ Linear              │ %5d │ %6.1f%% │\n\
     │ Quadratic           │ %5d │ %6.1f%% │\n\
     │ Hash Functions      │ %5d │ %6.1f%% │\n\
     │ Boolean             │ %5d │ %6.1f%% │\n\
     │ Comparison          │ %5d │ %6.1f%% │\n\
     ├─────────────────────┼───────┼──────────┤\n\
     │ TOTAL               │ %5d │  100.0%% │\n\
     └─────────────────────┴───────┴──────────┘\n\n\
     CIRCUIT DEPTH ANALYSIS:\n\
     - Multiplicative Depth: %d levels\n\
     - Critical Path: %d operations\n\
     - Parallelizable Operations: %d\n\
     - Parallelization Potential: %.1f%%\n\n\
     COMPLEXITY METRICS:\n\
     - Constraint Density: %.3f\n\
     - Average Operations per Level: %.1f\n\
     - Bottleneck Factor: %.2f"
    complexity.linear_constraints (percentage complexity.linear_constraints)
    complexity.quadratic_constraints (percentage complexity.quadratic_constraints)
    complexity.hash_constraints (percentage complexity.hash_constraints)
    complexity.boolean_constraints (percentage complexity.boolean_constraints)
    complexity.comparison_constraints (percentage complexity.comparison_constraints)
    total
    complexity.multiplicative_depth
    complexity.critical_path_length
    complexity.parallelizable_operations
    (if total = 0 then 0.0 else (float_of_int complexity.parallelizable_operations /. float_of_int total) *. 100.0)
    (Constraint_analyzer.calculate_constraint_density { metadata = { num_variables = total * 2; num_constraints = total; num_inputs = 0; num_private = 0; num_outputs = 0 }; constraints = []; wires = []; input_wires = []; private_wires = []; output_wires = [] })
    (if complexity.multiplicative_depth = 0 then 0.0 else float_of_int total /. float_of_int complexity.multiplicative_depth)
    (if complexity.critical_path_length = 0 then 1.0 else float_of_int total /. float_of_int complexity.critical_path_length)

let generate_performance_report (performance: performance_estimate) : string =
  let total_time = performance.setup_time_ms +. performance.proving_time_ms +. performance.verification_time_ms in
  
  Printf.sprintf
    "[PERFORMANCE] PERFORMANCE ANALYSIS REPORT\n\
     =============================\n\n\
     TIMING BREAKDOWN:\n\
     ┌─────────────────────┬───────────┬──────────┐\n\
     │ Phase               │ Time (ms) │ Percent  │\n\
     ├─────────────────────┼───────────┼──────────┤\n\
     │ Setup               │ %9.1f │ %6.1f%% │\n\
     │ Proving             │ %9.1f │ %6.1f%% │\n\
     │ Verification        │ %9.1f │ %6.1f%% │\n\
     ├─────────────────────┼───────────┼──────────┤\n\
     │ TOTAL               │ %9.1f │  100.0%% │\n\
     └─────────────────────┴───────────┴──────────┘\n\n\
     RESOURCE UTILIZATION:\n\
     - Memory Usage: %.1f MB\n\
     - Proof Size: %d bytes (%.1f KB)\n\
     - Public Input Size: %d elements\n\
     - Private Witness Size: %d elements\n\n\
     PERFORMANCE BENCHMARKS:\n\
     - Throughput: %.1f proofs/second\n\
     - Memory Efficiency: %.2f constraints/MB\n\
     - Time Efficiency: %.2f constraints/second\n\n\
     SCALABILITY ANALYSIS:\n\
     - Linear Scaling Factor: %.2fx per 100 constraints\n\
     - Memory Growth Rate: %.1f MB per 1000 constraints\n\
     - Recommended Max Circuit Size: %d constraints"
    performance.setup_time_ms (performance.setup_time_ms /. total_time *. 100.0)
    performance.proving_time_ms (performance.proving_time_ms /. total_time *. 100.0)
    performance.verification_time_ms (performance.verification_time_ms /. total_time *. 100.0)
    total_time
    performance.memory_usage_mb
    performance.proof_size_bytes (float_of_int performance.proof_size_bytes /. 1024.0)
    performance.public_input_size
    performance.private_witness_size
    (if total_time = 0.0 then 0.0 else 1000.0 /. total_time)
    (if performance.memory_usage_mb = 0.0 then 0.0 else 100.0 /. performance.memory_usage_mb)
    (if performance.proving_time_ms = 0.0 then 0.0 else 1000.0 /. performance.proving_time_ms)
    (1.0 +. performance.proving_time_ms /. 5000.0)
    (performance.memory_usage_mb /. 10.0)
    (int_of_float (min 10000.0 (50000.0 /. (1.0 +. performance.proving_time_ms /. 100.0))))

let generate_security_report (security: security_analysis) : string =
  let format_security_level level = security_level_to_string level in
  
  Printf.sprintf
    "[SECURITY] SECURITY ANALYSIS REPORT\n\
     ==========================\n\n\
     SECURITY ASSESSMENT:\n\
     - Field Security: %s\n\
     - Soundness Level: %s\n\
     - Zero-Knowledge Level: %s\n\n\
     VULNERABILITY ASSESSMENT:\n\
     %s\n\n\
     SECURITY RECOMMENDATIONS:\n\
     %s\n\n\
     RISK MATRIX:\n\
     ┌─────────────────────┬──────────┬──────────────┐\n\
     │ Security Aspect     │ Level    │ Risk Status  │\n\
     ├─────────────────────┼──────────┼──────────────┤\n\
     │ Cryptographic       │ %8s │ %12s │\n\
     │ Protocol Soundness  │ %8s │ %12s │\n\
     │ Privacy Protection  │ %8s │ %12s │\n\
     └─────────────────────┴──────────┴──────────────┘"
    (format_security_level security.field_security)
    (format_security_level security.soundness_level)
    (format_security_level security.zero_knowledge_level)
    (if List.length security.potential_vulnerabilities = 0 then
       "SUCCESS: No significant vulnerabilities identified"
     else
       String.concat "\n     WARNING: " ("     Issues found:" :: security.potential_vulnerabilities))
    (String.concat "\n     RECOMMEND: " ("     Recommendations:" :: security.recommendations))
    (match security.field_security with VeryStrong _ | Strong _ -> "HIGH" | _ -> "MEDIUM")
    (match security.field_security with VeryStrong _ | Strong _ -> "LOW" | _ -> "ELEVATED")
    (match security.soundness_level with VeryStrong _ | Strong _ -> "HIGH" | _ -> "MEDIUM")  
    (match security.soundness_level with VeryStrong _ | Strong _ -> "LOW" | _ -> "ELEVATED")
    (match security.zero_knowledge_level with VeryStrong _ | Strong _ -> "HIGH" | _ -> "MEDIUM")
    (match security.zero_knowledge_level with VeryStrong _ | Strong _ -> "LOW" | _ -> "ELEVATED")

let generate_bottleneck_report (bottlenecks: bottleneck list) : string =
  let bottlenecks_by_severity = Circuit_analyzer.prioritize_bottlenecks bottlenecks in
  
  let format_bottleneck bottleneck =
    let severity_icon = match bottleneck.severity with
      | `Critical -> "[CRITICAL]"
      | `High -> "[HIGH]"
      | `Medium -> "[MEDIUM]"
      | `Low -> "[LOW]"
    in
    
    Printf.sprintf
      "%s %s SEVERITY: %s\n\
       Description: %s\n\
       Impact: %s\n\
       Suggested Optimizations:\n\
       %s"
      severity_icon
      (String.uppercase_ascii (bottleneck_severity_to_string bottleneck.severity))
      (match bottleneck.bottleneck_type with
        | ConstraintCount s -> "Constraint Count - " ^ s
        | MultDepth s -> "Multiplicative Depth - " ^ s  
        | HashOperations s -> "Hash Operations - " ^ s
        | MemoryUsage s -> "Memory Usage - " ^ s
        | CriticalPath s -> "Critical Path - " ^ s)
      bottleneck.description
      bottleneck.impact_estimate
      (String.concat "\n       - " ("" :: bottleneck.suggested_optimizations))
  in
  
  if List.length bottlenecks = 0 then
    "[SUCCESS] BOTTLENECK ANALYSIS\n\
     ====================\n\n\
     SUCCESS: No significant bottlenecks identified!\n\
     The circuit appears to be well-optimized for its complexity level."
  else
    Printf.sprintf
      "[ANALYSIS] BOTTLENECK ANALYSIS\n\
       ====================\n\n\
       Found %d performance/security bottlenecks:\n\n\
       %s"
      (List.length bottlenecks)
      (String.concat "\n\n" (List.map format_bottleneck bottlenecks_by_severity))

let generate_comprehensive_report (analysis: circuit_analysis) : string =
  String.concat "\n\n" [
    generate_executive_summary analysis;
    generate_detailed_complexity_report analysis.complexity;
    generate_performance_report analysis.performance;  
    generate_security_report analysis.security;
    generate_bottleneck_report analysis.bottlenecks;
    Printf.sprintf "[STATS] Analysis completed at %s with Camellia Analysis Engine v%s"
      (let tm = Unix.localtime analysis.analysis_timestamp in
       Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" 
         (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec)
      analysis.analysis_version;
  ]

let export_analysis_report (analysis: circuit_analysis) (filename: string) : unit Error.result =
  try
    let report = generate_comprehensive_report analysis in
    let oc = open_out filename in
    output_string oc report;
    close_out oc;
    Ok ()
  with
  | exn -> Error (circuit_error ("Failed to export analysis report: " ^ Printexc.to_string exn) ())

let export_analysis_csv (analyses: circuit_analysis list) (filename: string) : unit Error.result =
  try
    let oc = open_out filename in
    (* CSV header *)
    output_string oc "circuit_name,timestamp,total_constraints,linear_constraints,quadratic_constraints,hash_constraints,multiplicative_depth,proving_time_ms,memory_mb,vulnerabilities,bottlenecks\n";
    
    (* CSV data rows *)
    List.iter (fun analysis ->
      Printf.fprintf oc "%s,%f,%d,%d,%d,%d,%d,%f,%f,%d,%d\n"
        analysis.circuit_name
        analysis.analysis_timestamp
        analysis.complexity.total_constraints
        analysis.complexity.linear_constraints  
        analysis.complexity.quadratic_constraints
        analysis.complexity.hash_constraints
        analysis.complexity.multiplicative_depth
        analysis.performance.proving_time_ms
        analysis.performance.memory_usage_mb
        (List.length analysis.security.potential_vulnerabilities)
        (List.length analysis.bottlenecks)
    ) analyses;
    
    close_out oc;
    Ok ()
  with
  | exn -> Error (circuit_error ("Failed to export CSV: " ^ Printexc.to_string exn) ())

(* helper function to truncate lists for display *)
let rec take n lst =
  if n <= 0 then [] else
  match lst with
  | [] -> []
  | x :: xs -> x :: take (n-1) xs