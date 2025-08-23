open Analysis_types

(* performance modeling constants based on empirical ZK-SNARK data *)
module PerfConstants = struct
  (* base times in milliseconds *)
  let base_setup_time = 50.0
  let base_proving_time = 100.0  
  let base_verification_time = 15.0
  
  (* scaling factors per constraint type *)
  let linear_constraint_factor = 0.1
  let quadratic_constraint_factor = 0.8
  let hash_constraint_factor = 5.0
  let comparison_constraint_factor = 0.3
  
  (* multiplicative depth impact *)
  let mult_depth_exponential_base = 1.4
  
  (* memory estimates (MB per constraint type) *)
  let base_memory_mb = 10.0
  let memory_per_linear = 0.02
  let memory_per_quadratic = 0.1
  let memory_per_hash = 0.5
  let memory_per_variable = 0.05
  
  (* proof size estimates (bytes) *)
  let base_proof_size = 256  (* typical SNARK proof size *)
  let proof_size_per_public_input = 32
end

(* estimate setup phase performance *)
let estimate_setup_time (metrics: complexity_metrics) (circuit_vars: int) : float =
  let constraint_overhead = 
    float_of_int metrics.linear_constraints *. PerfConstants.linear_constraint_factor +.
    float_of_int metrics.quadratic_constraints *. PerfConstants.quadratic_constraint_factor +.
    float_of_int metrics.hash_constraints *. PerfConstants.hash_constraint_factor +.
    float_of_int metrics.comparison_constraints *. PerfConstants.comparison_constraint_factor
  in
  
  let variable_overhead = float_of_int circuit_vars *. 0.02 in
  let depth_penalty = 
    PerfConstants.mult_depth_exponential_base ** (float_of_int metrics.multiplicative_depth) in
    
  (PerfConstants.base_setup_time +. constraint_overhead +. variable_overhead) *. depth_penalty

(* estimate proving phase performance *)  
let estimate_proving_time (metrics: complexity_metrics) (_circuit_vars: int) : float =
  let base_time = PerfConstants.base_proving_time in
  
  (* constraint complexity impact *)
  let constraint_time = 
    float_of_int metrics.linear_constraints *. PerfConstants.linear_constraint_factor +.
    float_of_int metrics.quadratic_constraints *. PerfConstants.quadratic_constraint_factor +.
    float_of_int metrics.hash_constraints *. PerfConstants.hash_constraint_factor +.
    float_of_int metrics.comparison_constraints *. PerfConstants.comparison_constraint_factor
  in
  
  (* multiplicative depth has exponential impact on proving time *)
  let depth_multiplier = 
    if metrics.multiplicative_depth <= 3 then 1.0
    else PerfConstants.mult_depth_exponential_base ** (float_of_int (metrics.multiplicative_depth - 3))
  in
  
  (* parallelization can reduce time *)
  let parallelization_benefit = 
    if metrics.parallelizable_operations > 0 then 
      1.0 -. (0.1 *. min 5.0 (float_of_int metrics.parallelizable_operations /. 10.0))
    else 1.0
  in
  
  (base_time +. constraint_time) *. depth_multiplier *. parallelization_benefit

(* estimate verification phase performance *)
let estimate_verification_time (num_public_inputs: int) : float =
  PerfConstants.base_verification_time +. 
  (float_of_int num_public_inputs *. 0.5)

(* estimate memory usage *)
let estimate_memory_usage (metrics: complexity_metrics) (circuit_vars: int) : float =
  PerfConstants.base_memory_mb +.
  float_of_int metrics.linear_constraints *. PerfConstants.memory_per_linear +.
  float_of_int metrics.quadratic_constraints *. PerfConstants.memory_per_quadratic +.
  float_of_int metrics.hash_constraints *. PerfConstants.memory_per_hash +.
  float_of_int circuit_vars *. PerfConstants.memory_per_variable

(* estimate proof size *)
let estimate_proof_size (num_public_inputs: int) : int =
  PerfConstants.base_proof_size + 
  (num_public_inputs * PerfConstants.proof_size_per_public_input)

(* comprehensive performance analysis *)
let analyze_performance 
  (metrics: complexity_metrics) 
  (num_public_inputs: int)
  (num_private_inputs: int)
  (circuit_vars: int) : performance_estimate =
  
  let setup_time = estimate_setup_time metrics circuit_vars in
  let proving_time = estimate_proving_time metrics circuit_vars in
  let verification_time = estimate_verification_time num_public_inputs in
  let memory_usage = estimate_memory_usage metrics circuit_vars in
  let proof_size = estimate_proof_size num_public_inputs in
  let witness_size = num_public_inputs + num_private_inputs in
  
  {
    setup_time_ms = setup_time;
    proving_time_ms = proving_time;
    verification_time_ms = verification_time;
    memory_usage_mb = memory_usage;
    proof_size_bytes = proof_size;
    public_input_size = num_public_inputs;
    private_witness_size = witness_size;
  }

(* performance benchmarking and comparison *)
let benchmark_against_targets (perf: performance_estimate) (config: analysis_config) : 
  [`Excellent | `Good | `Acceptable | `Poor] * string list =
  
  let issues = ref [] in
  let overall_score = ref 0 in
  
  (* evaluate setup time *)
  if perf.setup_time_ms > config.performance_target_ms then (
    issues := "Setup time exceeds target" :: !issues;
    decr overall_score
  ) else incr overall_score;
  
  (* evaluate proving time *) 
  if perf.proving_time_ms > config.performance_target_ms then (
    issues := "Proving time exceeds target" :: !issues;
    decr overall_score
  ) else incr overall_score;
  
  (* evaluate memory usage *)
  if perf.memory_usage_mb > config.memory_limit_mb then (
    issues := "Memory usage exceeds limit" :: !issues;
    decr overall_score
  ) else incr overall_score;
  
  (* evaluate verification time (should be fast) *)
  if perf.verification_time_ms > 100.0 then (
    issues := "Verification time is high" :: !issues;
    decr overall_score  
  ) else incr overall_score;
  
  let rating = match !overall_score with
    | 4 -> `Excellent
    | 3 -> `Good
    | 2 -> `Acceptable
    | _ -> `Poor
  in
  
  (rating, !issues)

(* identify performance bottlenecks *)
let identify_performance_bottlenecks (perf: performance_estimate) (metrics: complexity_metrics) : bottleneck list =
  let bottlenecks = ref [] in
  
  (* proving time bottleneck *)
  if perf.proving_time_ms > 5000.0 then
    bottlenecks := {
      bottleneck_type = ConstraintCount "High proving time";
      severity = `High;
      description = Printf.sprintf "Proving time of %.1fms is very high" perf.proving_time_ms;
      impact_estimate = "Will significantly impact user experience";
      suggested_optimizations = [
        "Reduce number of constraints";
        "Optimize multiplicative depth";
        "Consider constraint batching";
        "Evaluate alternative proving systems"
      ];
    } :: !bottlenecks;
    
  (* memory usage bottleneck *)  
  if perf.memory_usage_mb > 1000.0 then
    bottlenecks := {
      bottleneck_type = MemoryUsage "High memory consumption";
      severity = `Medium;
      description = Printf.sprintf "Memory usage of %.1fMB is high" perf.memory_usage_mb;
      impact_estimate = "May cause issues on resource-constrained systems";
      suggested_optimizations = [
        "Implement streaming witness generation";
        "Use memory-efficient data structures";
        "Consider constraint system optimizations"
      ];
    } :: !bottlenecks;
    
  (* critical path bottleneck *)
  if metrics.critical_path_length > 50 then
    bottlenecks := {
      bottleneck_type = CriticalPath "Long critical path";
      severity = `Medium; 
      description = Printf.sprintf "Critical path length of %d may limit parallelization" metrics.critical_path_length;
      impact_estimate = "Reduces potential for parallel proving optimizations";
      suggested_optimizations = [
        "Restructure circuit for better parallelism";
        "Break long dependency chains";
        "Consider alternative circuit architectures"
      ];
    } :: !bottlenecks;
    
  !bottlenecks

let performance_analysis_summary (perf: performance_estimate) : string =
  Printf.sprintf
    "Performance Analysis:\n\
     - Setup Time: %.1f ms\n\
     - Proving Time: %.1f ms\n\
     - Verification Time: %.1f ms\n\
     - Memory Usage: %.1f MB\n\
     - Proof Size: %d bytes\n\
     - Public Input Size: %d\n\
     - Private Witness Size: %d\n\
     - Estimated Total Time: %.1f ms"
    perf.setup_time_ms
    perf.proving_time_ms
    perf.verification_time_ms
    perf.memory_usage_mb
    perf.proof_size_bytes
    perf.public_input_size
    perf.private_witness_size
    (perf.setup_time_ms +. perf.proving_time_ms +. perf.verification_time_ms)