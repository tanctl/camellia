(* analysis types for circuit evaluation *)

(* constraint categorization *)
type constraint_category = 
  | Linear      (* addition, subtraction *)
  | Quadratic   (* multiplication *)
  | Hash        (* poseidon and other hash functions *)
  | Boolean     (* boolean operations *)
  | Comparison  (* equality, range checks *)

(* circuit complexity metrics *)
type complexity_metrics = {
  total_constraints: int;
  linear_constraints: int;
  quadratic_constraints: int;
  hash_constraints: int;
  boolean_constraints: int;
  comparison_constraints: int;
  multiplicative_depth: int;
  critical_path_length: int;
  parallelizable_operations: int;
}

(* performance estimation *)
type performance_estimate = {
  setup_time_ms: float;
  proving_time_ms: float;
  verification_time_ms: float;
  memory_usage_mb: float;
  proof_size_bytes: int;
  public_input_size: int;
  private_witness_size: int;
}

(* security analysis *)
type security_level = 
  | Weak of string        (* < 80 bits *)
  | Moderate of string    (* 80-112 bits *)
  | Strong of string      (* 112-128 bits *)
  | VeryStrong of string  (* > 128 bits *)

type security_analysis = {
  field_security: security_level;
  soundness_level: security_level;
  zero_knowledge_level: security_level;
  potential_vulnerabilities: string list;
  recommendations: string list;
}

(* bottleneck identification *)
type bottleneck_type =
  | ConstraintCount of string
  | MultDepth of string  
  | HashOperations of string
  | MemoryUsage of string
  | CriticalPath of string

type bottleneck = {
  bottleneck_type: bottleneck_type;
  severity: [`Low | `Medium | `High | `Critical];
  description: string;
  impact_estimate: string;
  suggested_optimizations: string list;
}

(* comprehensive circuit analysis *)
type circuit_analysis = {
  circuit_name: string;
  complexity: complexity_metrics;
  performance: performance_estimate;
  security: security_analysis;
  bottlenecks: bottleneck list;
  analysis_timestamp: float;
  analysis_version: string;
}

(* analysis configuration *)
type analysis_config = {
  target_security_bits: int;
  performance_target_ms: float;
  memory_limit_mb: float;
  enable_deep_analysis: bool;
  include_optimizations: bool;
}

let default_analysis_config = {
  target_security_bits = 128;
  performance_target_ms = 1000.0;
  memory_limit_mb = 512.0;
  enable_deep_analysis = true;
  include_optimizations = true;
}

let constraint_category_to_string = function
  | Linear -> "linear"
  | Quadratic -> "quadratic" 
  | Hash -> "hash"
  | Boolean -> "boolean"
  | Comparison -> "comparison"

let security_level_to_string = function
  | Weak s -> Printf.sprintf "weak (%s)" s
  | Moderate s -> Printf.sprintf "moderate (%s)" s
  | Strong s -> Printf.sprintf "strong (%s)" s
  | VeryStrong s -> Printf.sprintf "very-strong (%s)" s

let bottleneck_severity_to_string = function
  | `Low -> "low"
  | `Medium -> "medium"
  | `High -> "high"
  | `Critical -> "critical"