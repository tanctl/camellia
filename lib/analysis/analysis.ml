(* main analysis module - public interface *)

module Types = Analysis_types
module Constraint = Constraint_analyzer  
module Depth = Depth_analyzer
module Performance = Performance_analyzer
module Security = Security_analyzer
module Analyzer = Circuit_analyzer
module Reporter = Analysis_reporter

(* re-export key types *)
type circuit_analysis = Analysis_types.circuit_analysis
type complexity_metrics = Analysis_types.complexity_metrics
type performance_estimate = Analysis_types.performance_estimate
type security_analysis = Analysis_types.security_analysis
type bottleneck = Analysis_types.bottleneck
type analysis_config = Analysis_types.analysis_config

(* re-export key functions *)
let analyze_circuit = Circuit_analyzer.analyze_circuit
let quick_analyze = Circuit_analyzer.quick_analyze
let analyze_with_config = Circuit_analyzer.analyze_with_config

let generate_report = Analysis_reporter.generate_comprehensive_report
let export_report = Analysis_reporter.export_analysis_report
let export_json = Circuit_analyzer.export_analysis_json

let default_config = Analysis_types.default_analysis_config

(* convenience functions *)
let analyze_and_report 
  ?(config = default_config)
  (circuit: Ast.circuit) 
  (r1cs: R1cs.r1cs_system) : string Error.result =
  
  let open Error in
  let* analysis = analyze_circuit ~config circuit r1cs in
  Ok (generate_report analysis)

let analyze_and_export
  ?(config = default_config)
  (circuit: Ast.circuit) 
  (r1cs: R1cs.r1cs_system)
  (filename: string) : unit Error.result =
  
  let open Error in
  let* analysis = analyze_circuit ~config circuit r1cs in
  export_report analysis filename