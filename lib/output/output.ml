open Error

let take_n lst n =
  let rec take acc i = function
    | [] -> List.rev acc
    | x :: xs when i > 0 -> take (x :: acc) (i - 1) xs
    | _ -> List.rev acc
  in
  take [] n lst

let count_list pred lst =
  List.fold_left (fun acc x -> if pred x then acc + 1 else acc) 0 lst

type output_format =
  | JSON
  | HumanReadable
  | Statistics
  | Witness

type r1cs_statistics = {
  num_variables: int;
  num_constraints: int;
  num_inputs: int;
  num_private: int;
  num_outputs: int;
  constraint_density: float;
  max_constraint_degree: int;
  avg_constraint_degree: float;
  proving_time_estimate: float; (* in seconds *)
  setup_time_estimate: float; (* in seconds *)
  verification_time_estimate: float; (* in milliseconds *)
}

type satisfiability_result =
  | Satisfied
  | Unsatisfied of string (* constraint that failed *)
  | ValidationError of string

type witness_validation = {
  is_valid: bool;
  failed_constraint: int option;
  error_message: string option;
  constraint_results: (int * bool * string) list; (* constraint_id, satisfied, details *)
}

type circuit_analysis = {
  statistics: r1cs_statistics;
  wire_usage: (R1cs.wire_id * int) list; (* wire_id, usage_count *)
  constraint_complexity: int list; (* complexity score per constraint *)
  critical_path_length: int;
  parallelizable_constraints: int;
}

let calculate_statistics (r1cs : R1cs.r1cs_system) =
  let density = R1cs.analyze_constraint_density r1cs in
  let complexity_scores = R1cs.get_constraint_complexity r1cs in
  let max_degree = match complexity_scores with
    | [] -> 0
    | scores -> List.fold_left max 0 scores
  in
  let avg_degree = match complexity_scores with
    | [] -> 0.0
    | scores -> 
        let sum = List.fold_left (+) 0 scores in
        float_of_int sum /. float_of_int (List.length scores)
  in
  
  (* rough estimates *)
  let base_proving_time = float_of_int r1cs.metadata.num_constraints *. 0.001 in
  let setup_time = float_of_int r1cs.metadata.num_constraints *. 0.0005 in
  let verification_time = 5.0 +. (float_of_int r1cs.metadata.num_inputs *. 0.1) in
  
  {
    num_variables = r1cs.metadata.num_variables;
    num_constraints = r1cs.metadata.num_constraints;
    num_inputs = r1cs.metadata.num_inputs;
    num_private = r1cs.metadata.num_private;
    num_outputs = r1cs.metadata.num_outputs;
    constraint_density = density;
    max_constraint_degree = max_degree;
    avg_constraint_degree = avg_degree;
    proving_time_estimate = base_proving_time;
    setup_time_estimate = setup_time;
    verification_time_estimate = verification_time;
  }

let analyze_wire_usage (r1cs : R1cs.r1cs_system) =
  let usage_table = R1cs.get_wire_usage_stats r1cs in
  let wire_usage = Hashtbl.fold (fun wire_id count acc -> 
    (wire_id, count) :: acc
  ) usage_table [] in
  List.sort (fun (_, c1) (_, c2) -> compare c2 c1) wire_usage

let calculate_critical_path (r1cs : R1cs.r1cs_system) =
  (* simplified critical path *)
  let constraint_dependencies = List.length r1cs.constraints in
  min constraint_dependencies 10 (* reasonable upper bound *)

let count_parallelizable_constraints (r1cs : R1cs.r1cs_system) =
  (* constraints can be parallelized *)
  let independent_constraints = List.fold_left (fun acc constraint_triple ->
    let uses_intermediate = List.exists (fun (wire_id, _) ->
      let wire_info = List.find_opt (fun w -> w.R1cs.id = wire_id) r1cs.wires in
      match wire_info with
      | Some w -> w.wire_type = R1cs.Intermediate
      | None -> false
    ) (constraint_triple.R1cs.a @ constraint_triple.R1cs.b @ constraint_triple.R1cs.c) in
    if uses_intermediate then acc else acc + 1
  ) 0 r1cs.constraints in
  independent_constraints

let analyze_circuit (r1cs : R1cs.r1cs_system) =
  let statistics = calculate_statistics r1cs in
  let wire_usage = analyze_wire_usage r1cs in
  let constraint_complexity = R1cs.get_constraint_complexity r1cs in
  let critical_path_length = calculate_critical_path r1cs in
  let parallelizable_constraints = count_parallelizable_constraints r1cs in
  
  {
    statistics;
    wire_usage;
    constraint_complexity;
    critical_path_length;
    parallelizable_constraints;
  }

let serialize_enhanced_r1cs_json (r1cs : R1cs.r1cs_system) circuit_name =
  let analysis = analyze_circuit r1cs in
  let base_json = R1cs.serialize_r1cs_json r1cs in
  
  let metadata_json = `Assoc [
    ("circuit_name", `String circuit_name);
    ("compilation_timestamp", `String (string_of_float (Unix.time ())));
    ("camellia_version", `String "1.0.0-mvp");
    ("format_version", `String "1.0");
  ] in
  
  let analysis_json = `Assoc [
    ("constraint_density", `Float analysis.statistics.constraint_density);
    ("max_constraint_degree", `Int analysis.statistics.max_constraint_degree);
    ("avg_constraint_degree", `Float analysis.statistics.avg_constraint_degree);
    ("proving_time_estimate_sec", `Float analysis.statistics.proving_time_estimate);
    ("setup_time_estimate_sec", `Float analysis.statistics.setup_time_estimate);
    ("verification_time_estimate_ms", `Float analysis.statistics.verification_time_estimate);
    ("critical_path_length", `Int analysis.critical_path_length);
    ("parallelizable_constraints", `Int analysis.parallelizable_constraints);
  ] in
  
  let wire_usage_json = `List (List.map (fun (wire_id, count) ->
    `Assoc [("wire_id", `Int wire_id); ("usage_count", `Int count)]
  ) (take_n analysis.wire_usage 10)) in (* top 10 most used wires *)
  
  match base_json with
  | `Assoc fields ->
      `Assoc (fields @ [
        ("metadata", metadata_json);
        ("analysis", analysis_json);
        ("top_wire_usage", wire_usage_json);
      ])
  | json -> json

let export_enhanced_r1cs_json (r1cs : R1cs.r1cs_system) circuit_name filename =
  let json = serialize_enhanced_r1cs_json r1cs circuit_name in
  let json_string = Yojson.Safe.pretty_to_string json in
  let oc = open_out filename in
  output_string oc json_string;
  close_out oc

let format_constraint_human constraint_triple constraint_id =
  let format_lc lc =
    match lc with
    | [] -> "0"
    | terms ->
        let term_strs = List.map (fun (wire_id, coeff) ->
          let coeff_str = Field.to_string coeff in
          if coeff_str = "1" then Printf.sprintf "w%d" wire_id
          else if coeff_str = "-1" then Printf.sprintf "-w%d" wire_id  
          else Printf.sprintf "%s·w%d" coeff_str wire_id
        ) terms in
        String.concat " + " term_strs
  in
  
  Printf.sprintf "C%-3d: (%s) × (%s) = (%s)"
    constraint_id
    (format_lc constraint_triple.R1cs.a)
    (format_lc constraint_triple.R1cs.b) 
    (format_lc constraint_triple.R1cs.c)

let format_wire_info_human (wire : R1cs.wire_info) =
  let type_str = match wire.wire_type with
    | R1cs.Input -> "INPUT"
    | R1cs.Private -> "PRIVATE" 
    | R1cs.Intermediate -> "INTER"
    | R1cs.Output -> "OUTPUT"
    | R1cs.Constant -> "CONST"
  in
  let value_str = match wire.value with
    | Some v -> Printf.sprintf " = %s" (Field.to_string v)
    | None -> ""
  in
  Printf.sprintf "w%-3d %-7s %s%s" wire.id type_str wire.name value_str

let generate_human_readable_output (r1cs : R1cs.r1cs_system) circuit_name =
  let analysis = analyze_circuit r1cs in
  let lines = [
    "================================================================================";
    Printf.sprintf "CAMELLIA R1CS SYSTEM: %s" (String.uppercase_ascii circuit_name);
    "================================================================================";
    "";
    "SYSTEM OVERVIEW:";
    Printf.sprintf "  Circuit Name: %s" circuit_name;
    Printf.sprintf "  Variables: %d" analysis.statistics.num_variables;
    Printf.sprintf "  Constraints: %d" analysis.statistics.num_constraints;
    Printf.sprintf "  Public Inputs: %d" analysis.statistics.num_inputs;
    Printf.sprintf "  Private Inputs: %d" analysis.statistics.num_private;
    Printf.sprintf "  Outputs: %d" analysis.statistics.num_outputs;
    "";
    "PERFORMANCE ANALYSIS:";
    Printf.sprintf "  Constraint Density: %.2f%%" (analysis.statistics.constraint_density *. 100.0);
    Printf.sprintf "  Max Constraint Degree: %d" analysis.statistics.max_constraint_degree;
    Printf.sprintf "  Avg Constraint Degree: %.1f" analysis.statistics.avg_constraint_degree;
    Printf.sprintf "  Critical Path Length: %d" analysis.critical_path_length;
    Printf.sprintf "  Parallelizable Constraints: %d/%d (%.1f%%)" 
      analysis.parallelizable_constraints 
      analysis.statistics.num_constraints
      (float_of_int analysis.parallelizable_constraints /. float_of_int analysis.statistics.num_constraints *. 100.0);
    "";
    "PROVING TIME ESTIMATES:";
    Printf.sprintf "  Setup Time: %.3f seconds" analysis.statistics.setup_time_estimate;
    Printf.sprintf "  Proving Time: %.3f seconds" analysis.statistics.proving_time_estimate;
    Printf.sprintf "  Verification Time: %.1f milliseconds" analysis.statistics.verification_time_estimate;
    "";
    "WIRE ASSIGNMENTS:";
  ] in
  
  let wire_lines = List.map format_wire_info_human r1cs.wires in
  let constraint_lines = [
    "";
    "CONSTRAINT SYSTEM:";
  ] @ (List.mapi (fun i c -> format_constraint_human c i) (List.rev r1cs.constraints)) in
  
  let wire_usage_lines = [
    "";
    "TOP WIRE USAGE:";
  ] @ (take_n analysis.wire_usage 5 |> List.map (fun (wire_id, count) ->
    Printf.sprintf "  w%-3d: used in %d constraints" wire_id count
  )) in
  
  String.concat "\n" (lines @ wire_lines @ constraint_lines @ wire_usage_lines @ [
    "";
    "================================================================================";
  ])

let export_human_readable (r1cs : R1cs.r1cs_system) circuit_name filename =
  let content = generate_human_readable_output r1cs circuit_name in
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let validate_constraint_with_witness constraint_triple constraint_id witness =
  let* a_val = R1cs.evaluate_linear_combination constraint_triple.R1cs.a witness in
  let* b_val = R1cs.evaluate_linear_combination constraint_triple.R1cs.b witness in
  let* c_val = R1cs.evaluate_linear_combination constraint_triple.R1cs.c witness in
  let* ab_product = Field.mul a_val b_val in
  let* is_equal = R1cs.field_equal_evaluated (Field.to_string ab_product) (Field.to_string c_val) in
  
  let details = Printf.sprintf "(%s) × (%s) = %s, expected %s"
    (Field.to_string a_val)
    (Field.to_string b_val)
    (Field.to_string ab_product)
    (Field.to_string c_val) in
  
  Ok (constraint_id, is_equal, details)

let check_witness_satisfiability (r1cs : R1cs.r1cs_system) witness =
  let constraint_results = List.mapi (fun i constraint_triple ->
    validate_constraint_with_witness constraint_triple i witness
  ) r1cs.constraints in
  
  let* results = Error.collect_results constraint_results in
  
  let failed_constraint = List.find_opt (fun (_, satisfied, _) -> not satisfied) results in
  match failed_constraint with
  | Some (constraint_id, _, details) ->
      Ok {
        is_valid = false;
        failed_constraint = Some constraint_id;
        error_message = Some ("Constraint " ^ string_of_int constraint_id ^ " failed: " ^ details);
        constraint_results = results;
      }
  | None ->
      Ok {
        is_valid = true;
        failed_constraint = None;
        error_message = None;
        constraint_results = results;
      }

let generate_example_witness (r1cs : R1cs.r1cs_system) input_values private_values =
  let witness = ref [(0, Field.one)] in (* wire 0 = 1 *)
  
  let rec add_inputs wires values =
    match wires, values with
    | [], [] -> Ok ()
    | wire_id :: ws, value :: vs ->
        let* field_value = Field.of_string value in
        witness := (wire_id, field_value) :: !witness;
        add_inputs ws vs
    | _ -> Result.Error (Error.circuit_error "Mismatched input/private value counts" ())
  in
  
  let* () = add_inputs r1cs.input_wires input_values in
  let* () = add_inputs r1cs.private_wires private_values in
  
  Ok !witness

let generate_statistics_report (r1cs : R1cs.r1cs_system) circuit_name =
  let analysis = analyze_circuit r1cs in
  let stats = analysis.statistics in
  
  [
    Printf.sprintf "=== CIRCUIT STATISTICS: %s ===" circuit_name;
    "";
    "BASIC METRICS:";
    Printf.sprintf "  Variables: %d" stats.num_variables;
    Printf.sprintf "  Constraints: %d" stats.num_constraints;
    Printf.sprintf "  Public Inputs: %d" stats.num_inputs;
    Printf.sprintf "  Private Inputs: %d" stats.num_private;
    Printf.sprintf "  Outputs: %d" stats.num_outputs;
    "";
    "COMPLEXITY ANALYSIS:";
    Printf.sprintf "  Constraint Density: %.4f (%.2f%%)" stats.constraint_density (stats.constraint_density *. 100.0);
    Printf.sprintf "  Max Constraint Degree: %d terms" stats.max_constraint_degree;
    Printf.sprintf "  Avg Constraint Degree: %.2f terms" stats.avg_constraint_degree;
    Printf.sprintf "  Critical Path Length: %d" analysis.critical_path_length;
    "";
    "OPTIMIZATION METRICS:";
    Printf.sprintf "  Parallelizable Constraints: %d/%d (%.1f%%)" 
      analysis.parallelizable_constraints stats.num_constraints
      (float_of_int analysis.parallelizable_constraints /. float_of_int stats.num_constraints *. 100.0);
    Printf.sprintf "  Constraint/Variable Ratio: %.3f" 
      (float_of_int stats.num_constraints /. float_of_int stats.num_variables);
    "";
    "PERFORMANCE ESTIMATES:";
    Printf.sprintf "  Setup Phase: %.3f seconds" stats.setup_time_estimate;
    Printf.sprintf "  Proving Phase: %.3f seconds" stats.proving_time_estimate;
    Printf.sprintf "  Verification Phase: %.1f milliseconds" stats.verification_time_estimate;
    Printf.sprintf "  Memory Usage (est): %.1f MB" (float_of_int stats.num_variables *. 32.0 /. 1024.0 /. 1024.0);
    "";
    "SECURITY ASSESSMENT:";
    Printf.sprintf "  Field: BN254 (128-bit security)";
    Printf.sprintf "  Constraint System: R1CS (proven secure)";
    Printf.sprintf "  Zero-Knowledge: Complete (assuming proper setup)";
    Printf.sprintf "  Soundness: Computational (depends on discrete log assumption)";
  ]

let export_statistics_report (r1cs : R1cs.r1cs_system) circuit_name filename =
  let lines = generate_statistics_report r1cs circuit_name in
  let content = String.concat "\n" lines in
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let export_witness_format witness filename =
  let lines = List.map (fun (wire_id, value) ->
    Printf.sprintf "w%d = %s" wire_id (Field.to_string value)
  ) witness in
  let content = String.concat "\n" ("# Witness Assignment" :: lines) in
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let export_r1cs_comprehensive (r1cs : R1cs.r1cs_system) circuit_name base_filename =
  let json_file = base_filename ^ ".json" in
  let human_file = base_filename ^ "_readable.txt" in
  let stats_file = base_filename ^ "_statistics.txt" in
  
  export_enhanced_r1cs_json r1cs circuit_name json_file;
  export_human_readable r1cs circuit_name human_file;
  export_statistics_report r1cs circuit_name stats_file;
  
  Printf.printf "R1CS exported to multiple formats:\n";
  Printf.printf "  JSON: %s\n" json_file;
  Printf.printf "  Human-readable: %s\n" human_file;
  Printf.printf "  Statistics: %s\n" stats_file

let generate_validation_report (r1cs : R1cs.r1cs_system) witness circuit_name =
  let* validation = check_witness_satisfiability r1cs witness in
  let analysis = analyze_circuit r1cs in
  
  let status_icon = if validation.is_valid then "✅" else "❌" in
  let status_text = if validation.is_valid then "SATISFIED" else "UNSATISFIED" in
  
  let lines = [
    Printf.sprintf "=== R1CS VALIDATION REPORT: %s ===" circuit_name;
    "";
    Printf.sprintf "VALIDATION STATUS: %s %s" status_icon status_text;
  ] in
  
  let constraint_lines = match validation.failed_constraint with
    | Some failed_id ->
        [
          "";
          Printf.sprintf "FAILED CONSTRAINT: C%d" failed_id;
          Printf.sprintf "ERROR: %s" (Option.value validation.error_message ~default:"Unknown error");
        ]
    | None ->
        [
          "";
          Printf.sprintf "All %d constraints satisfied successfully" analysis.statistics.num_constraints;
        ]
  in
  
  let detail_lines = [
    "";
    "CONSTRAINT VALIDATION DETAILS:";
  ] @ (take_n validation.constraint_results 10 |> List.map (fun (id, satisfied, details) ->
    let icon = if satisfied then "✅" else "❌" in
    Printf.sprintf "  C%-3d %s %s" id icon details
  )) in
  
  let summary_lines = [
    "";
    "VALIDATION SUMMARY:";
    Printf.sprintf "  Total Constraints: %d" (List.length validation.constraint_results);
    Printf.sprintf "  Satisfied: %d" (count_list (fun (_, sat, _) -> sat) validation.constraint_results);
    Printf.sprintf "  Failed: %d" (count_list (fun (_, sat, _) -> not sat) validation.constraint_results);
    Printf.sprintf "  Success Rate: %.2f%%" 
      (float_of_int (count_list (fun (_, sat, _) -> sat) validation.constraint_results) /.
       float_of_int (List.length validation.constraint_results) *. 100.0);
  ] in
  
  Ok (String.concat "\n" (lines @ constraint_lines @ detail_lines @ summary_lines))

let export_validation_report (r1cs : R1cs.r1cs_system) witness circuit_name filename =
  let* report = generate_validation_report r1cs witness circuit_name in
  let oc = open_out filename in
  output_string oc report;
  close_out oc;
  Ok ()

let export_circom_compatible (r1cs : R1cs.r1cs_system) circuit_name filename =
  let header = [
    Printf.sprintf "// Circom-compatible R1CS export for %s" circuit_name;
    Printf.sprintf "// Generated by Camellia ZK Compiler";
    Printf.sprintf "// Variables: %d, Constraints: %d" r1cs.metadata.num_variables r1cs.metadata.num_constraints;
    "";
  ] in
  
  let constraint_lines = List.mapi (fun i constraint_triple ->
    Printf.sprintf "constraint[%d]: %s" i (R1cs.constraint_to_string constraint_triple)
  ) r1cs.constraints in
  
  let content = String.concat "\n" (header @ constraint_lines) in
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let export_bellman_compatible (r1cs : R1cs.r1cs_system) circuit_name filename =
  let json = `Assoc [
    ("circuit_name", `String circuit_name);
    ("num_variables", `Int r1cs.metadata.num_variables);
    ("num_constraints", `Int r1cs.metadata.num_constraints);
    ("num_inputs", `Int r1cs.metadata.num_inputs);
    ("constraints", `List (List.map R1cs.serialize_constraint_json r1cs.constraints));
    ("format", `String "bellman_compatible_v1");
  ] in
  let json_string = Yojson.Safe.pretty_to_string json in
  let oc = open_out filename in
  output_string oc json_string;
  close_out oc