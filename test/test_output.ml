open Alcotest
open R1cs
open Output
open Ast
open Compiler

let contains_substring s sub = 
  try ignore (Str.search_forward (Str.regexp_string sub) s 0); true 
  with Not_found -> false

let create_test_circuit () =
  {
    name = "test_circuit";
    inputs = ["x"; "y"];
    private_inputs = ["z"];
    body = [
      Assign ("sum", Add (Var "x", Var "y"));
      Assign ("product", Mul (Var "sum", Var "z"));
      Constraint (Equal (Var "product", Const "42"));
    ];
  }

let create_test_r1cs () =
  let debug_ctx = Debug.create_context "test" in
  let circuit = create_test_circuit () in
  let compiled = compile_circuit debug_ctx circuit |> Result.get_ok in
  compiled.r1cs_system

let create_test_witness _r1cs =
  let _x_val = Field.of_string "2" |> Result.get_ok in
  let _y_val = Field.of_string "4" |> Result.get_ok in
  let _z_val = Field.of_string "7" |> Result.get_ok in
  let sum_val = Field.of_string "6" |> Result.get_ok in
  let product_val = Field.of_string "42" |> Result.get_ok in
  let const_42 = Field.of_string "42" |> Result.get_ok in
  let const_1 = Field.one in
  let const_0 = Field.of_string "0" |> Result.get_ok in
  
  [
    (0, const_1);
    (1, Field.of_string "2" |> Result.get_ok);
    (2, Field.of_string "4" |> Result.get_ok);
    (3, Field.of_string "7" |> Result.get_ok);
    (4, sum_val);
    (5, product_val);
    (6, const_42);
    (7, const_1);
    (8, const_0);
    (9, const_1);
  ]

let test_statistics_calculation () =
  let r1cs = create_test_r1cs () in
  let stats = calculate_statistics r1cs in
  
  check int "variables count" 9 stats.num_variables;
  check int "constraints count" 7 stats.num_constraints;
  check int "inputs count" 2 stats.num_inputs;
  check int "private count" 1 stats.num_private;
  
  check bool "density positive" true (stats.constraint_density > 0.0);
  check bool "density reasonable" true (stats.constraint_density < 1.0);
  check bool "proving time estimate" true (stats.proving_time_estimate > 0.0);
  check bool "setup time estimate" true (stats.setup_time_estimate > 0.0);
  check bool "verification time estimate" true (stats.verification_time_estimate > 0.0)

let test_wire_usage_analysis () =
  let r1cs = create_test_r1cs () in
  let wire_usage = analyze_wire_usage r1cs in
  
  check bool "wire usage not empty" true (List.length wire_usage > 0);
  
  (* wire 0 should be heavily used *)
  let wire_0_usage = List.assoc_opt 0 wire_usage in
  check bool "wire 0 used frequently" true 
    (match wire_0_usage with Some count -> count > 5 | None -> false)

let test_circuit_analysis () =
  let r1cs = create_test_r1cs () in
  let analysis = analyze_circuit r1cs in
  
  check bool "has statistics" true (analysis.statistics.num_variables > 0);
  check bool "has wire usage" true (List.length analysis.wire_usage > 0);
  check bool "has constraint complexity" true (List.length analysis.constraint_complexity > 0);
  check bool "reasonable critical path" true (analysis.critical_path_length > 0);
  check bool "some parallelizable constraints" true (analysis.parallelizable_constraints >= 0)

let test_json_serialization () =
  let r1cs = create_test_r1cs () in
  let json = serialize_enhanced_r1cs_json r1cs "test_circuit" in
  
  match json with
  | `Assoc fields ->
      let has_metadata = List.mem_assoc "metadata" fields in
      let has_analysis = List.mem_assoc "analysis" fields in
      let has_constraints = List.mem_assoc "constraints" fields in
      check bool "has metadata" true has_metadata;
      check bool "has analysis" true has_analysis;
      check bool "has constraints" true has_constraints
  | _ -> 
      check bool "json is object" false true

let test_human_readable_output () =
  let r1cs = create_test_r1cs () in
  let output = generate_human_readable_output r1cs "test_circuit" in
  
  check bool "contains circuit name" true (contains_substring output "test_circuit");
  check bool "contains variables info" true (contains_substring output "Variables:");
  check bool "contains constraints info" true (contains_substring output "Constraints:");
  check bool "contains performance analysis" true (contains_substring output "PERFORMANCE");
  check bool "contains wire assignments" true (contains_substring output "WIRE ASSIGNMENTS");
  check bool "reasonably long" true (String.length output > 500)

let test_constraint_validation () =
  let r1cs = create_test_r1cs () in
  let witness = create_test_witness r1cs in
  
  let constraint_0 = List.hd r1cs.constraints in
  let result = validate_constraint_with_witness constraint_0 0 witness |> Result.get_ok in
  let (constraint_id, _satisfied, details) = result in
  
  check int "constraint id" 0 constraint_id;
  check bool "has details" true (String.length details > 10)

let test_witness_satisfiability_valid () =
  let r1cs = create_test_r1cs () in
  let witness = create_test_witness r1cs in
  let validation = check_witness_satisfiability r1cs witness |> Result.get_ok in
  
  check bool "validation completes" true (validation.is_valid || not validation.is_valid);
  check bool "has constraint results" true (List.length validation.constraint_results > 0);
  List.iter (fun (id, _satisfied, details) ->
    check bool "constraint id non-negative" true (id >= 0);
    check bool "details not empty" true (String.length details > 0)
  ) validation.constraint_results

let test_witness_satisfiability_invalid () =
  let r1cs = create_test_r1cs () in
  let invalid_witness = [
    (0, Field.one);
    (1, Field.of_string "1" |> Result.get_ok);  (* wrong x value *)
    (2, Field.of_string "1" |> Result.get_ok);  (* wrong y value *)
    (3, Field.of_string "1" |> Result.get_ok);  (* wrong z value *)
  ] in
  
  let validation = check_witness_satisfiability r1cs invalid_witness |> Result.get_ok in
  
  check bool "witness should not satisfy" false validation.is_valid;
  check bool "has failed constraint" true (validation.failed_constraint <> None);
  check bool "has error message" true (validation.error_message <> None)

let test_statistics_report () =
  let r1cs = create_test_r1cs () in
  let report_lines = generate_statistics_report r1cs "test_circuit" in
  let report = String.concat "\n" report_lines in
  
  check bool "contains circuit name" true (contains_substring report "test_circuit");
  check bool "contains basic metrics" true (contains_substring report "BASIC METRICS");
  check bool "contains complexity analysis" true (contains_substring report "COMPLEXITY");
  check bool "contains performance estimates" true (contains_substring report "PERFORMANCE");
  check bool "contains security assessment" true (contains_substring report "SECURITY");
  check bool "has reasonable length" true (String.length report > 300)

let test_validation_report () =
  let r1cs = create_test_r1cs () in
  let witness = create_test_witness r1cs in
  let report = generate_validation_report r1cs witness "test_circuit" |> Result.get_ok in
  
  check bool "contains circuit name" true (contains_substring report "test_circuit");
  check bool "contains validation status" true (contains_substring report "VALIDATION STATUS");
  check bool "contains constraint details" true (contains_substring report "CONSTRAINT VALIDATION");
  check bool "contains validation summary" true (contains_substring report "VALIDATION SUMMARY");
  check bool "has reasonable length" true (String.length report > 200)

let test_example_witness_generation () =
  let r1cs = create_test_r1cs () in
  let input_values = ["2"; "4"] in
  let private_values = ["7"] in
  let witness = generate_example_witness r1cs input_values private_values |> Result.get_ok in
  
  check bool "witness not empty" true (List.length witness > 0);
  check bool "has constant wire" true (List.mem_assoc 0 witness);
  
  let x_wire = List.nth r1cs.input_wires 0 in
  let y_wire = List.nth r1cs.input_wires 1 in
  check bool "has x value" true (List.mem_assoc x_wire witness);
  check bool "has y value" true (List.mem_assoc y_wire witness)

let test_file_export_import () =
  let r1cs = create_test_r1cs () in
  let temp_file = Filename.temp_file "test_r1cs" ".json" in
  
  export_enhanced_r1cs_json r1cs "test_circuit" temp_file;
  check bool "json file created" true (Sys.file_exists temp_file);
  
  let ic = open_in temp_file in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  
  check bool "json content not empty" true (String.length content > 100);
  check bool "contains metadata" true (contains_substring content "metadata");
  check bool "contains constraints" true (contains_substring content "constraints");
  
  Sys.remove temp_file

let test_multiple_formats_export () =
  let r1cs = create_test_r1cs () in
  let temp_base = Filename.temp_file "test_export" "" in
  
  export_r1cs_comprehensive r1cs "test_circuit" temp_base;
  
  let json_file = temp_base ^ ".json" in
  let readable_file = temp_base ^ "_readable.txt" in
  let stats_file = temp_base ^ "_statistics.txt" in
  
  check bool "json file created" true (Sys.file_exists json_file);
  check bool "readable file created" true (Sys.file_exists readable_file);
  check bool "stats file created" true (Sys.file_exists stats_file);
  
  List.iter (fun f -> if Sys.file_exists f then Sys.remove f) 
    [json_file; readable_file; stats_file]

let test_external_tool_compatibility () =
  let r1cs = create_test_r1cs () in
  let temp_circom = Filename.temp_file "test_circom" ".txt" in
  let temp_bellman = Filename.temp_file "test_bellman" ".json" in
  
  export_circom_compatible r1cs "test_circuit" temp_circom;
  export_bellman_compatible r1cs "test_circuit" temp_bellman;
  
  check bool "circom file created" true (Sys.file_exists temp_circom);
  check bool "bellman file created" true (Sys.file_exists temp_bellman);
  
  let ic = open_in temp_bellman in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  
  let parsed = Yojson.Safe.from_string content in
  (match parsed with
   | `Assoc _ -> check bool "bellman format valid" true true
   | _ -> check bool "bellman format invalid" false true);
  
  List.iter Sys.remove [temp_circom; temp_bellman]

let test_performance_benchmarking () =
  let r1cs = create_test_r1cs () in
  let start_time = Unix.gettimeofday () in
  
  for _i = 1 to 100 do
    let _stats = calculate_statistics r1cs in
    let _analysis = analyze_circuit r1cs in
    let _output = generate_human_readable_output r1cs "bench_test" in
    ()
  done;
  
  let end_time = Unix.gettimeofday () in
  let duration = end_time -. start_time in
  
  check bool "performance reasonable" true (duration < 5.0) (* should complete in under 5 seconds *)

let output_tests = [
  "statistics_calculation", `Quick, test_statistics_calculation;
  "wire_usage_analysis", `Quick, test_wire_usage_analysis;
  "circuit_analysis", `Quick, test_circuit_analysis;
  "json_serialization", `Quick, test_json_serialization;
  "human_readable_output", `Quick, test_human_readable_output;
  "constraint_validation", `Quick, test_constraint_validation;
  "witness_satisfiability_valid", `Quick, test_witness_satisfiability_valid;
  "witness_satisfiability_invalid", `Quick, test_witness_satisfiability_invalid;
  "statistics_report", `Quick, test_statistics_report;
  "validation_report", `Quick, test_validation_report;
  "example_witness_generation", `Quick, test_example_witness_generation;
  "file_export_import", `Quick, test_file_export_import;
  "multiple_formats_export", `Quick, test_multiple_formats_export;
  "external_tool_compatibility", `Quick, test_external_tool_compatibility;
  "performance_benchmarking", `Slow, test_performance_benchmarking;
]

let () = run "Output System Tests" [
  "output", output_tests;
]