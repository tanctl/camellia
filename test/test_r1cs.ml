open R1cs

let test_wire_manager_creation () =
  let manager = create_wire_manager () in
  Alcotest.(check int) "initial next_id" 1 manager.next_id;
  Alcotest.(check int) "empty wires table" 0 (Hashtbl.length manager.wires);
  Alcotest.(check int) "empty name table" 0 (Hashtbl.length manager.name_to_id)

let test_wire_allocation () =
  let manager = create_wire_manager () in
  
  let result1 = allocate_wire manager "input_a" Input () in
  Alcotest.(check bool) "wire allocation success" true (Result.is_ok result1);
  
  let result2 = allocate_wire manager "private_b" Private () in
  Alcotest.(check bool) "second wire allocation success" true (Result.is_ok result2);
  
  let result3 = allocate_wire manager "input_a" Input () in
  Alcotest.(check bool) "duplicate name fails" true (Result.is_error result3);
  
  Alcotest.(check int) "next_id incremented" 3 manager.next_id;
  Alcotest.(check int) "two wires allocated" 2 (Hashtbl.length manager.wires)

let test_wire_retrieval () =
  let manager = create_wire_manager () in
  
  let wire_id = match allocate_wire manager "test_wire" Intermediate () with
    | Ok id -> id
    | Error _ -> Alcotest.fail "Wire allocation should succeed"
  in
  
  let result1 = get_wire manager wire_id in
  Alcotest.(check bool) "wire retrieval success" true (Result.is_ok result1);
  (match result1 with
  | Ok wire -> 
      Alcotest.(check string) "wire name" "test_wire" wire.name;
      Alcotest.(check int) "wire id" wire_id wire.id
  | Error _ -> Alcotest.fail "Wire retrieval should succeed");
  
  let result2 = get_wire_by_name manager "test_wire" in
  Alcotest.(check bool) "wire retrieval by name success" true (Result.is_ok result2);
  
  let result3 = get_wire manager 999 in
  Alcotest.(check bool) "nonexistent wire fails" true (Result.is_error result3)

let test_wire_value_setting () =
  let manager = create_wire_manager () in
  let value = Field.of_string "42" |> Result.get_ok in
  
  let wire_id = allocate_wire manager "valued_wire" Input () |> Result.get_ok in
  
  let result1 = set_wire_value manager wire_id value in
  Alcotest.(check bool) "set wire value success" true (Result.is_ok result1);
  
  let wire = get_wire manager wire_id |> Result.get_ok in
  Alcotest.(check bool) "wire has value" true (Option.is_some wire.value);
  (match wire.value with
  | Some v -> Alcotest.(check string) "wire value" "42" (Field.to_string v)
  | None -> Alcotest.fail "Wire should have value")

let test_linear_combination () =
  let lc = empty_linear_combination in
  Alcotest.(check int) "empty combination length" 0 (List.length lc);
  
  let lc2 = add_term 1 Field.one lc in
  let lc3 = add_term 2 (Field.of_string "2" |> Result.get_ok) lc2 in
  
  Alcotest.(check int) "combination with 2 terms" 2 (List.length lc3);
  
  let coeff1 = get_coefficient 1 lc3 in
  Alcotest.(check bool) "coefficient 1 exists" true (Option.is_some coeff1);
  
  let coeff3 = get_coefficient 3 lc3 in
  Alcotest.(check bool) "coefficient 3 missing" true (Option.is_none coeff3)

let test_constraint_creation () =
  let a_lc = add_term 1 Field.one empty_linear_combination in
  let b_lc = add_term 2 Field.one empty_linear_combination in
  let c_lc = add_term 3 Field.one empty_linear_combination in
  
  let constraint_triple = create_constraint a_lc b_lc c_lc in
  
  Alcotest.(check int) "constraint a length" 1 (List.length constraint_triple.a);
  Alcotest.(check int) "constraint b length" 1 (List.length constraint_triple.b);
  Alcotest.(check int) "constraint c length" 1 (List.length constraint_triple.c);
  
  let constraint_str = constraint_to_string constraint_triple in
  Alcotest.(check bool) "constraint string contains wire references" true
    (String.contains constraint_str 'w')

let test_constraint_validation () =
  let two = Field.of_string "2" |> Result.get_ok in
  let three = Field.of_string "3" |> Result.get_ok in
  let six = Field.of_string "6" |> Result.get_ok in
  
  let a_lc = add_term 1 Field.one empty_linear_combination in
  let b_lc = add_term 2 Field.one empty_linear_combination in
  let c_lc = add_term 3 Field.one empty_linear_combination in
  
  let constraint_triple = create_constraint a_lc b_lc c_lc in
  let witness = [(1, two); (2, three); (3, six)] in
  
  let result = validate_constraint constraint_triple witness in
  Alcotest.(check bool) "constraint validation success" true (Result.is_ok result);
  
  let bad_witness = [(1, two); (2, three); (3, Field.one)] in
  let bad_result = validate_constraint constraint_triple bad_witness in
  Alcotest.(check bool) "bad constraint validation fails" true (Result.is_error bad_result)

let test_r1cs_creation () =
  let r1cs = create_empty_r1cs () in
  
  Alcotest.(check int) "empty r1cs variables" 0 r1cs.metadata.num_variables;
  Alcotest.(check int) "empty r1cs constraints" 0 r1cs.metadata.num_constraints;
  Alcotest.(check int) "empty r1cs inputs" 0 r1cs.metadata.num_inputs;
  Alcotest.(check int) "empty constraint list" 0 (List.length r1cs.constraints);
  Alcotest.(check int) "empty wire list" 0 (List.length r1cs.wires)

let test_r1cs_constraint_addition () =
  let r1cs = create_empty_r1cs () in
  let constraint_triple = create_multiplication_constraint 1 2 3 in
  
  let updated_r1cs = add_constraint_to_r1cs r1cs constraint_triple in
  
  Alcotest.(check int) "constraint count updated" 1 updated_r1cs.metadata.num_constraints;
  Alcotest.(check int) "constraint list length" 1 (List.length updated_r1cs.constraints)

let test_r1cs_wire_addition () =
  let r1cs = create_empty_r1cs () in
  let input_wire = { id = 1; name = "input_a"; wire_type = Input; value = None } in
  let private_wire = { id = 2; name = "private_b"; wire_type = Private; value = None } in
  
  let r1cs2 = add_wire_to_r1cs r1cs input_wire in
  let r1cs3 = add_wire_to_r1cs r1cs2 private_wire in
  
  Alcotest.(check int) "variable count" 2 r1cs3.metadata.num_variables;
  Alcotest.(check int) "input count" 1 r1cs3.metadata.num_inputs;
  Alcotest.(check int) "private count" 1 r1cs3.metadata.num_private;
  Alcotest.(check int) "input wire list" 1 (List.length r1cs3.input_wires);
  Alcotest.(check int) "private wire list" 1 (List.length r1cs3.private_wires)

let test_r1cs_validation () =
  let r1cs = create_empty_r1cs () in
  let constraint1 = create_multiplication_constraint 1 2 3 in
  let constraint2 = create_addition_constraint 3 4 5 in
  
  let r1cs2 = add_constraint_to_r1cs r1cs constraint1 in
  let r1cs3 = add_constraint_to_r1cs r1cs2 constraint2 in
  
  let two = Field.of_string "2" |> Result.get_ok in
  let three = Field.of_string "3" |> Result.get_ok in
  let four = Field.of_string "4" |> Result.get_ok in
  let six = Field.of_string "6" |> Result.get_ok in
  let ten = Field.of_string "10" |> Result.get_ok in
  
  let witness = [(1, two); (2, three); (3, six); (4, four); (5, ten)] in
  let result = validate_r1cs r1cs3 witness in
  Alcotest.(check bool) "r1cs validation success" true (Result.is_ok result);
  
  let bad_witness = [(1, two); (2, three); (3, Field.one); (4, four); (5, ten)] in
  let bad_result = validate_r1cs r1cs3 bad_witness in
  Alcotest.(check bool) "bad r1cs validation fails" true (Result.is_error bad_result)

let test_constraint_helpers () =
  let add_constraint = create_addition_constraint 1 2 3 in
  let add_str = constraint_to_string add_constraint in
  Alcotest.(check bool) "addition constraint format" true 
    (String.contains add_str '+' && String.contains add_str '=');
  
  let mul_constraint = create_multiplication_constraint 4 5 6 in
  let mul_str = constraint_to_string mul_constraint in
  Alcotest.(check bool) "multiplication constraint format" true 
    (String.contains mul_str '*' && String.contains mul_str '=');
  
  let const_value = Field.of_string "42" |> Result.get_ok in
  let const_constraint = create_constant_constraint 7 const_value in
  let const_str = constraint_to_string const_constraint in
  Alcotest.(check bool) "constant constraint format" true 
    (String.contains const_str '4')

let test_r1cs_statistics () =
  let r1cs = create_empty_r1cs () in
  let input_wire = { id = 1; name = "input"; wire_type = Input; value = None } in
  let output_wire = { id = 2; name = "output"; wire_type = Output; value = None } in
  let constraint1 = create_multiplication_constraint 1 1 2 in
  
  let r1cs2 = add_wire_to_r1cs r1cs input_wire in
  let r1cs3 = add_wire_to_r1cs r1cs2 output_wire in
  let r1cs4 = add_constraint_to_r1cs r1cs3 constraint1 in
  
  let stats = get_r1cs_statistics r1cs4 in
  Alcotest.(check int) "stats variables" 2 stats.num_variables;
  Alcotest.(check int) "stats constraints" 1 stats.num_constraints;
  Alcotest.(check int) "stats inputs" 1 stats.num_inputs;
  Alcotest.(check int) "stats outputs" 1 stats.num_outputs;
  
  let density = analyze_constraint_density r1cs4 in
  Alcotest.(check bool) "density positive" true (density > 0.0);
  Alcotest.(check bool) "density reasonable" true (density <= 1.0)

let test_constraint_complexity () =
  let simple_constraint = create_multiplication_constraint 1 2 3 in
  let complex_a = [1, Field.one; 2, Field.one] in
  let complex_b = [3, Field.one; 4, Field.one; 5, Field.one] in
  let complex_c = [6, Field.one] in
  let complex_constraint = create_constraint complex_a complex_b complex_c in
  
  let r1cs = create_empty_r1cs () in
  let r1cs2 = add_constraint_to_r1cs r1cs simple_constraint in
  let r1cs3 = add_constraint_to_r1cs r1cs2 complex_constraint in
  
  let complexities = get_constraint_complexity r1cs3 in
  Alcotest.(check int) "complexity list length" 2 (List.length complexities);
  Alcotest.(check bool) "has simple constraint" true 
    (List.exists (fun c -> c = 3) complexities);
  Alcotest.(check bool) "has complex constraint" true 
    (List.exists (fun c -> c = 6) complexities)

let test_wire_usage_analysis () =
  let constraint1 = create_multiplication_constraint 1 2 3 in
  let constraint2 = create_addition_constraint 1 3 4 in
  
  let r1cs = create_empty_r1cs () in
  let r1cs2 = add_constraint_to_r1cs r1cs constraint1 in
  let r1cs3 = add_constraint_to_r1cs r1cs2 constraint2 in
  
  let usage_stats = get_wire_usage_stats r1cs3 in
  let wire1_usage = try Hashtbl.find usage_stats 1 with Not_found -> 0 in
  let wire3_usage = try Hashtbl.find usage_stats 3 with Not_found -> 0 in
  
  Alcotest.(check int) "wire 1 used twice" 2 wire1_usage;
  Alcotest.(check int) "wire 3 used twice" 2 wire3_usage

let test_json_serialization () =
  let r1cs = create_empty_r1cs () in
  let constraint1 = create_multiplication_constraint 1 2 3 in
  let r1cs2 = add_constraint_to_r1cs r1cs constraint1 in
  
  let json = serialize_r1cs_json r1cs2 in
  Alcotest.(check bool) "json serialization produces object" true
    (match json with `Assoc _ -> true | _ -> false);
  
  let temp_file = Filename.temp_file "r1cs_test" ".json" in
  export_r1cs_json r1cs2 temp_file;
  
  let file_exists = Sys.file_exists temp_file in
  Alcotest.(check bool) "json file created" true file_exists;
  
  if file_exists then begin
    let file_size = (Unix.stat temp_file).st_size in
    Alcotest.(check bool) "json file not empty" true (file_size > 0);
    Sys.remove temp_file
  end

let () =
  let open Alcotest in
  run "R1CS Tests" [
    "wire_manager", [
      test_case "Wire manager creation" `Quick test_wire_manager_creation;
      test_case "Wire allocation" `Quick test_wire_allocation;
      test_case "Wire retrieval" `Quick test_wire_retrieval;
      test_case "Wire value setting" `Quick test_wire_value_setting;
    ];
    "constraints", [
      test_case "Linear combination" `Quick test_linear_combination;
      test_case "Constraint creation" `Quick test_constraint_creation;
      test_case "Constraint validation" `Quick test_constraint_validation;
      test_case "Constraint helpers" `Quick test_constraint_helpers;
    ];
    "r1cs", [
      test_case "R1CS creation" `Quick test_r1cs_creation;
      test_case "R1CS constraint addition" `Quick test_r1cs_constraint_addition;
      test_case "R1CS wire addition" `Quick test_r1cs_wire_addition;
      test_case "R1CS validation" `Quick test_r1cs_validation;
    ];
    "analysis", [
      test_case "R1CS statistics" `Quick test_r1cs_statistics;
      test_case "Constraint complexity" `Quick test_constraint_complexity;
      test_case "Wire usage analysis" `Quick test_wire_usage_analysis;
    ];
    "serialization", [
      test_case "JSON serialization" `Quick test_json_serialization;
    ];
  ]