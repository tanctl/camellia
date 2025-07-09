open R1cs

let demo_wire_management () =
  Printf.printf "=== Wire Management Demo ===\n";
  let manager = create_wire_manager () in
  
  let input_a = allocate_wire manager "input_a" Input () |> Result.get_ok in
  let input_b = allocate_wire manager "input_b" Input () |> Result.get_ok in
  let output = allocate_wire manager "output" Output () |> Result.get_ok in
  
  Printf.printf "Allocated wires:\n";
  Printf.printf "  input_a: wire_%d\n" input_a;
  Printf.printf "  input_b: wire_%d\n" input_b; 
  Printf.printf "  output: wire_%d\n" output;
  
  let wire_a = get_wire_by_name manager "input_a" |> Result.get_ok in
  Printf.printf "Retrieved wire 'input_a': %s (type: %s)\n" 
    wire_a.name (wire_type_to_string wire_a.wire_type);
  Printf.printf "\n"

let demo_linear_combinations () =
  Printf.printf "=== Linear Combinations Demo ===\n";
  
  let lc1 = empty_linear_combination in
  let lc2 = add_term 1 Field.one lc1 in
  let lc3 = add_term 2 (Field.of_string "3" |> Result.get_ok) lc2 in
  
  Printf.printf "Linear combination: %s\n" (linear_combination_to_string lc3);
  
  let coeff1 = get_coefficient 1 lc3 in
  let coeff2 = get_coefficient 2 lc3 in
  Printf.printf "Coefficient for wire 1: %s\n" 
    (match coeff1 with Some c -> Field.to_string c | None -> "none");
  Printf.printf "Coefficient for wire 2: %s\n" 
    (match coeff2 with Some c -> Field.to_string c | None -> "none");
  Printf.printf "\n"

let demo_addition_constraint () =
  Printf.printf "=== Addition Constraint Demo ===\n";
  
  let manager = create_wire_manager () in
  let a_wire = allocate_wire manager "a" Input () |> Result.get_ok in
  let b_wire = allocate_wire manager "b" Input () |> Result.get_ok in
  let sum_wire = allocate_wire manager "sum" Output () |> Result.get_ok in
  
  let addition_constraint = create_addition_constraint a_wire b_wire sum_wire in
  Printf.printf "Addition constraint: %s\n" (constraint_to_string addition_constraint);
  
  let a_val = Field.of_string "5" |> Result.get_ok in
  let b_val = Field.of_string "7" |> Result.get_ok in
  let sum_val = Field.of_string "12" |> Result.get_ok in
  
  let witness = [(a_wire, a_val); (b_wire, b_val); (sum_wire, sum_val)] in
  let validation_result = validate_constraint addition_constraint witness in
  Printf.printf "Constraint validation: %s\n"
    (match validation_result with Ok () -> "PASSED" | Error _ -> "FAILED");
  
  let bad_sum = Field.of_string "10" |> Result.get_ok in
  let bad_witness = [(a_wire, a_val); (b_wire, b_val); (sum_wire, bad_sum)] in
  let bad_result = validate_constraint addition_constraint bad_witness in
  Printf.printf "Bad constraint validation: %s\n"
    (match bad_result with Ok () -> "PASSED" | Error _ -> "FAILED");
  Printf.printf "\n"

let demo_multiplication_constraint () =
  Printf.printf "=== Multiplication Constraint Demo ===\n";
  
  let manager = create_wire_manager () in
  let x_wire = allocate_wire manager "x" Input () |> Result.get_ok in
  let y_wire = allocate_wire manager "y" Input () |> Result.get_ok in
  let product_wire = allocate_wire manager "product" Output () |> Result.get_ok in
  
  let multiplication_constraint = create_multiplication_constraint x_wire y_wire product_wire in
  Printf.printf "Multiplication constraint: %s\n" (constraint_to_string multiplication_constraint);
  
  let x_val = Field.of_string "6" |> Result.get_ok in
  let y_val = Field.of_string "7" |> Result.get_ok in
  let product_val = Field.of_string "42" |> Result.get_ok in
  
  let witness = [(x_wire, x_val); (y_wire, y_val); (product_wire, product_val)] in
  let validation_result = validate_constraint multiplication_constraint witness in
  Printf.printf "Constraint validation: %s\n"
    (match validation_result with Ok () -> "PASSED" | Error _ -> "FAILED");
  Printf.printf "\n"

let demo_r1cs_system () =
  Printf.printf "=== R1CS System Demo ===\n";
  
  let manager = create_wire_manager () in
  let a_wire = allocate_wire manager "a" Input () |> Result.get_ok in
  let b_wire = allocate_wire manager "b" Input () |> Result.get_ok in
  let c_wire = allocate_wire manager "c" Private () |> Result.get_ok in
  let sum_wire = allocate_wire manager "sum" Intermediate () |> Result.get_ok in
  let result_wire = allocate_wire manager "result" Output () |> Result.get_ok in
  
  let wire_a = get_wire manager a_wire |> Result.get_ok in
  let wire_b = get_wire manager b_wire |> Result.get_ok in
  let wire_c = get_wire manager c_wire |> Result.get_ok in
  let wire_sum = get_wire manager sum_wire |> Result.get_ok in
  let wire_result = get_wire manager result_wire |> Result.get_ok in
  
  let constraint1 = create_addition_constraint a_wire b_wire sum_wire in
  let constraint2 = create_multiplication_constraint sum_wire c_wire result_wire in
  
  let r1cs = create_empty_r1cs () in
  let r1cs2 = add_wire_to_r1cs r1cs wire_a in
  let r1cs3 = add_wire_to_r1cs r1cs2 wire_b in
  let r1cs4 = add_wire_to_r1cs r1cs3 wire_c in
  let r1cs5 = add_wire_to_r1cs r1cs4 wire_sum in
  let r1cs6 = add_wire_to_r1cs r1cs5 wire_result in
  let r1cs7 = add_constraint_to_r1cs r1cs6 constraint1 in
  let r1cs8 = add_constraint_to_r1cs r1cs7 constraint2 in
  
  Printf.printf "R1CS System:\n%s\n" (r1cs_to_string r1cs8);
  
  let stats = get_r1cs_statistics r1cs8 in
  Printf.printf "Statistics:\n";
  Printf.printf "  Variables: %d\n" stats.num_variables;
  Printf.printf "  Constraints: %d\n" stats.num_constraints;
  Printf.printf "  Inputs: %d\n" stats.num_inputs;
  Printf.printf "  Private: %d\n" stats.num_private;
  Printf.printf "  Outputs: %d\n" stats.num_outputs;
  
  let density = analyze_constraint_density r1cs8 in
  Printf.printf "  Constraint density: %.2f%%\n" (density *. 100.0);
  
  let a_val = Field.of_string "3" |> Result.get_ok in
  let b_val = Field.of_string "4" |> Result.get_ok in
  let c_val = Field.of_string "2" |> Result.get_ok in
  let sum_val = Field.of_string "7" |> Result.get_ok in
  let result_val = Field.of_string "14" |> Result.get_ok in
  
  let witness = [
    (a_wire, a_val); (b_wire, b_val); (c_wire, c_val);
    (sum_wire, sum_val); (result_wire, result_val)
  ] in
  
  let validation_result = validate_r1cs r1cs8 witness in
  Printf.printf "R1CS validation: %s\n"
    (match validation_result with Ok () -> "PASSED" | Error e -> 
      "FAILED - " ^ (Error.error_to_string e));
  Printf.printf "\n"

let demo_json_serialization () =
  Printf.printf "=== JSON Serialization Demo ===\n";
  
  let constraint1 = create_multiplication_constraint 1 2 3 in
  let constraint2 = create_addition_constraint 3 4 5 in
  
  let r1cs = create_empty_r1cs () in
  let r1cs2 = add_constraint_to_r1cs r1cs constraint1 in
  let r1cs3 = add_constraint_to_r1cs r1cs2 constraint2 in
  
  let json_file = "r1cs_demo.json" in
  export_r1cs_json r1cs3 json_file;
  Printf.printf "R1CS exported to %s\n" json_file;
  
  if Sys.file_exists json_file then begin
    let ic = open_in json_file in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Printf.printf "JSON preview (first 200 chars):\n%s...\n" 
      (String.sub content 0 (min 200 (String.length content)));
    Sys.remove json_file
  end;
  Printf.printf "\n"

let () =
  demo_wire_management ();
  demo_linear_combinations ();
  demo_addition_constraint ();
  demo_multiplication_constraint ();
  demo_r1cs_system ();
  demo_json_serialization ();
  Printf.printf "R1CS demonstration completed!\n"