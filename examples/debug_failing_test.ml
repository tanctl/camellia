let debug_failing_test () =
  Printf.printf "=== Debug Failing R1CS Validation Test ===\n";
  
  let r1cs = R1cs.create_empty_r1cs () in
  let constraint1 = R1cs.create_multiplication_constraint 1 2 3 in
  let constraint2 = R1cs.create_addition_constraint 3 4 5 in
  
  Printf.printf "Constraint 1: %s\n" (R1cs.constraint_to_string constraint1);
  Printf.printf "Constraint 2: %s\n" (R1cs.constraint_to_string constraint2);
  
  let r1cs2 = R1cs.add_constraint_to_r1cs r1cs constraint1 in
  let r1cs3 = R1cs.add_constraint_to_r1cs r1cs2 constraint2 in
  
  let two = Field.of_string "2" |> Result.get_ok in
  let three = Field.of_string "3" |> Result.get_ok in
  let four = Field.of_string "4" |> Result.get_ok in
  let six = Field.of_string "6" |> Result.get_ok in
  let ten = Field.of_string "10" |> Result.get_ok in
  let one = Field.one in
  
  (* witness: w1=2, w2=3, w3=6, w4=4, w5=10, w0=1 *)
  let witness = [(0, one); (1, two); (2, three); (3, six); (4, four); (5, ten)] in
  
  Printf.printf "\nValidating constraint 1 (multiplication): 2 * 3 = 6\n";
  let result1 = R1cs.validate_constraint constraint1 witness in
  (match result1 with
  | Ok () -> Printf.printf "Constraint 1: PASSED\n"
  | Error e -> Printf.printf "Constraint 1: FAILED - %s\n" (Error.error_to_string e));
  
  Printf.printf "\nValidating constraint 2 (addition): (6 + 4) * 1 = 10\n";
  let result2 = R1cs.validate_constraint constraint2 witness in
  (match result2 with
  | Ok () -> Printf.printf "Constraint 2: PASSED\n"
  | Error e -> Printf.printf "Constraint 2: FAILED - %s\n" (Error.error_to_string e));
  
  Printf.printf "\nValidating full R1CS system...\n";
  let result = R1cs.validate_r1cs r1cs3 witness in
  match result with
  | Ok () -> Printf.printf "Full R1CS validation: PASSED\n"
  | Error e -> Printf.printf "Full R1CS validation: FAILED - %s\n" (Error.error_to_string e)

let () =
  debug_failing_test ()