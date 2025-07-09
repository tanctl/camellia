let debug_missing_constant () =
  Printf.printf "=== Debug Missing Constant Wire ===\n";
  
  let constraint_add = R1cs.create_addition_constraint 3 4 5 in
  Printf.printf "Addition Constraint: %s\n" (R1cs.constraint_to_string constraint_add);
  
  let six = Field.of_string "6" |> Result.get_ok in
  let four = Field.of_string "4" |> Result.get_ok in  
  let ten = Field.of_string "10" |> Result.get_ok in
  
  (* witness WITHOUT wire 0 (constant) - matching the test *)
  let witness = [(3, six); (4, four); (5, ten)] in
  
  Printf.printf "Evaluating B·witness (should be 1 from implicit wire 0)...\n";
  let b_result = R1cs.evaluate_linear_combination constraint_add.b witness in
  (match b_result with
  | Ok b_val -> Printf.printf "B·witness = %s\n" (Field.to_string b_val)
  | Error e -> Printf.printf "B evaluation failed: %s\n" (Error.error_to_string e));
  
  let validation_result = R1cs.validate_constraint constraint_add witness in
  match validation_result with
  | Ok () -> Printf.printf "Addition constraint validation: PASSED\n"
  | Error e -> Printf.printf "Addition constraint validation: FAILED - %s\n" (Error.error_to_string e)

let () =
  debug_missing_constant ()