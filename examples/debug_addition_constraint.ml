let debug_addition_constraint () =
  Printf.printf "=== Debug Addition Constraint ===\n";
  
  (* Create addition constraint: (wire3 + wire4) * 1 = wire5 *)
  let constraint_add = R1cs.create_addition_constraint 3 4 5 in
  Printf.printf "Addition Constraint: %s\n" (R1cs.constraint_to_string constraint_add);
  
  let six = Field.of_string "6" |> Result.get_ok in
  let four = Field.of_string "4" |> Result.get_ok in  
  let ten = Field.of_string "10" |> Result.get_ok in
  let one = Field.one in
  
  (* witness: w3=6, w4=4, w5=10, w0=1 (constant) *)
  let witness = [(0, one); (3, six); (4, four); (5, ten)] in
  
  Printf.printf "Evaluating A·witness (should be 6+4=10)...\n";
  let a_result = R1cs.evaluate_linear_combination constraint_add.a witness in
  (match a_result with
  | Ok a_val -> Printf.printf "A·witness = %s\n" (Field.to_string a_val)
  | Error e -> Printf.printf "A evaluation failed: %s\n" (Error.error_to_string e));
  
  Printf.printf "Evaluating B·witness (should be 1)...\n";
  let b_result = R1cs.evaluate_linear_combination constraint_add.b witness in
  (match b_result with
  | Ok b_val -> Printf.printf "B·witness = %s\n" (Field.to_string b_val)
  | Error e -> Printf.printf "B evaluation failed: %s\n" (Error.error_to_string e));
  
  Printf.printf "Evaluating C·witness (should be 10)...\n";
  let c_result = R1cs.evaluate_linear_combination constraint_add.c witness in
  (match c_result with
  | Ok c_val -> Printf.printf "C·witness = %s\n" (Field.to_string c_val)
  | Error e -> Printf.printf "C evaluation failed: %s\n" (Error.error_to_string e));
  
  let validation_result = R1cs.validate_constraint constraint_add witness in
  match validation_result with
  | Ok () -> Printf.printf "Addition constraint validation: PASSED\n"
  | Error e -> Printf.printf "Addition constraint validation: FAILED - %s\n" (Error.error_to_string e)

let () =
  debug_addition_constraint ()