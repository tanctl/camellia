
let debug_constraint_validation () =
  Printf.printf "=== Debug Constraint Validation ===\n";
  
  let a_lc = R1cs.add_term 1 Field.one R1cs.empty_linear_combination in
  let b_lc = R1cs.add_term 2 Field.one R1cs.empty_linear_combination in  
  let c_lc = R1cs.add_term 3 Field.one R1cs.empty_linear_combination in
  
  let constraint_triple = R1cs.create_constraint a_lc b_lc c_lc in
  Printf.printf "Constraint: %s\n" (R1cs.constraint_to_string constraint_triple);
  
  let two = Field.of_string "2" |> Result.get_ok in
  let three = Field.of_string "3" |> Result.get_ok in
  let six = Field.of_string "6" |> Result.get_ok in
  
  let witness = [(1, two); (2, three); (3, six)] in
  
  Printf.printf "Evaluating A·witness...\n";
  let a_result = R1cs.evaluate_linear_combination constraint_triple.a witness in
  (match a_result with
  | Ok a_val -> Printf.printf "A·witness = %s\n" (Field.to_string a_val)
  | Error e -> Printf.printf "A evaluation failed: %s\n" (Error.error_to_string e));
  
  Printf.printf "Evaluating B·witness...\n";
  let b_result = R1cs.evaluate_linear_combination constraint_triple.b witness in
  (match b_result with
  | Ok b_val -> Printf.printf "B·witness = %s\n" (Field.to_string b_val)
  | Error e -> Printf.printf "B evaluation failed: %s\n" (Error.error_to_string e));
  
  Printf.printf "Evaluating C·witness...\n";
  let c_result = R1cs.evaluate_linear_combination constraint_triple.c witness in
  (match c_result with
  | Ok c_val -> Printf.printf "C·witness = %s\n" (Field.to_string c_val)
  | Error e -> Printf.printf "C evaluation failed: %s\n" (Error.error_to_string e));
  
  match a_result, b_result, c_result with
  | Ok a_val, Ok b_val, Ok c_val ->
      let product_result = Field.mul a_val b_val in
      (match product_result with
      | Ok ab_product -> 
          let product_str = Field.to_string ab_product in
          Printf.printf "A * B = %s\n" product_str;
          Printf.printf "String length: %d\n" (String.length product_str);
          Printf.printf "Contains '(' : %b\n" (String.contains product_str '(');
          Printf.printf "Contains '*' : %b\n" (String.contains product_str '*');
          let eval_result = R1cs.eval_simple_field_expr product_str in
          (match eval_result with
          | Ok ab_eval -> Printf.printf "A * B evaluated = %s\n" (Field.to_string ab_eval)
          | Error e -> Printf.printf "A * B evaluation failed: %s\n" (Error.error_to_string e));
          
          let equal_result = R1cs.field_equal_evaluated (Field.to_string ab_product) (Field.to_string c_val) in
          (match equal_result with
          | Ok eq -> Printf.printf "(A * B) == C: %b\n" eq
          | Error e -> Printf.printf "Equality check failed: %s\n" (Error.error_to_string e))
      | Error e -> Printf.printf "A * B computation failed: %s\n" (Error.error_to_string e))
  | _ -> Printf.printf "Linear combination evaluation failed\n";
  
  let validation_result = R1cs.validate_constraint constraint_triple witness in
  match validation_result with
  | Ok () -> Printf.printf "Final constraint validation: PASSED\n"
  | Error e -> Printf.printf "Final constraint validation: FAILED - %s\n" (Error.error_to_string e)

let () =
  debug_constraint_validation ()