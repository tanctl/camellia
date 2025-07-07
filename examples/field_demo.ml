open Error

let demo_basic_operations () =
  Printf.printf "=== Field Basic Operations Demo ===\n";
  
  Printf.printf "Constants:\n";
  Printf.printf "  zero = %s\n" Field.zero;
  Printf.printf "  one = %s\n" Field.one;
  Printf.printf "  BN254 modulus = %s\n" Field.bn254_modulus;
  Printf.printf "\n";
  
  let a = "42" in
  let b = "17" in
  
  Printf.printf "Field elements:\n";
  Printf.printf "  a = %s\n" a;
  Printf.printf "  b = %s\n" b;
  Printf.printf "\n";
  
  Printf.printf "Arithmetic operations:\n";
  (match Field.add a b with
  | Ok sum -> Printf.printf "  a + b = %s\n" sum
  | Error e -> Printf.printf "  Addition failed: %s\n" (error_to_string e));
  
  (match Field.mul a b with
  | Ok product -> Printf.printf "  a * b = %s\n" product
  | Error e -> Printf.printf "  Multiplication failed: %s\n" (error_to_string e));
  
  (match Field.sub a b with
  | Ok diff -> Printf.printf "  a - b = %s\n" diff
  | Error e -> Printf.printf "  Subtraction failed: %s\n" (error_to_string e));
  
  (match Field.div a b with
  | Ok quotient -> Printf.printf "  a / b = %s\n" quotient
  | Error e -> Printf.printf "  Division failed: %s\n" (error_to_string e));
  
  Printf.printf "\n"

let demo_special_operations () =
  Printf.printf "=== Field Special Operations Demo ===\n";
  
  let a = "123" in
  
  Printf.printf "Field element: a = %s\n" a;
  Printf.printf "\n";
  
  (match Field.neg a with
  | Ok neg_a -> Printf.printf "  -a = %s\n" neg_a
  | Error e -> Printf.printf "  Negation failed: %s\n" (error_to_string e));
  
  (match Field.inv a with
  | Ok inv_a -> Printf.printf "  a^-1 = %s\n" inv_a
  | Error e -> Printf.printf "  Inversion failed: %s\n" (error_to_string e));
  
  (match Field.pow a 3 with
  | Ok pow_a -> Printf.printf "  a^3 = %s\n" pow_a
  | Error e -> Printf.printf "  Exponentiation failed: %s\n" (error_to_string e));
  
  Printf.printf "\n"

let demo_conversions () =
  Printf.printf "=== Field Conversions Demo ===\n";
  
  Printf.printf "Converting from integers:\n";
  (match Field.of_int 42 with
  | Ok f -> Printf.printf "  of_int 42 = %s\n" (Field.to_string f)
  | Error e -> Printf.printf "  of_int failed: %s\n" (error_to_string e));
  
  (match Field.of_int (-5) with
  | Ok f -> Printf.printf "  of_int (-5) = %s\n" (Field.to_string f)
  | Error e -> Printf.printf "  of_int (-5) failed: %s\n" (error_to_string e));
  
  Printf.printf "\nConverting from strings:\n";
  (match Field.of_string "999" with
  | Ok f -> Printf.printf "  of_string \"999\" = %s\n" f
  | Error e -> Printf.printf "  of_string failed: %s\n" (error_to_string e));
  
  (match Field.of_string "invalid" with
  | Ok f -> Printf.printf "  of_string \"invalid\" = %s\n" f
  | Error e -> Printf.printf "  of_string \"invalid\" failed: %s\n" (error_to_string e));
  
  Printf.printf "\n"

let demo_validation_and_errors () =
  Printf.printf "=== Field Validation and Error Handling Demo ===\n";
  
  let test_inputs = [
    "";
    "abc";
    "12a34";
    String.make 100 '9';
    "0";
    "12345";
  ] in
  
  Printf.printf "Testing field validation:\n";
  List.iter (fun input ->
    match Field.of_string input with
    | Ok f -> Printf.printf "  \"%s\" -> valid: %s\n" input f
    | Error e -> Printf.printf "  \"%s\" -> invalid: %s\n" input (error_to_string e)
  ) test_inputs;
  
  Printf.printf "\nTesting division by zero:\n";
  (match Field.div "42" Field.zero with
  | Ok _ -> Printf.printf "  Division by zero succeeded (unexpected)\n"
  | Error e -> Printf.printf "  Division by zero failed (expected): %s\n" (error_to_string e));
  
  Printf.printf "\nTesting inversion of zero:\n";
  (match Field.inv Field.zero with
  | Ok _ -> Printf.printf "  Inversion of zero succeeded (unexpected)\n"
  | Error e -> Printf.printf "  Inversion of zero failed (expected): %s\n" (error_to_string e));
  
  Printf.printf "\n"

let demo_monadic_composition () =
  Printf.printf "=== Field Monadic Composition Demo ===\n";
  
  let computation1 = 
    let* a = Field.of_string "10" in
    let* b = Field.of_string "5" in
    let* sum = Field.add a b in
    let* product = Field.mul a b in
    let+ are_equal = Field.equal sum product in
    (sum, product, are_equal)
  in
  
  (match computation1 with
  | Ok (sum, product, are_equal) ->
      Printf.printf "Success: sum=%s, product=%s, equal=%b\n" sum product are_equal
  | Error e -> Printf.printf "Computation 1 failed: %s\n" (error_to_string e));
  
  let computation2 = 
    let* a = Field.of_string "invalid" in
    let* b = Field.of_string "5" in
    Field.add a b
  in
  
  (match computation2 with
  | Ok _ -> Printf.printf "Computation 2 succeeded (unexpected)\n"
  | Error e -> Printf.printf "Computation 2 failed (expected): %s\n" (error_to_string e));
  
  Printf.printf "\n"

let () =
  demo_basic_operations ();
  demo_special_operations ();
  demo_conversions ();
  demo_validation_and_errors ();
  demo_monadic_composition ();
  Printf.printf "Field arithmetic system demo completed!\n"