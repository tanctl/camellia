open Error

let test_field_constants () =
  Alcotest.(check string) "zero" "0" Field.zero;
  Alcotest.(check string) "one" "1" Field.one;
  Alcotest.(check bool) "is_zero" true (Field.is_zero Field.zero);
  Alcotest.(check bool) "is_one" true (Field.is_one Field.one);
  Alcotest.(check bool) "zero not one" false (Field.is_one Field.zero);
  Alcotest.(check bool) "one not zero" false (Field.is_zero Field.one)

let test_field_conversions () =
  let result1 = Field.of_int 42 in
  Alcotest.(check bool) "of_int success" true (Result.is_ok result1);
  (match result1 with
  | Ok f -> Alcotest.(check string) "of_int value" "42" (Field.to_string f)
  | Error _ -> Alcotest.(check bool) "should not fail" false true);
  
  let result2 = Field.of_string "123" in
  Alcotest.(check bool) "of_string success" true (Result.is_ok result2);
  (match result2 with
  | Ok f -> Alcotest.(check string) "of_string value" "123" f
  | Error _ -> Alcotest.(check bool) "should not fail" false true);
  
  let result3 = Field.of_int (-5) in
  Alcotest.(check bool) "negative int fails" true (Result.is_error result3)

let test_field_validation () =
  let result1 = Field.of_string "" in
  Alcotest.(check bool) "empty string fails" true (Result.is_error result1);
  
  let result2 = Field.of_string "abc" in
  Alcotest.(check bool) "non-numeric fails" true (Result.is_error result2);
  
  let result3 = Field.of_string "12a34" in
  Alcotest.(check bool) "mixed chars fails" true (Result.is_error result3);
  
  let very_large = String.make 100 '9' in
  let result4 = Field.of_string very_large in
  Alcotest.(check bool) "oversized number fails" true (Result.is_error result4);
  
  let result5 = Field.of_string "0" in
  Alcotest.(check bool) "valid zero" true (Result.is_ok result5);
  
  let result6 = Field.of_string "999999999999999999999999999999" in
  Alcotest.(check bool) "valid large number" true (Result.is_ok result6)

let test_field_arithmetic () =
  let a = "42" in
  let b = "17" in
  let modulus = Field.bn254_modulus in
  
  let result1 = Field.add a b in
  Alcotest.(check bool) "add success" true (Result.is_ok result1);
  (match result1 with
  | Ok sum -> 
      let expected = Printf.sprintf "(42 + 17 mod %s)" modulus in
      Alcotest.(check string) "add format" expected sum
  | Error _ -> Alcotest.(check bool) "should not fail" false true);
  
  let result2 = Field.mul a b in
  Alcotest.(check bool) "mul success" true (Result.is_ok result2);
  (match result2 with
  | Ok product ->
      let expected = Printf.sprintf "(42 * 17 mod %s)" modulus in
      Alcotest.(check string) "mul format" expected product
  | Error _ -> Alcotest.(check bool) "should not fail" false true);
  
  let result3 = Field.sub a b in
  Alcotest.(check bool) "sub success" true (Result.is_ok result3);
  (match result3 with
  | Ok diff ->
      let expected = Printf.sprintf "(42 - 17 mod %s)" modulus in
      Alcotest.(check string) "sub format" expected diff
  | Error _ -> Alcotest.(check bool) "should not fail" false true)

let test_field_equality () =
  let result1 = Field.equal "42" "42" in
  Alcotest.(check bool) "equal same" true (Result.is_ok result1);
  (match result1 with
  | Ok eq -> Alcotest.(check bool) "42 equals 42" true eq
  | Error _ -> Alcotest.(check bool) "should not fail" false true);
  
  let result2 = Field.equal "42" "17" in
  Alcotest.(check bool) "equal different" true (Result.is_ok result2);
  (match result2 with
  | Ok eq -> Alcotest.(check bool) "42 not equal 17" false eq
  | Error _ -> Alcotest.(check bool) "should not fail" false true)

let test_field_special_operations () =
  let a = "42" in
  let modulus = Field.bn254_modulus in
  
  let result1 = Field.neg a in
  Alcotest.(check bool) "neg success" true (Result.is_ok result1);
  (match result1 with
  | Ok neg_a ->
      let expected = Printf.sprintf "(-%s mod %s)" a modulus in
      Alcotest.(check string) "neg format" expected neg_a
  | Error _ -> Alcotest.(check bool) "should not fail" false true);
  
  let result2 = Field.inv a in
  Alcotest.(check bool) "inv success" true (Result.is_ok result2);
  (match result2 with
  | Ok inv_a ->
      let expected = Printf.sprintf "(%s^-1 mod %s)" a modulus in
      Alcotest.(check string) "inv format" expected inv_a
  | Error _ -> Alcotest.(check bool) "should not fail" false true);
  
  let result3 = Field.inv Field.zero in
  Alcotest.(check bool) "inv zero fails" true (Result.is_error result3);
  
  let result4 = Field.pow a 3 in
  Alcotest.(check bool) "pow success" true (Result.is_ok result4);
  (match result4 with
  | Ok pow_a ->
      let expected = Printf.sprintf "(%s^3 mod %s)" a modulus in
      Alcotest.(check string) "pow format" expected pow_a
  | Error _ -> Alcotest.(check bool) "should not fail" false true);
  
  let result5 = Field.pow a (-2) in
  Alcotest.(check bool) "negative pow fails" true (Result.is_error result5)

let test_field_division () =
  let a = "42" in
  let b = "17" in
  let modulus = Field.bn254_modulus in
  
  let result1 = Field.div a b in
  Alcotest.(check bool) "div success" true (Result.is_ok result1);
  (match result1 with
  | Ok div_result ->
      let expected_inv = Printf.sprintf "(%s^-1 mod %s)" b modulus in
      let expected = Printf.sprintf "(%s * %s mod %s)" a expected_inv modulus in
      Alcotest.(check string) "div format" expected div_result
  | Error _ -> Alcotest.(check bool) "should not fail" false true);
  
  let result2 = Field.div a Field.zero in
  Alcotest.(check bool) "div by zero fails" true (Result.is_error result2)

let test_field_error_propagation () =
  let result = 
    let* a = Field.of_string "invalid" in
    let* b = Field.of_string "42" in
    Field.add a b
  in
  Alcotest.(check bool) "error propagation" true (Result.is_error result);
  
  let result2 = 
    let* a = Field.of_string "42" in
    let* b = Field.of_string "17" in
    let* _sum = Field.add a b in
    let* _product = Field.mul a b in
    Field.equal a b
  in
  Alcotest.(check bool) "monadic success chain" true (Result.is_ok result2)

let test_field_compare_and_display () =
  let a = "42" in
  let b = "17" in
  let c = "42" in
  
  Alcotest.(check int) "compare equal" 0 (Field.compare a c);
  Alcotest.(check bool) "compare greater" true (Field.compare a b > 0);
  Alcotest.(check bool) "compare less" true (Field.compare b a < 0);
  
  let detailed = Field.to_string_detailed a in
  Alcotest.(check string) "detailed format" "Field(42)" detailed

let () =
  let open Alcotest in
  run "Field Tests" [
    "constants", [
      test_case "Field constants" `Quick test_field_constants;
    ];
    "conversions", [
      test_case "Field conversions" `Quick test_field_conversions;
      test_case "Field validation" `Quick test_field_validation;
    ];
    "arithmetic", [
      test_case "Basic arithmetic" `Quick test_field_arithmetic;
      test_case "Field equality" `Quick test_field_equality;
      test_case "Special operations" `Quick test_field_special_operations;
      test_case "Field division" `Quick test_field_division;
    ];
    "advanced", [
      test_case "Error propagation" `Quick test_field_error_propagation;
      test_case "Compare and display" `Quick test_field_compare_and_display;
    ];
  ]