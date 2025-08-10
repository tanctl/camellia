open Alcotest
open Ast
open Parser

let test_simple_circuit () =
  let input = {|
    circuit test {
      public inputs: x
      y = x + 1
      assert y == 2
    }
  |} in
  match parse_circuit_string input with
  | Ok circuit ->
    check string "circuit name" "test" circuit.name;
    check (list string) "public inputs" ["x"] circuit.inputs;
    check (list string) "private inputs" [] circuit.private_inputs;
    check int "statement count" 2 (List.length circuit.body)
  | Error _ ->
    Alcotest.fail "Failed to parse simple circuit"

let test_private_inputs () =
  let input = {|
    circuit test {
      private inputs: secret
      y = secret * 2
    }
  |} in
  match parse_circuit_string input with
  | Ok circuit ->
    check (list string) "public inputs" [] circuit.inputs;
    check (list string) "private inputs" ["secret"] circuit.private_inputs
  | Error _ ->
    Alcotest.fail "Failed to parse circuit with private inputs"

let test_poseidon_function () =
  let input = {|
    circuit test {
      inputs: x, y  
      hash = poseidon(x, y)
      assert hash == 123
    }
  |} in
  match parse_circuit_string input with
  | Ok circuit ->
    check int "statement count" 2 (List.length circuit.body);
    (match circuit.body with
    | [Assign ("hash", Poseidon [Var "x"; Var "y"]); _] -> ()
    | _ -> Alcotest.fail "Poseidon call not parsed correctly")
  | Error _ ->
    Alcotest.fail "Failed to parse circuit with poseidon"

let test_invalid_circuit () =
  let input = "invalid circuit syntax" in
  match parse_circuit_string input with
  | Ok _ -> Alcotest.fail "Should have failed to parse invalid circuit"
  | Error _ ->
    check bool "error occurred" true true

let test_expression_precedence () =
  let input = {|
    circuit precedence_test {
      inputs: a, b, c
      
      result1 = a + b * c
      result2 = (a + b) * c  
      result3 = a == b + c
    }
  |} in
  match parse_circuit_string input with
  | Ok circuit ->
      check string "circuit name" "precedence_test" circuit.name;
      check int "statement count" 3 (List.length circuit.body);
      
      (* check precedence: a + (b * c) not (a + b) * c *)
      (match List.hd circuit.body with
      | Assign ("result1", Add (Var "a", Mul (Var "b", Var "c"))) -> ()
      | _ -> Alcotest.fail "Precedence parsing failed for result1")
  | Error _ ->
      Alcotest.fail "Precedence test failed"

let test_function_calls () =
  let input = {|
    circuit function_test {
      inputs: x, y, z
      
      hash1 = poseidon(x)
      hash2 = poseidon(x, y, z)  
      hash3 = poseidon([x, y])
      
      assert hash1 == hash2
    }
  |} in
  match parse_circuit_string input with
  | Ok circuit ->
      check string "circuit name" "function_test" circuit.name;
      check int "statement count" 4 (List.length circuit.body);
      
      (match circuit.body with
      | [Assign ("hash1", Poseidon [Var "x"]);
         Assign ("hash2", Poseidon [Var "x"; Var "y"; Var "z"]);
         Assign ("hash3", Poseidon [Var "x"; Var "y"]);
         Constraint (Equal (Var "hash1", Var "hash2"))] -> ()
      | _ -> Alcotest.fail "Function call parsing failed")
  | Error _ ->
      Alcotest.fail "Parse error occurred"

let test_minimal_circuits () =
  let empty = "circuit empty {}" in
  (match parse_circuit_string empty with
  | Ok circuit ->
      check string "empty circuit name" "empty" circuit.name;
      check (list string) "empty inputs" [] circuit.inputs;
      check (list string) "empty private" [] circuit.private_inputs;
      check int "empty body length" 0 (List.length circuit.body)
  | Error _ ->
      Alcotest.fail "Parse error occurred");

  let inputs_only = {|
    circuit inputs_only {
      inputs: a, b, c
    }
  |} in
  (match parse_circuit_string inputs_only with
  | Ok circuit ->
      check (list string) "inputs only" ["a"; "b"; "c"] circuit.inputs;
      check int "no statements length" 0 (List.length circuit.body)
  | Error _ ->
      Alcotest.fail "Parse error occurred")

let test_whitespace_and_comments () =
  let input = {|


# this is a circuit comment
circuit   whitespace_test   {


    # public input comment  
    public   inputs  :  a,   b,   c   # inline comment


    private inputs:d,e

    # computation section
    result=a+b*c  # no spaces

    # assertion section
    assert   result==d+e   # spaced out


}  # end comment


|} in
  match parse_circuit_string input with
  | Ok circuit ->
      check string "whitespace circuit name" "whitespace_test" circuit.name;
      check (list string) "whitespace inputs" ["a"; "b"; "c"] circuit.inputs;
      check (list string) "whitespace private" ["d"; "e"] circuit.private_inputs;
      check int "whitespace statements" 2 (List.length circuit.body)
  | Error _ ->
      Alcotest.fail "Parse error occurred"

let test_syntax_errors () =
  let missing_name = "circuit { inputs: x }" in
  (match parse_circuit_string missing_name with
  | Ok _ -> Alcotest.fail "Should have failed with missing name"
  | Error _ ->
      
      check bool "missing name error" true true);

  let invalid_expr = "circuit test { result = x ++ }" in
  (match parse_circuit_string invalid_expr with
  | Ok _ -> Alcotest.fail "Should have failed with invalid expression"
  | Error _ ->
      
      check bool "invalid expr error" true true);

  let missing_comma = "circuit test { inputs: a b c }" in
  (match parse_circuit_string missing_comma with
  | Ok _ -> Alcotest.fail "Should have failed with missing comma"
  | Error _ ->
      
      check bool "missing comma error" true true)

let test_complex_expressions () =
  let input = {|
    circuit complex_test {
      public inputs: a, b
      private inputs: c, d
      
      # nested expressions
      nested = (a + b) * c
      chained = a + b * d
      equality = a == b
      mixed = poseidon(a + b, c * d)
      
      assert nested == chained
    }
  |} in
  match parse_circuit_string input with
  | Ok circuit ->
      check string "circuit name" "complex_test" circuit.name;
      check int "statement count" 5 (List.length circuit.body)
  | Error _ ->
      Alcotest.fail "Parse error occurred"

let () =
  run "Menhir Parser Tests" [
    "basic parsing", [
      test_case "Simple circuit" `Quick test_simple_circuit;
      test_case "Private inputs" `Quick test_private_inputs;
      test_case "Poseidon function" `Quick test_poseidon_function;
      test_case "Invalid circuit" `Quick test_invalid_circuit;
    ];
    "advanced parsing", [
      test_case "Expression precedence" `Quick test_expression_precedence;
      test_case "Function calls" `Quick test_function_calls;
      test_case "Minimal circuits" `Quick test_minimal_circuits;
      test_case "Whitespace and comments" `Quick test_whitespace_and_comments;
      test_case "Complex expressions" `Quick test_complex_expressions;
    ];
    "error handling", [
      test_case "Syntax errors" `Quick test_syntax_errors;
    ];
  ]