
let test_minimal_circuit () =
  let input = {|
circuit MinimalCircuit {
  inputs: x
  assert x == 5
}
|} in
  match Parser.parse_circuit input with
  | Ok circuit ->
      let debug_ctx = Debug.create_context "test" in
      (match Compiler.compile_circuit debug_ctx circuit with
       | Ok _ -> ()
       | Error err -> Alcotest.fail ("Compilation failed: " ^ Error.error_to_string err))
  | Error err -> Alcotest.fail ("Parse failed: " ^ Error.error_to_string err)

let test_hash_preimage_circuit () =
  let input = {|
circuit HashPreimage {
  inputs: expected_hash
  private inputs: preimage
  
  computed_hash = poseidon(preimage)
  assert computed_hash == expected_hash
}
|} in
  match Parser.parse_circuit input with
  | Ok circuit ->
      let debug_ctx = Debug.create_context "test" in
      (match Compiler.compile_circuit debug_ctx circuit with
       | Ok compiled ->
           let stats = Compiler.get_compilation_stats compiled in
           Alcotest.(check int) "has inputs" 1 stats.num_inputs;
           Alcotest.(check int) "has private inputs" 1 stats.num_private;
           Alcotest.(check bool) "has constraints" true (stats.num_constraints > 0)
       | Error err -> Alcotest.fail ("Compilation failed: " ^ Error.error_to_string err))
  | Error err -> Alcotest.fail ("Parse failed: " ^ Error.error_to_string err)

let test_arithmetic_operations () =
  let input = {|
circuit ArithmeticTest {
  inputs: a, b
  
  sum = a + b
  
  assert sum == 10
}
|} in
  match Parser.parse_circuit input with
  | Ok circuit ->
      let debug_ctx = Debug.create_context "test" in
      (match Compiler.compile_circuit debug_ctx circuit with
       | Ok _ -> ()
       | Error err -> Alcotest.fail ("Compilation failed: " ^ Error.error_to_string err))
  | Error err -> Alcotest.fail ("Parse failed: " ^ Error.error_to_string err)

let test_poseidon_variations () =
  let input = {|
circuit PoseidonTest {
  inputs: a, b, c
  private inputs: secret
  
  hash1 = poseidon(a)
  hash2 = poseidon(a, b)
  hash3 = poseidon(a, b, c, secret)
  
  assert hash1 == 123
}
|} in
  match Parser.parse_circuit input with
  | Ok circuit ->
      let debug_ctx = Debug.create_context "test" in
      (match Compiler.compile_circuit debug_ctx circuit with
       | Ok _ -> ()
       | Error err -> Alcotest.fail ("Compilation failed: " ^ Error.error_to_string err))
  | Error err -> Alcotest.fail ("Parse failed: " ^ Error.error_to_string err)

let test_syntax_errors () =
  let invalid_inputs = [
    ("unclosed brace", "circuit Test { inputs: x");
    ("invalid token", "circuit Test { $$$ }");
  ] in
  
  List.iter (fun (name, input) ->
    match Parser.parse_circuit input with
    | Ok _ -> Alcotest.fail (name ^ ": should have failed to parse")
    | Error _ -> ()
  ) invalid_inputs

let test_comment_handling () =
  let input = {|
// This is a line comment
circuit CommentTest {
  # This is also a comment
  inputs: x  // inline comment
  
  // Another comment
  result = x + 1  # mixed comment styles
  assert result == 6
}
|} in
  match Parser.parse_circuit input with
  | Ok circuit ->
      Alcotest.(check string) "circuit name" "CommentTest" circuit.name;
      Alcotest.(check int) "input count" 1 (List.length circuit.inputs);
  | Error err -> Alcotest.fail ("Parse failed: " ^ Error.error_to_string err)

let test_field_constants () =
  let input = {|
circuit FieldTest {
  inputs: x
  
  zero = 0
  small = 42
  large = 999999999
  
  assert x == zero
}
|} in
  match Parser.parse_circuit input with
  | Ok circuit ->
      let debug_ctx = Debug.create_context "test" in
      (match Compiler.compile_circuit debug_ctx circuit with
       | Ok _ -> ()
       | Error err -> Alcotest.fail ("Compilation failed: " ^ Error.error_to_string err))
  | Error err -> Alcotest.fail ("Parse failed: " ^ Error.error_to_string err)

let syntax_integration_tests = [
  "minimal_circuit", `Quick, test_minimal_circuit;
  "hash_preimage_circuit", `Quick, test_hash_preimage_circuit;
  "arithmetic_operations", `Quick, test_arithmetic_operations;
  "poseidon_variations", `Quick, test_poseidon_variations;
  "syntax_errors", `Quick, test_syntax_errors;
  "comment_handling", `Quick, test_comment_handling;
  "field_constants", `Quick, test_field_constants;
]

let () = Alcotest.run "Syntax Integration Tests" [
  "end_to_end", syntax_integration_tests;
]