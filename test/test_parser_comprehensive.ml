let test_comprehensive_parser () =
  let input = {|
    circuit comprehensive_test {
      public inputs: a, b, c
      private inputs: secret1, secret2
      
      sum = a + b
      product = a * b
      complex = (a + b) * c
      hash1 = poseidon(a, b)
      hash2 = poseidon([secret1, secret2])
      assert sum == 5
      assert product == complex + hash1
      assert hash2 == 12345
    }
  |} in
  match Parser.parse_circuit input with
  | Ok circuit ->
    Alcotest.(check string) "circuit name" "comprehensive_test" circuit.name;
    Alcotest.(check (list string)) "public inputs" ["a"; "b"; "c"] circuit.inputs;
    Alcotest.(check (list string)) "private inputs" ["secret1"; "secret2"] circuit.private_inputs;
    Alcotest.(check int) "statement count" 8 (List.length circuit.body);
    Printf.printf "✓ Comprehensive parser test passed\n"
  | Error err ->
    Printf.printf "✗ Comprehensive parser test failed: %s\n" (Error.error_to_string err);
    Alcotest.fail "Failed to parse comprehensive circuit"

let test_edge_cases () =
  (* Test minimal circuit *)
  let minimal = "circuit empty {}" in
  (match Parser.parse_circuit minimal with
  | Ok _ -> Printf.printf "✓ Minimal circuit test passed\n"
  | Error _ -> Alcotest.fail "Failed to parse minimal circuit");
  
  (* Test with only inputs *)
  let inputs_only = "circuit inputs_only { inputs: x, y, z }" in
  (match Parser.parse_circuit inputs_only with
  | Ok _ -> Printf.printf "✓ Inputs-only circuit test passed\n"
  | Error _ -> Alcotest.fail "Failed to parse inputs-only circuit");
  
  (* Test with complex expressions *)
  let complex_expr = "circuit complex { result = (a + b) * (c + d) }" in
  (match Parser.parse_circuit complex_expr with
  | Ok _ -> Printf.printf "✓ Complex expression test passed\n"
  | Error _ -> Alcotest.fail "Failed to parse complex expressions")

let () =
  let open Alcotest in
  run "Comprehensive Parser Tests" [
    "comprehensive", [
      test_case "Full feature test" `Quick test_comprehensive_parser;
      test_case "Edge cases" `Quick test_edge_cases;
    ];
  ]