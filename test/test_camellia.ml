
let test_ast_creation () =
  let _expr = Ast.Add (Ast.Var "x", Ast.Const "1") in
  let circuit = Ast.{ name = "test"; inputs = ["x"]; private_inputs = []; body = [] } in
  Alcotest.(check (list string)) "inputs" ["x"] circuit.inputs

let test_parser () =
  match Parser.parse_circuit "valid" with
  | Ok circuit -> Alcotest.(check (list string)) "parsed inputs" ["x"; "y"] circuit.inputs
  | Error _ -> Alcotest.fail "Expected successful parse"

let test_compiler () =
  let circuit = Ast.{ name = "test"; inputs = ["x"]; private_inputs = []; body = [] } in
  let compiled = Compiler.compile_circuit circuit in
  Alcotest.(check (list (triple string string string))) "gates" [] compiled.gates

let () =
  let open Alcotest in
  run "Camellia" [
    "ast", [
      test_case "AST creation" `Quick test_ast_creation;
    ];
    "parser", [
      test_case "Parser basic" `Quick test_parser;
    ];
    "compiler", [
      test_case "Compiler basic" `Quick test_compiler;
    ];
  ]