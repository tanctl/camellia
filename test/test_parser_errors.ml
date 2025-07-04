open Parser
open Error

let test_parse_expr_errors () =
  let result1 = parse_expr "" in
  Alcotest.(check bool) "empty expr fails" true (Result.is_error result1);
  
  let result2 = parse_expr "invalid" in
  Alcotest.(check bool) "invalid token fails" true (Result.is_error result2);
  
  let result3 = parse_expr "valid_var" in
  Alcotest.(check bool) "valid expr succeeds" true (Result.is_ok result3)

let test_parse_circuit_errors () =
  let result1 = parse_circuit "" in
  Alcotest.(check bool) "empty circuit fails" true (Result.is_error result1);
  (match result1 with
  | Error err -> 
      let err_str = error_to_string err in
      Alcotest.(check bool) "empty circuit error message" true
        (String.contains err_str 'e')
  | Ok _ -> Alcotest.(check bool) "should not succeed" false true);
  
  let result2 = parse_circuit "cyclic" in
  Alcotest.(check bool) "cyclic dependency fails" true (Result.is_error result2);
  (match result2 with
  | Error err ->
      let err_str = error_to_string err in
      Alcotest.(check bool) "cyclic error has arrow" true
        (String.contains err_str '-')
  | Ok _ -> Alcotest.(check bool) "should not succeed" false true);
  
  let result3 = parse_circuit "valid" in
  Alcotest.(check bool) "valid circuit succeeds" true (Result.is_ok result3)

let test_error_propagation () =
  let result = 
    let* expr = parse_expr "test_var" in
    let* _circuit = parse_circuit "valid" in
    Ok expr
  in
  Alcotest.(check bool) "monadic success" true (Result.is_ok result);
  
  let result2 = 
    let* _expr = parse_expr "" in  (* this will fail *)
    let* _circuit = parse_circuit "valid" in
    Ok ()
  in
  Alcotest.(check bool) "monadic error propagation" true (Result.is_error result2)

let () =
  let open Alcotest in
  run "Parser Error Tests" [
    "parse_errors", [
      test_case "Expression parse errors" `Quick test_parse_expr_errors;
      test_case "Circuit parse errors" `Quick test_parse_circuit_errors;
      test_case "Error propagation" `Quick test_error_propagation;
    ];
  ]