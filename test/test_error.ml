open Error

let test_position () =
  let dummy = dummy_pos in
  Alcotest.(check string) "dummy position" "<unknown>" (position_to_string dummy);
  
  let pos = make_pos "test.cam" 10 5 in
  Alcotest.(check string) "specific position" "test.cam:10:5" (position_to_string pos);
  
  let zero_pos = make_pos "test.cam" 0 0 in
  Alcotest.(check string) "zero line position" "test.cam" (position_to_string zero_pos)

let test_error_creation () =
  let pos = make_pos "test.cam" 5 10 in
  
  let err1 = parse_error "syntax error" ~pos () in
  let err_str = error_to_string err1 in
  Alcotest.(check bool) "contains position" true 
    (String.contains err_str ':');
  Alcotest.(check bool) "contains error message" true
    (String.contains err_str 's');
  
  let err2 = type_mismatch "int" "string" ~pos ~context:"in function call" () in
  let err2_str = error_to_string err2 in
  Alcotest.(check bool) "contains context" true
    (String.contains err2_str 'C')

let test_error_types () =
  let pos = make_pos "test.cam" 1 1 in
  
  let parse_err = unexpected_token ")" ";" ~pos () in
  let parse_str = error_to_string parse_err in
  Alcotest.(check bool) "unexpected token format" true
    (String.contains parse_str '\'');
  
  let type_err = unbound_variable "undefined_var" ~pos () in
  let type_str = error_to_string type_err in
  Alcotest.(check bool) "unbound variable format" true
    (String.contains type_str 'u');
  
  let arity_err = arity_mismatch 3 2 ~pos () in
  let arity_str = error_to_string arity_err in
  Alcotest.(check bool) "arity mismatch format" true
    (String.contains arity_str '3');
  
  let crypto_err = invalid_field_element "invalid_hex" ~pos () in
  let crypto_str = error_to_string crypto_err in
  Alcotest.(check bool) "field element format" true
    (String.contains crypto_str 'i');
  
  let div_err = division_by_zero ~pos () in
  let div_str = error_to_string div_err in
  Alcotest.(check bool) "division by zero format" true
    (String.contains div_str 'z');
  
  let circuit_err = circuit_not_found "missing_circuit" ~pos () in
  let circuit_str = error_to_string circuit_err in
  Alcotest.(check bool) "circuit not found format" true
    (String.contains circuit_str 'm');
  
  let cyclic_err = cyclic_dependency ["A"; "B"; "A"] ~pos () in
  let cyclic_str = error_to_string cyclic_err in
  Alcotest.(check bool) "cyclic dependency format" true
    (String.contains cyclic_str 'A')

let test_monadic_operations () =
  let pos = make_pos "test.cam" 1 1 in
  
  let result1 = 
    let* x = Ok 5 in
    let* y = Ok 3 in
    Ok (x + y)
  in
  Alcotest.(check (result int string)) "successful binding" (Ok 8) 
    (Result.map_error error_to_string result1);
  
  let result2 = 
    let* x = Ok 5 in
    let* _ = Error (parse_error "test error" ~pos ()) in
    Ok x
  in
  Alcotest.(check bool) "error propagation" true
    (Result.is_error result2);
  
  let result3 = 
    let+ x = Ok 10 in
    x * 2
  in
  Alcotest.(check (result int string)) "successful mapping" (Ok 20)
    (Result.map_error error_to_string result3);
  
  let result4 = Ok 5 >>= fun x -> Ok (x + 1) in
  Alcotest.(check (result int string)) "bind operator" (Ok 6)
    (Result.map_error error_to_string result4)

let test_error_composition () =
  let pos = make_pos "test.cam" 1 1 in
  
  let base_err = parse_error "base error" ~pos () in
  let with_context = add_context base_err "additional context" in
  let context_str = error_to_string with_context in
  Alcotest.(check bool) "has additional context" true
    (String.contains context_str 'a');
  
  let err1 = type_error "first error" ~pos () in
  let err2 = parse_error "second error" ~pos () in
  let chained = chain_error err1 err2 in
  let chained_str = error_to_string chained in
  Alcotest.(check bool) "chained error contains both" true
    (String.length chained_str > String.length (error_to_string err2))

let test_exception_handling () =
  let pos = make_pos "test.cam" 1 1 in
  
  let exn = Invalid_argument "test exception" in
  let err = from_exn exn ~pos () in
  let err_str = error_to_string err in
  Alcotest.(check bool) "exception conversion" true
    (String.contains err_str 'E');
  
  let result1 = try_with (fun () -> 42) ~pos () in
  Alcotest.(check (result int string)) "try_with success" (Ok 42)
    (Result.map_error error_to_string result1);
  
  let result2 = try_with (fun () -> failwith "test failure") ~pos () in
  Alcotest.(check bool) "try_with failure" true
    (Result.is_error result2)

let test_result_collection () =
  let results1 = [Ok 1; Ok 2; Ok 3] in
  let collected1 = collect_results results1 in
  Alcotest.(check (result (list int) string)) "collect success" (Ok [1; 2; 3])
    (Result.map_error error_to_string collected1);
  
  let pos = make_pos "test.cam" 1 1 in
  let err = parse_error "test error" ~pos () in
  let results2 = [Ok 1; Error err; Ok 3] in
  let collected2 = collect_results results2 in
  Alcotest.(check bool) "collect with error" true
    (Result.is_error collected2);
  
  let results3 = [Ok 1; Ok 2] in
  let collected3 = collect_all_errors results3 in
  Alcotest.(check (result (list int) string)) "collect all success" (Ok [1; 2])
    (Result.map_error error_to_string collected3)

let test_convenience_functions () =
  let result1 = ok 42 in
  Alcotest.(check (result int string)) "ok function" (Ok 42)
    (Result.map_error error_to_string result1);
  
  let result2 = error_msg "test message" in
  Alcotest.(check bool) "error_msg function" true
    (Result.is_error result2);
  
  let pos = make_pos "test.cam" 1 1 in
  let result3 = fail (TypeError "test type error") ~pos () in
  Alcotest.(check bool) "fail function" true
    (Result.is_error result3)

let test_error_message_quality () =
  let pos = make_pos "circuit.cam" 15 8 in
  
  let parse_err = unexpected_token "identifier" "number" ~pos 
    ~context:"while parsing variable declaration" () in
  let parse_msg = error_to_string parse_err in
  Alcotest.(check bool) "parse error has location" true
    (String.contains parse_msg '1');
  Alcotest.(check bool) "parse error has expectation" true
    (String.contains parse_msg 'i');
  
  let type_err = type_mismatch "Field" "Bool" ~pos 
    ~context:"in arithmetic expression" () in
  let type_msg = error_to_string type_err in
  Alcotest.(check bool) "type error has types" true
    (String.contains type_msg 'F');
  Alcotest.(check bool) "type error has context" true
    (String.contains type_msg 'a');
  
  let circuit_err = cyclic_dependency ["MainCircuit"; "SubCircuit"; "MainCircuit"] ~pos
    ~context:"during circuit dependency resolution" () in
  let circuit_msg = error_to_string circuit_err in
  Alcotest.(check bool) "circuit error shows cycle" true
    (String.contains circuit_msg 'M');
  Alcotest.(check bool) "circuit error has arrow" true
    (String.contains circuit_msg '-')

let () =
  let open Alcotest in
  run "Error System Tests" [
    "position", [
      test_case "Position creation and formatting" `Quick test_position;
    ];
    "error_creation", [
      test_case "Error creation and formatting" `Quick test_error_creation;
      test_case "Specific error types" `Quick test_error_types;
    ];
    "monadic", [
      test_case "Monadic operations" `Quick test_monadic_operations;
    ];
    "composition", [
      test_case "Error composition" `Quick test_error_composition;
      test_case "Exception handling" `Quick test_exception_handling;
      test_case "Result collection" `Quick test_result_collection;
    ];
    "convenience", [
      test_case "Convenience functions" `Quick test_convenience_functions;
    ];
    "quality", [
      test_case "Error message quality" `Quick test_error_message_quality;
    ];
  ]