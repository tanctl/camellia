open Error

let demo_error_creation () =
  Printf.printf "=== Error Creation Demo ===\n";
  
  let pos = make_pos "demo.cam" 42 15 in
  
  let parse_err = unexpected_token "identifier" "123" ~pos 
    ~context:"while parsing variable declaration" () in
  Printf.printf "Parse Error:\n%s\n\n" (error_to_string parse_err);
  
  let type_err = type_mismatch "Field" "Bool" ~pos 
    ~context:"in arithmetic expression a + b" () in
  Printf.printf "Type Error:\n%s\n\n" (error_to_string type_err);
  
  let circuit_err = cyclic_dependency ["MainCircuit"; "SubCircuit"; "MainCircuit"] ~pos
    ~context:"during circuit dependency resolution" () in
  Printf.printf "Circuit Error:\n%s\n\n" (error_to_string circuit_err)

let demo_monadic_operations () =
  Printf.printf "=== Monadic Operations Demo ===\n";
  
  let pos = make_pos "demo.cam" 10 5 in
  
  let success_result = 
    let* x = Ok 10 in
    let* y = Ok 5 in
    let+ result = Ok (x + y) in
    result * 2
  in
  Printf.printf "Success: %s\n" 
    (match success_result with Ok v -> string_of_int v | Error _ -> "error");
  
  let error_result = 
    let* x = Ok 10 in
    let* _ = Error (division_by_zero ~pos ~context:"in computation" ()) in
    let+ y = Ok 5 in
    x + y
  in
  Printf.printf "Error: %s\n\n" 
    (match error_result with Ok _ -> "success" | Error e -> error_to_string e)

let demo_error_composition () =
  Printf.printf "=== Error Composition Demo ===\n";
  
  let pos = make_pos "demo.cam" 20 8 in
  
  let base_err = parse_error "unexpected end of file" ~pos () in
  
  let with_context = add_context base_err "while parsing circuit body" in
  Printf.printf "With Context:\n%s\n\n" (error_to_string with_context);
  
  let second_err = type_error "undefined variable" ~pos () in
  let chained = chain_error base_err second_err in
  Printf.printf "Chained Error:\n%s\n\n" (error_to_string chained)

let demo_result_collection () =
  Printf.printf "=== Result Collection Demo ===\n";
  
  let pos = make_pos "demo.cam" 5 1 in
  
  let results1 = [Ok 1; Ok 2; Ok 3; Ok 4] in
  let collected1 = collect_results results1 in
  (match collected1 with
  | Ok values -> Printf.printf "Collected: [%s]\n" 
      (String.concat "; " (List.map string_of_int values))
  | Error e -> Printf.printf "Collection failed: %s\n" (error_to_string e));
  
  let results2 = [
    Ok 1; 
    Error (invalid_field_element "bad_hex" ~pos ()); 
    Ok 3
  ] in
  let collected2 = collect_results results2 in
  (match collected2 with
  | Ok _ -> Printf.printf "Unexpected success\n"
  | Error e -> Printf.printf "Collection failed (expected): %s\n\n" (error_to_string e))

let demo_exception_handling () =
  Printf.printf "=== Exception Handling Demo ===\n";
  
  let pos = make_pos "demo.cam" 1 1 in
  
  let result1 = try_with (fun () -> 42 / 0) ~pos ~context:"during field division" () in
  (match result1 with
  | Ok _ -> Printf.printf "Unexpected success\n"
  | Error e -> Printf.printf "Exception caught: %s\n\n" (error_to_string e))

let () =
  demo_error_creation ();
  demo_monadic_operations ();
  demo_error_composition ();
  demo_result_collection ();
  demo_exception_handling ();