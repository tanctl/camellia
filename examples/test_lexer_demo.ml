open Error

let test_lexer_basic () =
  Printf.printf "=== Testing Camellia Lexer ===\n";
  
  let test_inputs = [
    ("circuit", "Keyword: circuit");
    ("hash_preimage", "Identifier: hash_preimage");  
    ("42", "Field value: 42");
    ("\"hello world\"", "String literal");
    ("+", "Plus operator");
    ("*", "Multiply operator");
    ("==", "Double equals");
    ("(", "Left parenthesis");
    (")", "Right parenthesis");
    ("{", "Left brace");
    ("}", "Right brace");
    ("circuit hash_preimage { }", "Complete simple circuit");
  ] in
  
  List.iter (fun (input, description) ->
    Printf.printf "\n--- Testing: %s ---\n" description;
    Printf.printf "Input: %s\n" input;
    
    match Parser.parse_expr input with
    | Ok expr -> 
        Printf.printf "Success! Parsed expression: %s\n" (Ast.expr_to_string expr)
    | Error err ->
        Printf.printf "Error: %s\n" (error_to_string err)
  ) test_inputs;
  
  Printf.printf "\n=== Lexer Testing Complete ===\n"

let () = 
  try
    test_lexer_basic ()
  with
  | exn ->
      Printf.printf "Exception during lexer testing: %s\n" (Printexc.to_string exn)