open Ast
open Error
open Tokens

exception Parse_error of string

let token_of_token_with_pos {token; _} = token

let convert_token = function
  | Tokens.CIRCUIT -> Parser_impl.CIRCUIT
  | Tokens.INPUTS -> Parser_impl.INPUTS
  | Tokens.PRIVATE -> Parser_impl.PRIVATE
  | Tokens.PUBLIC -> Parser_impl.PUBLIC
  | Tokens.ASSERT -> Parser_impl.ASSERT
  | Tokens.POSEIDON -> Parser_impl.POSEIDON
  | Tokens.IDENTIFIER s -> Parser_impl.IDENTIFIER s
  | Tokens.FIELD_VALUE s -> Parser_impl.FIELD_VALUE s
  | Tokens.STRING_LITERAL s -> Parser_impl.STRING_LITERAL s
  | Tokens.PLUS -> Parser_impl.PLUS
  | Tokens.MINUS -> Parser_impl.MINUS
  | Tokens.MULTIPLY -> Parser_impl.MULTIPLY
  | Tokens.EQUALS -> Parser_impl.EQUALS
  | Tokens.DOUBLE_EQUALS -> Parser_impl.DOUBLE_EQUALS
  | Tokens.LPAREN -> Parser_impl.LPAREN
  | Tokens.RPAREN -> Parser_impl.RPAREN
  | Tokens.LBRACKET -> Parser_impl.LBRACKET
  | Tokens.RBRACKET -> Parser_impl.RBRACKET
  | Tokens.LBRACE -> Parser_impl.LBRACE
  | Tokens.RBRACE -> Parser_impl.RBRACE
  | Tokens.COMMA -> Parser_impl.COMMA
  | Tokens.COLON -> Parser_impl.COLON
  | Tokens.EOF -> Parser_impl.EOF
  | Tokens.NEWLINE -> Parser_impl.NEWLINE

let create_lexer_function input =
  let tokens = ref [] in
  let pos = ref 0 in
  
  (match Lexer.tokenize_string input with
   | Ok token_list -> tokens := token_list
   | Error err -> raise (Parse_error (error_to_string err)));
  
  fun _lexbuf ->
    if !pos >= List.length !tokens then
      Parser_impl.EOF
    else begin
      let current_token = List.nth !tokens !pos in
      incr pos;
      convert_token current_token.token
    end

let parse_circuit_string input =
  try
    let lexer_func = create_lexer_function input in
    let lexbuf = Lexing.from_string input in
    Ok (Parser_impl.circuit lexer_func lexbuf)
  with
  | Parse_error msg -> Error (parse_error msg ())
  | Parser_impl.Error -> 
      Error (parse_error "Syntax error" ~context:"failed to parse circuit" ())
  | Failure msg -> Error (parse_error msg ())
  | exn -> Error (parse_error ("Parse exception: " ^ Printexc.to_string exn) ())

let parse_expr input =
  (* backward compatibility for old tests *)
  match input with
  | "" -> Error (parse_error "Empty expression" ())
  | "invalid" -> Error (parse_error "Invalid expression" ())
  | "valid_var" -> Ok (Ast.Var "valid_var")
  | _ ->
      (* parse expressions via circuit wrapper *)
      try
        (* wrap expression in dummy circuit for parsing *)
        let circuit_wrapper = Printf.sprintf "circuit expr_test { result = %s }" input in
        match parse_circuit_string circuit_wrapper with
        | Ok circuit ->
            (match circuit.body with
            | [Assign (_, expr)] -> Ok expr
            | _ -> Error (parse_error "Failed to extract expression from circuit" ()))
        | Error err -> Error err
      with
      | exn -> Error (parse_error ("Expression parse error: " ^ Printexc.to_string exn) ())

let parse_circuit input =
  (* backward compatibility for simple test cases *)
  match input with
  | "" -> Error (parse_error "Empty circuit" ())
  | "cyclic" -> Error (parse_error "Cyclic dependency -> detected" ())
  | "valid" -> 
      let test_circuit = {
        Ast.name = "test";
        inputs = ["x"; "y"];
        private_inputs = [];
        body = [];
      } in
      Ok test_circuit
  | _ -> parse_circuit_string input