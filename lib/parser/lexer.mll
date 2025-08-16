{
  open Tokens
  open Error

  let create_pos file line column = Error.make_pos file line column
  
  let current_file = ref "<input>"
  let current_line = ref 1
  let current_column = ref 1

  let set_source_file filename =
    current_file := filename;
    current_line := 1;
    current_column := 1

  let advance_column n = current_column := !current_column + n
  let advance_line () = 
    current_line := !current_line + 1; 
    current_column := 1

  let current_pos () = create_pos !current_file !current_line !current_column

  let create_token tok = {
    token = tok;
    position = current_pos ();
  }

  exception Lexer_error of string * Error.position
}

let whitespace = [' ' '\t']
let newline = '\n' | '\r' '\n'
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let identifier_start = letter | '_'
let identifier_char = letter | digit | '_'
let field_value = digit+
let comment_line = '#' [^ '\n' '\r']* | "//" [^ '\n' '\r']*

rule token = parse
  | whitespace+    { advance_column (String.length (Lexing.lexeme lexbuf)); token lexbuf }
  | newline        { advance_line (); create_token NEWLINE }
  | comment_line   { advance_column (String.length (Lexing.lexeme lexbuf)); token lexbuf }
  
  | identifier_start identifier_char* as s {
      advance_column (String.length s);
      create_token (lookup_keyword s)
    }
  
  | field_value as s {
      advance_column (String.length s);
      create_token (FIELD_VALUE s)
    }
  
  | '"' { 
      advance_column 1; 
      let str_content = string_literal "" lexbuf in
      create_token (STRING_LITERAL str_content)
    }
  
  | "==" { advance_column 2; create_token DOUBLE_EQUALS }
  | "="  { advance_column 1; create_token EQUALS }
  | "+"  { advance_column 1; create_token PLUS }
  | "-"  { advance_column 1; create_token MINUS }
  | "*"  { advance_column 1; create_token MULTIPLY }
  
  | "("  { advance_column 1; create_token LPAREN }
  | ")"  { advance_column 1; create_token RPAREN }
  | "["  { advance_column 1; create_token LBRACKET }
  | "]"  { advance_column 1; create_token RBRACKET }
  | "{"  { advance_column 1; create_token LBRACE }
  | "}"  { advance_column 1; create_token RBRACE }
  | ","  { advance_column 1; create_token COMMA }
  | ":"  { advance_column 1; create_token COLON }
  
  | eof  { create_token EOF }
  
  | _ as c {
      let pos = current_pos () in
      let msg = Printf.sprintf "Unexpected character '%c'" c in
      raise (Lexer_error (msg, pos))
    }

and string_literal acc = parse
  | '"' { 
      advance_column 1; 
      acc 
    }
  | '\\' 'n' { 
      advance_column 2; 
      string_literal (acc ^ "\n") lexbuf 
    }
  | '\\' 't' { 
      advance_column 2; 
      string_literal (acc ^ "\t") lexbuf 
    }
  | '\\' '\\' { 
      advance_column 2; 
      string_literal (acc ^ "\\") lexbuf 
    }
  | '\\' '"' { 
      advance_column 2; 
      string_literal (acc ^ "\"") lexbuf 
    }
  | newline { 
      advance_line (); 
      string_literal (acc ^ "\n") lexbuf 
    }
  | eof { 
      let pos = current_pos () in
      raise (Lexer_error ("Unterminated string literal", pos))
    }
  | _ as c { 
      advance_column 1; 
      string_literal (acc ^ String.make 1 c) lexbuf 
    }

{
  let tokenize_string ?(filename="<input>") input =
    set_source_file filename;
    let lexbuf = Lexing.from_string input in
    try
      let rec collect_tokens acc =
        let token_with_pos = token lexbuf in
        match token_with_pos.token with
        | EOF -> List.rev (token_with_pos :: acc)
        | _ -> collect_tokens (token_with_pos :: acc)
      in
      Ok (collect_tokens [])
    with
    | Lexer_error (msg, pos) -> 
        let error = parse_error msg 
          ~context:(Printf.sprintf "at line %d, column %d" pos.line pos.col)
          ~pos:pos () in
        Error error

  let tokenize_file filename =
    try
      let ic = open_in filename in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      tokenize_string ~filename content
    with
    | Sys_error msg -> 
        Error (parse_error ("File error: " ^ msg) ())
}