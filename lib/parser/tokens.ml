let dummy_pos = Error.make_pos "<input>" 1 1

type token = 
  | CIRCUIT
  | INPUTS
  | PRIVATE
  | ASSERT
  | POSEIDON
  | PUBLIC
  
  | IDENTIFIER of string
  | FIELD_VALUE of string
  | STRING_LITERAL of string
  
  | PLUS
  | MINUS
  | MULTIPLY
  | EQUALS
  | DOUBLE_EQUALS
  
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COMMA
  | COLON
  
  | EOF
  | NEWLINE

type token_with_pos = {
  token: token;
  position: Error.position;
}

let string_of_token = function
  | CIRCUIT -> "circuit"
  | INPUTS -> "inputs"
  | PRIVATE -> "private"
  | PUBLIC -> "public"
  | ASSERT -> "assert"
  | POSEIDON -> "poseidon"
  | IDENTIFIER s -> "identifier(" ^ s ^ ")"
  | FIELD_VALUE s -> "field_value(" ^ s ^ ")"
  | STRING_LITERAL s -> "string_literal(\"" ^ s ^ "\")"
  | PLUS -> "+"
  | MINUS -> "-"
  | MULTIPLY -> "*"
  | EQUALS -> "="
  | DOUBLE_EQUALS -> "=="
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACKET -> "["
  | RBRACKET -> "]"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | COMMA -> ","
  | COLON -> ":"
  | EOF -> "EOF"
  | NEWLINE -> "NEWLINE"

let keyword_table = [
  ("circuit", CIRCUIT);
  ("inputs", INPUTS);
  ("private", PRIVATE);
  ("public", PUBLIC);
  ("assert", ASSERT);
  ("poseidon", POSEIDON);
]

let keywords = 
  let tbl = Hashtbl.create 16 in
  List.iter (fun (kw, tok) -> Hashtbl.add tbl kw tok) keyword_table;
  tbl

let lookup_keyword s =
  match Hashtbl.find_opt keywords s with
  | Some tok -> tok
  | None -> IDENTIFIER s

let is_field_char c =
  match c with
  | '0'..'9' -> true
  | _ -> false

let is_identifier_start c =
  match c with
  | 'a'..'z' | 'A'..'Z' | '_' -> true
  | _ -> false

let is_identifier_char c =
  match c with
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
  | _ -> false