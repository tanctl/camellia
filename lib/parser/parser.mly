%{
  open Ast
  open Error

  (* use existing error position structure *)
  let make_position start_pos _end_pos =
    let start_col = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
    {
      file = start_pos.Lexing.pos_fname;
      line = start_pos.Lexing.pos_lnum;
      col = start_col;
    }

  let syntax_error msg start_pos end_pos =
    parse_error msg ~pos:(make_position start_pos end_pos) ()

  let recover_statement msg start_pos end_pos =
    let _err = syntax_error msg start_pos end_pos in
    (* allow parser to continue after errors *)
    Assign ("__error_recovery", Const "0")

  let recover_expr msg start_pos end_pos =
    let _err = syntax_error msg start_pos end_pos in
    Const "0"
%}

%token <string> IDENTIFIER FIELD_VALUE STRING_LITERAL
%token CIRCUIT INPUTS PRIVATE PUBLIC ASSERT POSEIDON
%token PLUS MINUS MULTIPLY EQUALS DOUBLE_EQUALS
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token COMMA COLON
%token EOF NEWLINE

/* precedence: lowest to highest */
%left DOUBLE_EQUALS
%left PLUS MINUS
%left MULTIPLY
%nonassoc UMINUS UPLUS

%type <Ast.circuit> circuit
%type <string list * string list * Ast.stmt list> circuit_body
%type <string list * string list> input_declarations
%type <string list * string list> input_declaration
%type <string list> identifier_list
%type <Ast.stmt list> statement_list
%type <Ast.stmt> statement
%type <Ast.stmt> assignment
%type <Ast.stmt> constraint_stmt
%type <Ast.expr> expression
%type <Ast.expr> primary_expr
%type <Ast.expr> function_call
%type <Ast.expr list> expression_list
%type <Ast.expr list> nonempty_expression_list
%type <unit> optional_newlines
%type <unit> newlines

%start circuit

%%

circuit:
  | optional_newlines CIRCUIT name=IDENTIFIER LBRACE circuit_body RBRACE optional_newlines EOF
    { 
      let (inputs, private_inputs, body) = $5 in
      { name; inputs; private_inputs; body }
    }
  | error
    { 
      let err = syntax_error "Invalid circuit definition" $startpos $endpos in
      failwith (error_to_string err)
    }

circuit_body:
  | optional_newlines input_declarations optional_newlines statement_list optional_newlines
    { 
      let (inputs, private_inputs) = $2 in
      (inputs, private_inputs, $4)
    }
  | optional_newlines statement_list optional_newlines
    { ([], [], $2) }
  | optional_newlines
    { ([], [], []) }

input_declarations:
  | input_declaration
    { $1 }
  | input_declarations newlines input_declaration
    { 
      let (inputs1, private1) = $1 in
      let (inputs2, private2) = $3 in
      (inputs1 @ inputs2, private1 @ private2)
    }

input_declaration:
  | PUBLIC INPUTS COLON identifier_list
    { ($4, []) }
  | INPUTS COLON identifier_list
    { ($3, []) }
  | PRIVATE INPUTS COLON identifier_list
    { ([], $4) }

identifier_list:
  | IDENTIFIER
    { [$1] }
  | identifier_list COMMA IDENTIFIER
    { $1 @ [$3] }

statement_list:
  | /* empty */
    { [] }
  | statement newlines statement_list
    { $1 :: $3 }
  | statement
    { [$1] }

statement:
  | assignment
    { $1 }
  | constraint_stmt
    { $1 }
  | error
    {
      recover_statement "Invalid statement" $startpos $endpos
    }

assignment:
  | IDENTIFIER EQUALS expression
    { Assign ($1, $3) }

constraint_stmt:
  | ASSERT expression
    { Constraint $2 }

expression:
  | expression PLUS expression
    { Add ($1, $3) }
  | expression MINUS expression  
    { Add ($1, Mul (Const "-1", $3)) }
  | expression MULTIPLY expression
    { Mul ($1, $3) }
  | expression DOUBLE_EQUALS expression
    { Equal ($1, $3) }
  | MINUS expression %prec UMINUS
    { Mul (Const "-1", $2) }
  | PLUS expression %prec UPLUS
    { $2 }
  | LPAREN expression RPAREN
    { $2 }
  | primary_expr
    { $1 }
  | error
    {
      recover_expr "Invalid expression" $startpos $endpos
    }

primary_expr:
  | IDENTIFIER
    { Var $1 }
  | FIELD_VALUE
    { Const $1 }
  | STRING_LITERAL
    { Const $1 }
  | function_call
    { $1 }

function_call:
  | POSEIDON LPAREN expression_list RPAREN
    { Poseidon $3 }
  | POSEIDON LPAREN LBRACKET expression_list RBRACKET RPAREN
    { Poseidon $4 }

expression_list:
  | /* empty */
    { [] }
  | nonempty_expression_list
    { $1 }

nonempty_expression_list:
  | expression
    { [$1] }
  | nonempty_expression_list COMMA expression
    { $1 @ [$3] }

optional_newlines:
  | /* empty */
    { () }
  | newlines
    { () }

newlines:
  | NEWLINE
    { () }
  | newlines NEWLINE
    { () }

%%