type position = {
  file: string;
  line: int;
  col: int;
}

let dummy_pos = { file = "<unknown>"; line = 0; col = 0 }

let make_pos file line col = { file; line; col }

let pp_position fmt pos =
  if pos.line = 0 then
    Format.fprintf fmt "%s" pos.file
  else
    Format.fprintf fmt "%s:%d:%d" pos.file pos.line pos.col

let position_to_string pos =
  Format.asprintf "%a" pp_position pos

type error_kind =
  | ParseError of string
  | UnexpectedToken of string * string
  | UnterminatedString
  | InvalidNumber of string
  
  | TypeError of string
  | UnboundVariable of string
  | TypeMismatch of string * string
  | ArityMismatch of int * int
  
  | CryptoError of string
  | InvalidFieldElement of string
  | PoseidonError of string
  
  | R1CSError of string
  | ConstraintUnsatisfiable of string
  | WitnessGenerationFailed of string
  
  | FieldError of string
  | DivisionByZero
  | InvalidFieldOperation of string
  
  | CircuitError of string
  | CircuitNotFound of string
  | DuplicateCircuit of string
  | CyclicDependency of string list
  | UndefinedInput of string

type error = {
  kind: error_kind;
  pos: position;
  msg: string option;
}

type 'a result = ('a, error) Result.t

let (let*) = Result.bind
let (let+) r f = Result.map f r

let (>>=) = Result.bind

let map = Result.map

let make_error kind pos ?msg () = {
  kind;
  pos;
  msg;
}

let error kind ?msg () = make_error kind dummy_pos ?msg ()

let pp_error_kind fmt = function
  | ParseError msg -> Format.fprintf fmt "Parse error: %s" msg
  | UnexpectedToken (expected, actual) -> 
      Format.fprintf fmt "Expected '%s', but found '%s'" expected actual
  | UnterminatedString -> Format.fprintf fmt "Unterminated string literal"
  | InvalidNumber s -> Format.fprintf fmt "Invalid number format: '%s'" s
  
  | TypeError msg -> Format.fprintf fmt "Type error: %s" msg
  | UnboundVariable var -> Format.fprintf fmt "Unbound variable: '%s'" var
  | TypeMismatch (expected, actual) -> 
      Format.fprintf fmt "Type mismatch: expected %s, got %s" expected actual
  | ArityMismatch (expected, actual) -> 
      Format.fprintf fmt "Arity mismatch: expected %d arguments, got %d" expected actual
  
  | CryptoError msg -> Format.fprintf fmt "Cryptographic error: %s" msg
  | InvalidFieldElement s -> Format.fprintf fmt "Invalid field element: '%s'" s
  | PoseidonError msg -> Format.fprintf fmt "Poseidon hash error: %s" msg
  
  | R1CSError msg -> Format.fprintf fmt "R1CS error: %s" msg
  | ConstraintUnsatisfiable constraint_ -> 
      Format.fprintf fmt "Constraint unsatisfiable: %s" constraint_
  | WitnessGenerationFailed msg -> 
      Format.fprintf fmt "Witness generation failed: %s" msg
  
  | FieldError msg -> Format.fprintf fmt "Field arithmetic error: %s" msg
  | DivisionByZero -> Format.fprintf fmt "Division by zero"
  | InvalidFieldOperation op -> 
      Format.fprintf fmt "Invalid field operation: %s" op
  
  | CircuitError msg -> Format.fprintf fmt "Circuit error: %s" msg
  | CircuitNotFound name -> Format.fprintf fmt "Circuit not found: '%s'" name
  | DuplicateCircuit name -> Format.fprintf fmt "Duplicate circuit definition: '%s'" name
  | CyclicDependency circuits -> 
      Format.fprintf fmt "Cyclic dependency in circuits: %s" 
        (String.concat " -> " circuits)
  | UndefinedInput var -> Format.fprintf fmt "Undefined input variable: '%s'" var

let pp_error fmt err =
  Format.fprintf fmt "%a: %a" pp_position err.pos pp_error_kind err.kind;
  match err.msg with
  | None -> ()
  | Some msg -> Format.fprintf fmt "\n  Context: %s" msg

let error_to_string err =
  Format.asprintf "%a" pp_error err

let parse_error msg ?pos ?context () = 
  let pos = Option.value pos ~default:dummy_pos in
  make_error (ParseError msg) pos ?msg:context ()

let unexpected_token expected actual ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (UnexpectedToken (expected, actual)) pos ?msg:context ()

let unterminated_string ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error UnterminatedString pos ?msg:context ()

let invalid_number s ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (InvalidNumber s) pos ?msg:context ()

let type_error msg ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (TypeError msg) pos ?msg:context ()

let unbound_variable var ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (UnboundVariable var) pos ?msg:context ()

let type_mismatch expected actual ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (TypeMismatch (expected, actual)) pos ?msg:context ()

let arity_mismatch expected actual ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (ArityMismatch (expected, actual)) pos ?msg:context ()

let crypto_error msg ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (CryptoError msg) pos ?msg:context ()

let invalid_field_element s ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (InvalidFieldElement s) pos ?msg:context ()

let poseidon_error msg ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (PoseidonError msg) pos ?msg:context ()

let r1cs_error msg ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (R1CSError msg) pos ?msg:context ()

let constraint_unsatisfiable constraint_ ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (ConstraintUnsatisfiable constraint_) pos ?msg:context ()

let witness_generation_failed msg ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (WitnessGenerationFailed msg) pos ?msg:context ()

let field_error msg ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (FieldError msg) pos ?msg:context ()

let division_by_zero ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error DivisionByZero pos ?msg:context ()

let invalid_field_operation op ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (InvalidFieldOperation op) pos ?msg:context ()

let circuit_error msg ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (CircuitError msg) pos ?msg:context ()

let circuit_not_found name ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (CircuitNotFound name) pos ?msg:context ()

let duplicate_circuit name ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (DuplicateCircuit name) pos ?msg:context ()

let cyclic_dependency circuits ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (CyclicDependency circuits) pos ?msg:context ()

let undefined_input var ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  make_error (UndefinedInput var) pos ?msg:context ()

let add_context err context = { err with msg = Some context }

let chain_error err1 err2 =
  let context = error_to_string err1 in
  add_context err2 context

let from_exn exn ?pos ?context () =
  let pos = Option.value pos ~default:dummy_pos in
  let msg = Printexc.to_string exn in
  make_error (ParseError ("Exception: " ^ msg)) pos ?msg:context ()

let try_with f ?pos ?context () =
  try Ok (f ()) with
  | exn -> Error (from_exn exn ?pos ?context ())

let collect_results results =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | (Ok x) :: rest -> loop (x :: acc) rest
    | (Error err) :: _ -> Error err
  in
  loop [] results

let collect_all_errors results =
  let oks = ref [] in
  let errors = ref [] in
  List.iter (function
    | Ok x -> oks := x :: !oks
    | Error err -> errors := err :: !errors
  ) results;
  match !errors with
  | [] -> Ok (List.rev !oks)
  | err :: _ -> Error err (* return first error for now *)

let bind_error f = function
  | Ok x -> f x
  | Error e -> Error e

let lift_result f x = Ok (f x)

let ok x = Ok x
let error_msg msg = Error (parse_error msg ())
let fail kind ?pos ?context () = 
  let pos = Option.value pos ~default:dummy_pos in
  Error (make_error kind pos ?msg:context ())

let format_errors errors =
  String.concat "\n" (List.map error_to_string errors)