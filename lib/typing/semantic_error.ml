open Error
open Types
open Type_errors

type semantic_error_kind =
  | UndeclaredVariable of string
  | RedefinedVariable of string
  | TypeIncompatible of camellia_type * camellia_type
  | InvalidOperation of string * camellia_type list
  | ArityError of string * int * int
  | ScopeViolation of string * string
  | CircuitStructureError of string
  | CryptoValidationError of string
  | DependencyError of string list

type semantic_error = {
  kind: semantic_error_kind;
  pos: position;
  context: string option;
  suggestions: string list;
}

let make_semantic_error kind pos ?context ?(suggestions=[]) () = {
  kind;
  pos;
  context;
  suggestions;
}

let pp_semantic_error_kind fmt = function
  | UndeclaredVariable var -> 
      Format.fprintf fmt "Undeclared variable '%s'" var
  | RedefinedVariable var -> 
      Format.fprintf fmt "Variable '%s' is already defined" var
  | TypeIncompatible (expected, actual) -> 
      Format.fprintf fmt "Type incompatible: expected %s, got %s" 
        (type_to_string expected) (type_to_string actual)
  | InvalidOperation (op, types) -> 
      Format.fprintf fmt "Invalid operation '%s' for types [%s]" op
        (String.concat "; " (List.map type_to_string types))
  | ArityError (op, expected, actual) -> 
      Format.fprintf fmt "Operation '%s' expects %d arguments, got %d" 
        op expected actual
  | ScopeViolation (var, scope) -> 
      Format.fprintf fmt "Variable '%s' not accessible in scope '%s'" var scope
  | CircuitStructureError msg -> 
      Format.fprintf fmt "Circuit structure error: %s" msg
  | CryptoValidationError msg -> 
      Format.fprintf fmt "Cryptographic validation error: %s" msg
  | DependencyError cycle -> 
      Format.fprintf fmt "Dependency cycle: %s" (String.concat " -> " cycle)

let pp_semantic_error fmt err =
  Format.fprintf fmt "%a: %a" pp_position err.pos pp_semantic_error_kind err.kind;
  (match err.context with
   | None -> ()
   | Some ctx -> Format.fprintf fmt "\n  Context: %s" ctx);
  (match err.suggestions with
   | [] -> ()
   | suggestions -> 
       Format.fprintf fmt "\n  Suggestions:\n";
       List.iter (fun s -> Format.fprintf fmt "    - %s\n" s) suggestions)

let semantic_error_to_string err =
  Format.asprintf "%a" pp_semantic_error err

let convert_to_error_system semantic_err =
  match semantic_err.kind with
  | UndeclaredVariable var -> 
      make_unbound_variable var semantic_err.pos
  | RedefinedVariable var -> 
      make_type_error (Printf.sprintf "Variable '%s' redefined" var) 
        semantic_err.pos
  | TypeIncompatible (expected, actual) -> 
      make_type_mismatch (type_to_string expected) (type_to_string actual) 
        semantic_err.pos
  | InvalidOperation (op, types) -> 
      make_type_error 
        (Printf.sprintf "Invalid operation '%s' for types [%s]" op
         (String.concat "; " (List.map type_to_string types)))
        semantic_err.pos
  | ArityError (_op, expected, actual) -> 
      make_arity_mismatch expected actual semantic_err.pos
  | ScopeViolation (var, scope) -> 
      make_type_error (Printf.sprintf "Variable '%s' not in scope '%s'" var scope)
        semantic_err.pos
  | CircuitStructureError msg -> 
      make_type_error msg semantic_err.pos
  | CryptoValidationError msg -> 
      make_crypto_error msg semantic_err.pos
  | DependencyError cycle -> 
      make_cyclic_dependency cycle semantic_err.pos

let undeclared_variable_error var pos ?context ?(suggestions=[]) () =
  make_semantic_error (UndeclaredVariable var) pos ?context ~suggestions ()

let redefined_variable_error var pos ?context ?(suggestions=[]) () =
  make_semantic_error (RedefinedVariable var) pos ?context ~suggestions ()

let type_incompatible_error expected actual pos ?context ?(suggestions=[]) () =
  make_semantic_error (TypeIncompatible (expected, actual)) pos ?context ~suggestions ()

let invalid_operation_error op types pos ?context ?(suggestions=[]) () =
  make_semantic_error (InvalidOperation (op, types)) pos ?context ~suggestions ()

let arity_error op expected actual pos ?context ?(suggestions=[]) () =
  make_semantic_error (ArityError (op, expected, actual)) pos ?context ~suggestions ()

let scope_violation_error var scope pos ?context ?(suggestions=[]) () =
  make_semantic_error (ScopeViolation (var, scope)) pos ?context ~suggestions ()

let circuit_structure_error msg pos ?context ?(suggestions=[]) () =
  make_semantic_error (CircuitStructureError msg) pos ?context ~suggestions ()

let crypto_validation_error msg pos ?context ?(suggestions=[]) () =
  make_semantic_error (CryptoValidationError msg) pos ?context ~suggestions ()

let dependency_error cycle pos ?context ?(suggestions=[]) () =
  make_semantic_error (DependencyError cycle) pos ?context ~suggestions ()

let suggest_similar_variables var available_vars =
  let distance s1 s2 =
    let len1 = String.length s1 in
    let len2 = String.length s2 in
    let matrix = Array.make_matrix (len1 + 1) (len2 + 1) 0 in
    
    for i = 0 to len1 do
      matrix.(i).(0) <- i
    done;
    for j = 0 to len2 do
      matrix.(0).(j) <- j
    done;
    
    for i = 1 to len1 do
      for j = 1 to len2 do
        let cost = if s1.[i-1] = s2.[j-1] then 0 else 1 in
        matrix.(i).(j) <- min (min
          (matrix.(i-1).(j) + 1)
          (matrix.(i).(j-1) + 1))
          (matrix.(i-1).(j-1) + cost)
      done
    done;
    matrix.(len1).(len2)
  in
  
  let suggestions = List.filter_map (fun v ->
    let dist = distance var v in
    if dist <= 2 && String.length v > 1 then Some v else None
  ) available_vars in
  
  List.sort compare suggestions

let enhanced_undeclared_variable_error var available_vars pos ?context () =
  let suggestions = suggest_similar_variables var available_vars in
  let suggestion_msgs = List.map (fun v -> Printf.sprintf "Did you mean '%s'?" v) suggestions in
  undeclared_variable_error var pos ?context ~suggestions:suggestion_msgs ()

(* enhanced error reporting with actionable suggestions *)
let enhanced_type_mismatch_error expected _actual operation pos () =
  let context = Printf.sprintf "Operation '%s' requires %s operands" operation expected in
  let suggestions = match operation with
    | "addition" | "multiplication" -> 
        ["Ensure both operands are field elements"; 
         "Check variable assignments and input declarations"]
    | "equality" -> 
        ["Both sides of equality must be field elements";
         "Use '==' for equality comparison, not '='"]
    | "poseidon" -> 
        ["Poseidon hash requires field element inputs";
         "Check that all inputs are properly typed"]
    | _ -> ["Check that all operands have compatible types"]
  in
  type_incompatible_error (FieldType) (UnknownType) pos ~context ~suggestions ()

let enhanced_arity_error operation expected actual pos () =
  let context = Printf.sprintf "%s operation arity mismatch" operation in
  let suggestions = match operation with
    | "poseidon" when actual = 0 -> 
        ["Poseidon hash requires at least one input";
         "Add input arguments: poseidon(input1, input2, ...)"]
    | "poseidon" when actual > 16 -> 
        ["Poseidon hash supports at most 16 inputs for security";
         "Consider splitting into multiple hash operations"]
    | "addition" | "multiplication" when actual < 2 -> 
        [Printf.sprintf "%s requires exactly 2 operands" operation;
         "Check the expression syntax"]
    | _ -> [Printf.sprintf "Operation '%s' expects %d arguments, provide exactly %d" 
             operation expected expected]
  in
  arity_error operation expected actual pos ~context ~suggestions ()

let enhanced_circuit_structure_error msg circuit_name pos () =
  let context = Printf.sprintf "Circuit '%s' has structural issues" circuit_name in
  let suggestions = 
    if String.contains msg 'e' then 
      ["Add at least one assignment or constraint to the circuit";
       "Circuits need statements to be compilable to R1CS"]
    else if String.contains msg 'c' then 
      ["Add at least one 'assert' statement to create meaningful constraints";
       "Zero-knowledge proofs require constraints to prove statements"]
    else if String.contains msg 'i' then 
      ["Declare circuit inputs with 'public inputs:' or 'private inputs:'";
       "Inputs are necessary for meaningful zero-knowledge circuits"]
    else 
      ["Check the circuit syntax and structure";
       "Ensure all required components are present"]
  in
  circuit_structure_error msg pos ~context ~suggestions ()

let enhanced_dependency_error cycle pos () =
  let context = "Circular dependencies between variables prevent compilation" in
  let suggestions = [
    "Reorder assignments to remove circular references";
    "Each variable must be assigned before it is used";
    "Check the dependency chain: " ^ String.concat " -> " cycle
  ] in
  dependency_error cycle pos ~context ~suggestions ()

(* comprehensive error reporting with fixes *)
let comprehensive_error_report error_type details pos () =
  match error_type with
  | "parsing" -> 
      let context = "Syntax error in circuit definition" in
      let suggestions = [
        "Check circuit syntax: 'circuit name { ... }'";
        "Ensure proper use of keywords: inputs, private, assert";
        "Verify all brackets and parentheses are balanced"
      ] in
      circuit_structure_error details pos ~context ~suggestions ()
  
  | "scoping" -> 
      let context = "Variable scoping error" in
      let suggestions = [
        "Variables must be declared before use";
        "Check input declarations and assignments";
        "Ensure no forward references"
      ] in
      circuit_structure_error details pos ~context ~suggestions ()
  
  | "typing" -> 
      let context = "Type system validation failed" in
      let suggestions = [
        "All expressions must evaluate to field elements";
        "Check arithmetic operations and function calls";
        "Verify constraint expressions are boolean-equivalent"
      ] in
      circuit_structure_error details pos ~context ~suggestions ()
  
  | _ -> 
      circuit_structure_error details pos ()