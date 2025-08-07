open Alcotest

let test_valid_circuit () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"; "y"];
    Ast.private_inputs = ["z"];
    Ast.body = [
      Ast.Assign ("sum", Ast.Add (Ast.Var "x", Ast.Var "y"));
      Ast.Assign ("product", Ast.Mul (Ast.Var "sum", Ast.Var "z"));
      Ast.Constraint (Ast.Equal (Ast.Var "product", Ast.Const "42"));
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> ()
  | Error err -> Alcotest.fail (Error.error_to_string err)

let test_undeclared_variable () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("result", Ast.Add (Ast.Var "x", Ast.Var "undeclared"));
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected type error for undeclared variable"
  | Error _ -> ()

let test_variable_before_definition () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("result", Ast.Add (Ast.Var "x", Ast.Var "y"));
      Ast.Assign ("y", Ast.Const "10");
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for using variable before definition"
  | Error _ -> ()

let test_duplicate_assignment () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("result", Ast.Var "x");
      Ast.Assign ("result", Ast.Var "x");
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for duplicate assignment"
  | Error _ -> ()

let test_duplicate_input () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"; "x"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("result", Ast.Var "x");
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for duplicate input"
  | Error _ -> ()

let test_input_private_overlap () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"];
    Ast.private_inputs = ["x"];
    Ast.body = [
      Ast.Assign ("result", Ast.Var "x");
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for input/private overlap"
  | Error _ -> ()

let test_cyclic_dependency () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("a", Ast.Add (Ast.Var "x", Ast.Var "b"));
      Ast.Assign ("b", Ast.Add (Ast.Var "x", Ast.Var "a"));
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for cyclic dependency"
  | Error _ -> ()

let test_poseidon_validation () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["preimage"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("hash", Ast.Poseidon [Ast.Var "preimage"]);
      Ast.Constraint (Ast.Equal (Ast.Var "hash", Ast.Const "12345"));
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> ()
  | Error err -> Alcotest.fail (Error.error_to_string err)

let test_empty_poseidon () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = [];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("hash", Ast.Poseidon []);
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for empty Poseidon arguments"
  | Error _ -> ()

let test_invalid_circuit_name () =
  let circuit = {
    Ast.name = "123invalid";
    Ast.inputs = ["x"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("result", Ast.Var "x");
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for invalid circuit name"
  | Error _ -> ()

let test_empty_circuit_body () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"];
    Ast.private_inputs = [];
    Ast.body = [];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for empty circuit body"
  | Error _ -> ()

let test_invalid_variable_name () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("123invalid", Ast.Var "x");
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for invalid variable name"
  | Error _ -> ()

let test_complex_valid_circuit () =
  let circuit = {
    Ast.name = "complex_circuit";
    Ast.inputs = ["a"; "b"; "c"];
    Ast.private_inputs = ["secret"];
    Ast.body = [
      Ast.Assign ("sum1", Ast.Add (Ast.Var "a", Ast.Var "b"));
      Ast.Assign ("sum2", Ast.Add (Ast.Var "sum1", Ast.Var "c"));
      Ast.Assign ("product", Ast.Mul (Ast.Var "sum2", Ast.Var "secret"));
      Ast.Assign ("hash", Ast.Poseidon [Ast.Var "product"; Ast.Var "secret"]);
      Ast.Constraint (Ast.Equal (Ast.Var "hash", Ast.Const "54321"));
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> ()
  | Error err -> Alcotest.fail (Error.error_to_string err)

let test_constraint_validation () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"; "y"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("sum", Ast.Add (Ast.Var "x", Ast.Var "y"));
      Ast.Constraint (Ast.Equal (Ast.Var "sum", Ast.Const "10"));
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> ()
  | Error err -> Alcotest.fail (Error.error_to_string err)

(* enhanced tests for new validation features *)
let test_assignment_to_input () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("x", Ast.Const "5"); (* trying to assign to input *)
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for assignment to input variable"
  | Error _ -> ()

let test_assignment_to_private_input () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = [];
    Ast.private_inputs = ["secret"];
    Ast.body = [
      Ast.Assign ("secret", Ast.Const "42"); (* trying to assign to private input *)
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for assignment to private input variable"
  | Error _ -> ()

let test_poseidon_too_many_inputs () =
  let many_inputs = List.init 20 (fun i -> Ast.Var ("input" ^ string_of_int i)) in
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = List.init 20 (fun i -> "input" ^ string_of_int i);
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("hash", Ast.Poseidon many_inputs); (* too many inputs *)
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for too many Poseidon inputs"
  | Error _ -> ()

let test_circuit_with_invalid_field_constant () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("result", Ast.Add (Ast.Var "x", Ast.Const "invalid_field"));
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for invalid field constant"
  | Error _ -> ()

let test_circuit_no_constraints () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("result", Ast.Var "x"); (* no constraints *)
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected warning/error for circuit with no constraints"
  | Error _ -> ()

let test_unused_inputs () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"; "unused_input"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("result", Ast.Var "x");
      Ast.Constraint (Ast.Equal (Ast.Var "result", Ast.Const "5"));
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for unused inputs"
  | Error _ -> ()

let test_reserved_keyword_as_variable () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("poseidon", Ast.Var "x"); (* using reserved keyword *)
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for using reserved keyword as variable"
  | Error _ -> ()

let test_circuit_name_with_spaces () =
  let circuit = {
    Ast.name = "invalid name"; (* spaces in name *)
    Ast.inputs = ["x"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("result", Ast.Var "x");
      Ast.Constraint (Ast.Equal (Ast.Var "result", Ast.Const "5"));
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for circuit name with spaces"
  | Error _ -> ()

let test_input_name_with_spaces () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["invalid input"]; (* spaces in input name *)
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Constraint (Ast.Equal (Ast.Const "1", Ast.Const "1"));
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for input name with spaces"
  | Error _ -> ()

let test_complex_dependency_validation () =
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = ["x"];
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Assign ("a", Ast.Add (Ast.Var "x", Ast.Var "b"));
      Ast.Assign ("b", Ast.Add (Ast.Var "x", Ast.Var "c"));
      Ast.Assign ("c", Ast.Var "x"); (* valid dependency chain *)
      Ast.Constraint (Ast.Equal (Ast.Var "a", Ast.Const "10"));
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for forward reference dependency"
  | Error _ -> ()

let test_large_circuit_inputs () =
  let many_inputs = List.init 1001 (fun i -> "input" ^ string_of_int i) in
  let circuit = {
    Ast.name = "test_circuit";
    Ast.inputs = many_inputs;
    Ast.private_inputs = [];
    Ast.body = [
      Ast.Constraint (Ast.Equal (Ast.Const "1", Ast.Const "1"));
    ];
  } in
  match Typing.type_check_with_context circuit with
  | Ok _ -> Alcotest.fail "Expected error for too many inputs"
  | Error _ -> ()

let basic_tests = [
  "valid_circuit", `Quick, test_valid_circuit;
  "undeclared_variable", `Quick, test_undeclared_variable;
  "variable_before_definition", `Quick, test_variable_before_definition;
  "duplicate_assignment", `Quick, test_duplicate_assignment;
  "duplicate_input", `Quick, test_duplicate_input;
  "input_private_overlap", `Quick, test_input_private_overlap;
  "cyclic_dependency", `Quick, test_cyclic_dependency;
  "poseidon_validation", `Quick, test_poseidon_validation;
  "empty_poseidon", `Quick, test_empty_poseidon;
]

let structure_tests = [
  "invalid_circuit_name", `Quick, test_invalid_circuit_name;
  "empty_circuit_body", `Quick, test_empty_circuit_body;
  "invalid_variable_name", `Quick, test_invalid_variable_name;
  "complex_valid_circuit", `Quick, test_complex_valid_circuit;
  "constraint_validation", `Quick, test_constraint_validation;
]

let enhanced_validation_tests = [
  "assignment_to_input", `Quick, test_assignment_to_input;
  "assignment_to_private_input", `Quick, test_assignment_to_private_input;
  "poseidon_too_many_inputs", `Quick, test_poseidon_too_many_inputs;
  "invalid_field_constant", `Quick, test_circuit_with_invalid_field_constant;
  "circuit_no_constraints", `Quick, test_circuit_no_constraints;
  "unused_inputs", `Quick, test_unused_inputs;
  "reserved_keyword_as_variable", `Quick, test_reserved_keyword_as_variable;
  "circuit_name_with_spaces", `Quick, test_circuit_name_with_spaces;
  "input_name_with_spaces", `Quick, test_input_name_with_spaces;
  "complex_dependency_validation", `Quick, test_complex_dependency_validation;
  "large_circuit_inputs", `Quick, test_large_circuit_inputs;
]

let () =
  run "Type Checking Tests" [
    "basic type checking", basic_tests;
    "structure validation", structure_tests;
    "enhanced validation", enhanced_validation_tests;
  ]