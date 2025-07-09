open Alcotest
open Ast
open Compiler

let test_constant_reuse () =
  let debug_ctx = Debug.create_context "test" in
  let ctx = create_compilation_context debug_ctx in
  
  let (result1, ctx1) = compile_expr ctx (Const "42") |> Result.get_ok in
  let (result2, ctx2) = compile_expr ctx1 (Const "42") |> Result.get_ok in

  
  check int "same wire ID for duplicate constant" result1.wire_id result2.wire_id;

  
  check int "first constant has constraints" 1 (List.length result1.constraints);
  check int "second constant has no constraints" 0 (List.length result2.constraints);

  
  check int "total R1CS constraints" 1 (List.length ctx2.r1cs.constraints)

let test_different_constants () =
  let debug_ctx = Debug.create_context "test" in
  let ctx = create_compilation_context debug_ctx in
  
  let (result1, ctx1) = compile_expr ctx (Const "42") |> Result.get_ok in
  let (result2, ctx2) = compile_expr ctx1 (Const "123") |> Result.get_ok in

  
  check bool "different wire IDs for different constants" true
    (result1.wire_id <> result2.wire_id);

  
  check int "first constant has constraints" 1 (List.length result1.constraints);
  check int "second constant has constraints" 1 (List.length result2.constraints);

  
  check int "total R1CS constraints" 2 (List.length ctx2.r1cs.constraints)

let test_complex_circuit_with_constant_reuse () =
  let debug_ctx = Debug.create_context "test" in
  
  let circuit = {
    name = "constant_reuse_test";
    inputs = ["x"];
    private_inputs = [];
    body = [
      Assign ("y", Add (Var "x", Const "1"));
      Assign ("z", Mul (Var "y", Const "1")); 
      Constraint (Equal (Var "z", Const "5"));
    ];
  } in
  
  let compiled = compile_circuit debug_ctx circuit |> Result.get_ok in

  
  check bool "circuit compiles successfully" true (compiled.total_constraints > 0);
  
  let stats = get_compilation_stats compiled in
  check bool "has reasonable constraint count" true (stats.num_constraints <= 10);
  
  Printf.printf "Constant reuse test: %d constraints generated\n" stats.num_constraints

let constant_reuse_tests = [
  "constant_reuse", `Quick, test_constant_reuse;
  "different_constants", `Quick, test_different_constants;
  "complex_circuit_constant_reuse", `Quick, test_complex_circuit_with_constant_reuse;
]

let () = run "Constant Reuse Tests" [
  "constant_reuse", constant_reuse_tests;
]