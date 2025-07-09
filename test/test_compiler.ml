open Alcotest
open Error
open Ast
open Compiler

let test_compilation_context () =
  let debug_ctx = Debug.create_context "test" in
  let ctx = create_compilation_context debug_ctx in
  
  let (wire_id, new_ctx) = allocate_wire ctx "test_wire" R1cs.Input () |> Result.get_ok in
  check int "allocated wire ID" 1 wire_id;
  check int "wire count" 1 (List.length new_ctx.r1cs.wires);
  
  let bound_ctx = bind_variable new_ctx "x" wire_id |> Result.get_ok in
  let looked_up = lookup_variable bound_ctx "x" |> Result.get_ok in
  check int "variable lookup" wire_id looked_up

let test_scope_management () =
  let debug_ctx = Debug.create_context "test" in
  let ctx = create_compilation_context debug_ctx in
  
  let (wire1, ctx1) = allocate_wire ctx "x" R1cs.Input () |> Result.get_ok in
  let ctx2 = bind_variable ctx1 "x" wire1 |> Result.get_ok in
  
  let ctx3 = push_scope ctx2 in
  let (wire2, ctx4) = allocate_wire ctx3 "x_inner" R1cs.Private () |> Result.get_ok in
  let ctx5 = bind_variable ctx4 "x" wire2 |> Result.get_ok in
  
  let inner_x = lookup_variable ctx5 "x" |> Result.get_ok in
  check int "inner scope variable" wire2 inner_x;
  
  let ctx6 = pop_scope ctx5 in
  let outer_x = lookup_variable ctx6 "x" |> Result.get_ok in
  check int "outer scope restored" wire1 outer_x

let test_compile_const () =
  let debug_ctx = Debug.create_context "test" in
  let ctx = create_compilation_context debug_ctx in
  
  let (result, new_ctx) = compile_expr ctx (Const "42") |> Result.get_ok in
  
  check int "constraint count" 1 (List.length result.constraints);
  check int "total constraints in R1CS" 1 (List.length new_ctx.r1cs.constraints);
  
  let wire_info = R1cs.get_wire new_ctx.wire_manager result.wire_id |> Result.get_ok in
  let starts_with_const_42 = String.length wire_info.name >= 8 && 
    String.sub wire_info.name 0 8 = "const_42" in
  check bool "wire name starts with const_42" true starts_with_const_42;
  check bool "wire is constant" true (wire_info.wire_type = R1cs.Constant)

let test_compile_var () =
  let debug_ctx = Debug.create_context "test" in
  let ctx = create_compilation_context debug_ctx in
  
  let (wire_id, ctx1) = allocate_wire ctx "x" R1cs.Input () |> Result.get_ok in
  let ctx2 = bind_variable ctx1 "x" wire_id |> Result.get_ok in
  
  let (result, _new_ctx) = compile_expr ctx2 (Var "x") |> Result.get_ok in
  
  check int "variable wire ID" wire_id result.wire_id;
  check int "no constraints for variable reference" 0 (List.length result.constraints)

let test_unbound_variable () =
  let debug_ctx = Debug.create_context "test" in
  let ctx = create_compilation_context debug_ctx in
  
  let result = compile_expr ctx (Var "undefined") in
  check bool "unbound variable error" true (Result.is_error result);
  
  match result with
  | Error err -> 
      (match err.kind with
      | UnboundVariable var -> check string "error variable name" "undefined" var
      | _ -> Alcotest.fail "Wrong error type")
  | Ok _ -> Alcotest.fail "Should have failed"

let test_compile_add () =
  let debug_ctx = Debug.create_context "test" in
  let ctx = create_compilation_context debug_ctx in
  
  (* setup variables *)
  let (wire1, ctx1) = allocate_wire ctx "a" R1cs.Input () |> Result.get_ok in
  let ctx2 = bind_variable ctx1 "a" wire1 |> Result.get_ok in
  let (wire2, ctx3) = allocate_wire ctx2 "b" R1cs.Input () |> Result.get_ok in
  let ctx4 = bind_variable ctx3 "b" wire2 |> Result.get_ok in
  
  let (result, new_ctx) = compile_expr ctx4 (Add (Var "a", Var "b")) |> Result.get_ok in
  
  check int "addition constraint count" 1 (List.length result.constraints);
  check int "total R1CS constraints" 1 (List.length new_ctx.r1cs.constraints);
  
  (* check wire allocation for sum *)
  let wire_info = R1cs.get_wire new_ctx.wire_manager result.wire_id |> Result.get_ok in
  check bool "sum wire is intermediate" true (wire_info.wire_type = R1cs.Intermediate);
  check bool "wire name contains add" true (String.contains wire_info.name 'a')

let test_compile_mul () =
  let debug_ctx = Debug.create_context "test" in
  let ctx = create_compilation_context debug_ctx in
  
  (* setup variables *)
  let (wire1, ctx1) = allocate_wire ctx "x" R1cs.Input () |> Result.get_ok in
  let ctx2 = bind_variable ctx1 "x" wire1 |> Result.get_ok in
  let (wire2, ctx3) = allocate_wire ctx2 "y" R1cs.Input () |> Result.get_ok in
  let ctx4 = bind_variable ctx3 "y" wire2 |> Result.get_ok in
  
  let (result, new_ctx) = compile_expr ctx4 (Mul (Var "x", Var "y")) |> Result.get_ok in
  
  check int "multiplication constraint count" 1 (List.length result.constraints);
  check int "total R1CS constraints" 1 (List.length new_ctx.r1cs.constraints);
  
  (* verify constraint structure *)
  let constraint_triple = List.hd result.constraints in
  check bool "multiplication constraint A has x" true 
    (List.exists (fun (w, _) -> w = wire1) constraint_triple.a);
  check bool "multiplication constraint B has y" true 
    (List.exists (fun (w, _) -> w = wire2) constraint_triple.b)

let test_complex_expression () =
  let debug_ctx = Debug.create_context "test" in
  let ctx = create_compilation_context debug_ctx in
  
  (* setup variables: x, y, z *)
  let (x_wire, ctx1) = allocate_wire ctx "x" R1cs.Input () |> Result.get_ok in
  let ctx2 = bind_variable ctx1 "x" x_wire |> Result.get_ok in
  let (y_wire, ctx3) = allocate_wire ctx2 "y" R1cs.Input () |> Result.get_ok in
  let ctx4 = bind_variable ctx3 "y" y_wire |> Result.get_ok in
  let (z_wire, ctx5) = allocate_wire ctx4 "z" R1cs.Input () |> Result.get_ok in
  let ctx6 = bind_variable ctx5 "z" z_wire |> Result.get_ok in
  
  (* compile: (x + y) * z *)
  let expr = Mul (Add (Var "x", Var "y"), Var "z") in
  let (result, new_ctx) = compile_expr ctx6 expr |> Result.get_ok in
  
  check int "complex expression constraint count" 2 (List.length result.constraints);
  check int "total R1CS constraints" 2 (List.length new_ctx.r1cs.constraints);
  
  let stats = R1cs.get_r1cs_statistics new_ctx.r1cs in
  check bool "has intermediate wires" true (stats.num_variables >= 5) (* 3 inputs + 2 intermediates *)

let test_assignment_statement () =
  let debug_ctx = Debug.create_context "test" in
  let ctx = create_compilation_context debug_ctx in
  
  (* setup input *)
  let (x_wire, ctx1) = allocate_wire ctx "x" R1cs.Input () |> Result.get_ok in
  let ctx2 = bind_variable ctx1 "x" x_wire |> Result.get_ok in
  
  let assign_stmt = Assign ("y", Add (Var "x", Const "5")) in
  let ctx_result = compile_stmt ctx2 assign_stmt |> Result.get_ok in
  
  (* check that y is now bound *)
  let y_wire = lookup_variable ctx_result "y" |> Result.get_ok in
  check bool "y wire allocated" true (y_wire > 0);
  
  check bool "has constraints" true (List.length ctx_result.r1cs.constraints >= 2)

let test_constraint_statement () =
  let debug_ctx = Debug.create_context "test" in
  let ctx = create_compilation_context debug_ctx in
  
  (* setup variables for equality *)
  let (a_wire, ctx1) = allocate_wire ctx "a" R1cs.Input () |> Result.get_ok in
  let ctx2 = bind_variable ctx1 "a" a_wire |> Result.get_ok in
  let (b_wire, ctx3) = allocate_wire ctx2 "b" R1cs.Input () |> Result.get_ok in
  let ctx4 = bind_variable ctx3 "b" b_wire |> Result.get_ok in
  
  let constraint_stmt = Constraint (Equal (Var "a", Var "b")) in
  let ctx_result = compile_stmt ctx4 constraint_stmt |> Result.get_ok in
  
  check bool "constraint statement adds R1CS constraints" true 
    (List.length ctx_result.r1cs.constraints > List.length ctx4.r1cs.constraints)

let test_simple_circuit_compilation () =
  let debug_ctx = Debug.create_context "test" in
  
  (* create simple circuit: multiply two inputs *)
  let circuit = {
    name = "test_multiply";
    inputs = ["a"; "b"];
    private_inputs = [];
    body = [
      Assign ("product", Mul (Var "a", Var "b"));
      Constraint (Equal (Var "product", Const "42"))
    ];
  } in
  
  let compiled = compile_circuit debug_ctx circuit |> Result.get_ok in
  
  (* verify structure *)
  check int "input count" 2 (List.length compiled.input_assignments);
  check int "private count" 0 (List.length compiled.private_assignments);
  check bool "has constraints" true (compiled.total_constraints > 0);
  
  (* check input wire assignments *)
  let input_names = List.map fst compiled.input_assignments in
  check bool "has input a" true (List.mem "a" input_names);
  check bool "has input b" true (List.mem "b" input_names);
  
  let stats = get_compilation_stats compiled in
  check int "total inputs in stats" 2 stats.num_inputs

let test_circuit_with_private_inputs () =
  let debug_ctx = Debug.create_context "test" in
  
  let circuit = {
    name = "private_test";
    inputs = ["public_input"];
    private_inputs = ["secret"];
    body = [
      Assign ("sum", Add (Var "public_input", Var "secret"));
    ];
  } in
  
  let compiled = compile_circuit debug_ctx circuit |> Result.get_ok in
  
  check int "public inputs" 1 (List.length compiled.input_assignments);
  check int "private inputs" 1 (List.length compiled.private_assignments);
  
  let stats = get_compilation_stats compiled in
  check int "stats public inputs" 1 stats.num_inputs;
  check int "stats private inputs" 1 stats.num_private

let test_poseidon_compilation () =
  let debug_ctx = Debug.create_context "test" in
  let ctx = create_compilation_context debug_ctx in
  
  (* setup inputs *)
  let (a_wire, ctx1) = allocate_wire ctx "a" R1cs.Input () |> Result.get_ok in
  let ctx2 = bind_variable ctx1 "a" a_wire |> Result.get_ok in
  let (b_wire, ctx3) = allocate_wire ctx2 "b" R1cs.Input () |> Result.get_ok in
  let ctx4 = bind_variable ctx3 "b" b_wire |> Result.get_ok in
  
  let poseidon_expr = Poseidon [Var "a"; Var "b"] in
  let (result, new_ctx) = compile_expr ctx4 poseidon_expr |> Result.get_ok in
  
  check int "poseidon constraint count" 1 (List.length result.constraints);
  check int "total R1CS constraints" 1 (List.length new_ctx.r1cs.constraints);
  
  let wire_info = R1cs.get_wire new_ctx.wire_manager result.wire_id |> Result.get_ok in
  check bool "poseidon wire name" true (String.contains wire_info.name 'p') (* contains "poseidon" *)

let test_error_propagation () =
  let debug_ctx = Debug.create_context "test" in
  
  let bad_circuit = {
    name = "bad_circuit";
    inputs = [];
    private_inputs = [];
    body = [
      Assign ("result", Var "undefined_var")
    ];
  } in
  
  let result = compile_circuit debug_ctx bad_circuit in
  check bool "bad circuit compilation fails" true (Result.is_error result);
  
  match result with
  | Error err ->
      (match err.kind with
      | UnboundVariable _ -> () (* expected *)
      | _ -> Alcotest.fail "Wrong error type for undefined variable")
  | Ok _ -> Alcotest.fail "Should have failed with undefined variable"

let compilation_tests = [
  "compilation_context", `Quick, test_compilation_context;
  "scope_management", `Quick, test_scope_management;
  "compile_const", `Quick, test_compile_const;
  "compile_var", `Quick, test_compile_var;
  "unbound_variable", `Quick, test_unbound_variable;
  "compile_add", `Quick, test_compile_add;
  "compile_mul", `Quick, test_compile_mul;
  "complex_expression", `Quick, test_complex_expression;
  "assignment_statement", `Quick, test_assignment_statement;
  "constraint_statement", `Quick, test_constraint_statement;
  "simple_circuit_compilation", `Quick, test_simple_circuit_compilation;
  "circuit_with_private_inputs", `Quick, test_circuit_with_private_inputs;
  "poseidon_compilation", `Quick, test_poseidon_compilation;
  "error_propagation", `Quick, test_error_propagation;
]

let () = run "Compiler Tests" [
  "compilation", compilation_tests;
]