open Ast

let test_expr_creation () =
  let x = var "x" in
  let y = var "y" in
  let c1 = const "42" in
  let c0 = const "0" in
  
  Alcotest.(check string) "var" "x" (expr_to_string x);
  Alcotest.(check string) "const" "42" (expr_to_string c1);
  
  let sum = add x y in
  Alcotest.(check string) "add" "(x + y)" (expr_to_string sum);
  
  let product = mul x c1 in
  Alcotest.(check string) "mul" "(x * 42)" (expr_to_string product);
  
  let equality = equal sum c0 in
  Alcotest.(check string) "equal" "((x + y) == 0)" (expr_to_string equality);
  
  let hash = poseidon [x; y; c1] in
  Alcotest.(check string) "poseidon" "poseidon(x, y, 42)" (expr_to_string hash)

let test_nested_expressions () =
  let x = var "x" in
  let y = var "y" in
  let z = var "z" in
  let c1 = const "1" in
  let c2 = const "2" in
  
  (* nested expression test *)
  let nested = equal (mul (add x y) (add z c1)) c2 in
  let expected = "(((x + y) * (z + 1)) == 2)" in
  Alcotest.(check string) "nested expr" expected (expr_to_string nested);
  
  let complex_hash = poseidon [add x y; mul z c1; c2] in
  let expected_hash = "poseidon((x + y), (z * 1), 2)" in
  Alcotest.(check string) "complex poseidon" expected_hash (expr_to_string complex_hash)

let test_statements () =
  let x = var "x" in
  let y = var "y" in
  let sum = add x y in
  
  let assign_stmt = assign "result" sum in
  Alcotest.(check string) "assign" "result = (x + y);" (stmt_to_string assign_stmt);
  
  let constraint_stmt = constraint_ (equal sum (const "10")) in
  Alcotest.(check string) "constraint" "assert ((x + y) == 10);" (stmt_to_string constraint_stmt)

let test_circuit_creation () =
  let x = var "x" in
  let y = var "y" in
  let w = var "w" in
  
  let circuit = 
    let c = (empty_circuit "test_circuit"
      |> add_input "x" 
      |> add_input "y"
      |> add_private_input "w") in
    let c = add_stmt (assign "sum" (add x y)) c in
    let c = add_stmt (assign "product" (mul (var "sum") w)) c in
    add_stmt (constraint_ (equal (var "product") (const "42"))) c
  in
  
  Alcotest.(check string) "circuit name" "test_circuit" circuit.name;
  Alcotest.(check (list string)) "inputs" ["x"; "y"] circuit.inputs;
  Alcotest.(check (list string)) "private inputs" ["w"] circuit.private_inputs;
  Alcotest.(check int) "body length" 3 (List.length circuit.body);
  
  let circuit_str = circuit_to_string circuit in
  let contains s sub = String.length sub = 0 || String.length s >= String.length sub &&
    (let rec check i = i > String.length s - String.length sub || 
       (String.sub s i (String.length sub) = sub || check (i + 1)) in check 0) in
  Alcotest.(check bool) "contains circuit name" true 
    (contains circuit_str "circuit test_circuit");
  Alcotest.(check bool) "contains inputs" true 
    (contains circuit_str "inputs: [x, y]");
  Alcotest.(check bool) "contains private" true 
    (contains circuit_str "private: [w]")

let test_ast_traversal () =
  let x = var "x" in
  let y = var "y" in
  let z = var "z" in
  let c1 = const "42" in
  
  let complex_expr = add (mul x y) (add z c1) in
  let vars = get_vars complex_expr in
  Alcotest.(check (list string)) "get_vars" ["x"; "y"; "z"] vars;
  
  let constants = get_constants complex_expr in
  Alcotest.(check (list string)) "get_constants" ["42"] constants;
  
  Alcotest.(check int) "simple depth" 1 (expr_depth x);
  Alcotest.(check int) "add depth" 2 (expr_depth (add x y));
  Alcotest.(check int) "complex depth" 3 (expr_depth complex_expr);
  
  Alcotest.(check bool) "contains x" true (contains_var "x" complex_expr);
  Alcotest.(check bool) "contains missing" false (contains_var "missing" complex_expr);
  
  let hash_expr = poseidon [x; add y z; c1] in
  let hash_vars = get_vars hash_expr in
  Alcotest.(check (list string)) "poseidon vars" ["x"; "y"; "z"] hash_vars;
  Alcotest.(check int) "poseidon depth" 3 (expr_depth hash_expr)

let test_circuit_analysis () =
  let circuit = 
    let c = (empty_circuit "analysis_test"
      |> add_input "a"
      |> add_input "b"
      |> add_private_input "secret") in
    let c = add_stmt (assign "temp1" (add (var "a") (var "b"))) c in
    let c = add_stmt (assign "temp2" (mul (var "temp1") (var "secret"))) c in
    add_stmt (constraint_ (equal (var "temp2") (const "100"))) c
  in
  
  let assigned = get_assigned_vars circuit in
  Alcotest.(check (list string)) "assigned vars" ["temp1"; "temp2"] assigned;
  
  let all_vars = get_all_circuit_vars circuit in
  Alcotest.(check (list string)) "all vars" ["a"; "b"; "secret"; "temp1"; "temp2"] all_vars

let test_edge_cases () =
  let empty_hash = poseidon [] in
  Alcotest.(check string) "empty poseidon" "poseidon()" (expr_to_string empty_hash);
  
  let single_hash = poseidon [var "x"] in
  Alcotest.(check string) "single poseidon" "poseidon(x)" (expr_to_string single_hash);
  
  let empty = empty_circuit "empty" in
  Alcotest.(check (list string)) "empty inputs" [] empty.inputs;
  Alcotest.(check (list string)) "empty private" [] empty.private_inputs;
  Alcotest.(check int) "empty body" 0 (List.length empty.body);
  
  let const_expr = add (const "1") (const "2") in
  Alcotest.(check (list string)) "no vars in constants" [] (get_vars const_expr)

let () =
  let open Alcotest in
  run "AST Tests" [
    "creation", [
      test_case "Expression creation" `Quick test_expr_creation;
      test_case "Nested expressions" `Quick test_nested_expressions;
      test_case "Statement creation" `Quick test_statements;
      test_case "Circuit creation" `Quick test_circuit_creation;
    ];
    "traversal", [
      test_case "AST traversal" `Quick test_ast_traversal;
      test_case "Circuit analysis" `Quick test_circuit_analysis;
    ];
    "edge_cases", [
      test_case "Edge cases" `Quick test_edge_cases;
    ];
  ]