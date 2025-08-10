open Alcotest

(* Simple lexer functionality test *)
let test_basic_lexer () =
  (* Test that we can call basic parser functions *)
  match Parser.parse_expr "x" with
  | Ok _ -> 
      check bool "basic parsing works" true true
  | Error _ -> 
      check bool "basic parsing failed" false true

let basic_tests = [
  "basic_lexer", `Quick, test_basic_lexer;
]

let () = run "Basic Lexer Tests" [
  "basic", basic_tests;
]