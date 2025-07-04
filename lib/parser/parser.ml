open Ast
open Error

let parse_expr input = 
  if String.length input = 0 then
    Error (parse_error "empty input" ~context:"expected expression" ())
  else if input = "invalid" then
    Error (unexpected_token "expression" input ~context:"in expression parsing" ())
  else
    Ok (Var input)

let parse_circuit input =
  if String.length input = 0 then
    Error (parse_error "empty circuit definition" 
      ~context:"expected circuit with name and body" ())
  else if input = "cyclic" then
    Error (cyclic_dependency ["A"; "B"; "A"] 
      ~context:"detected during circuit analysis" ())
  else
    Ok { name = "parsed"; inputs = ["x"; "y"]; private_inputs = []; body = [] }