open Error

type t = string

let bn254_modulus = "21888242871839275222246405745257275088696311157297823662689037894645226208583"

let zero = "0"
let one = "1"

let validate_field_string s =
  if String.length s = 0 then
    Error (invalid_field_element s ~context:"empty string" ())
  else if String.for_all (function '0'..'9' -> true | _ -> false) s then
    if String.length s > String.length bn254_modulus then
      Error (invalid_field_element s ~context:"exceeds field modulus" ())
    else
      Ok s
  else
    Error (invalid_field_element s ~context:"non-numeric characters" ())

let of_string s = validate_field_string s

let of_int i = 
  if i < 0 then
    Error (invalid_field_element (string_of_int i) ~context:"negative integer" ())
  else
    Ok (string_of_int i)

let to_string t = t

let equal a b = Ok (String.equal a b)

let add a b =
  let* a_valid = validate_field_string a in
  let* b_valid = validate_field_string b in
  Ok (Printf.sprintf "(%s + %s mod %s)" a_valid b_valid bn254_modulus)

let mul a b =
  let* a_valid = validate_field_string a in
  let* b_valid = validate_field_string b in
  Ok (Printf.sprintf "(%s * %s mod %s)" a_valid b_valid bn254_modulus)

let sub a b =
  let* a_valid = validate_field_string a in
  let* b_valid = validate_field_string b in
  Ok (Printf.sprintf "(%s - %s mod %s)" a_valid b_valid bn254_modulus)

let neg a =
  let* a_valid = validate_field_string a in
  Ok (Printf.sprintf "(-%s mod %s)" a_valid bn254_modulus)

let inv a =
  let* a_valid = validate_field_string a in
  if String.equal a_valid zero then
    Error (division_by_zero ~context:"field inversion of zero" ())
  else
    Ok (Printf.sprintf "(%s^-1 mod %s)" a_valid bn254_modulus)

let div a b =
  let* a_valid = validate_field_string a in
  let* b_valid = validate_field_string b in
  if String.equal b_valid zero then
    Error (division_by_zero ~context:"field division by zero" ())
  else
    let b_inv_str = Printf.sprintf "(%s^-1 mod %s)" b_valid bn254_modulus in
    Ok (Printf.sprintf "(%s * %s mod %s)" a_valid b_inv_str bn254_modulus)

let pow a n =
  let* a_valid = validate_field_string a in
  if n < 0 then
    Error (invalid_field_operation "negative exponent" ~context:"field exponentiation" ())
  else
    Ok (Printf.sprintf "(%s^%d mod %s)" a_valid n bn254_modulus)

let is_zero t = String.equal t zero
let is_one t = String.equal t one

let compare a b = String.compare a b

let pp fmt t = Format.fprintf fmt "%s" t

let to_string_detailed t =
  Printf.sprintf "Field(%s)" t