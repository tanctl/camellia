open Error
open Debug

type wire_id = int

type wire_type = 
  | Input
  | Private
  | Intermediate
  | Output
  | Constant

type wire_info = {
  id: wire_id;
  name: string;
  wire_type: wire_type;
  value: Field.t option;
}

type coefficient = Field.t

type linear_combination = (wire_id * coefficient) list

type constraint_triple = {
  a: linear_combination;
  b: linear_combination;
  c: linear_combination;
}

type r1cs_metadata = {
  num_variables: int;
  num_constraints: int;
  num_inputs: int;
  num_private: int;
  num_outputs: int;
}

type r1cs_system = {
  metadata: r1cs_metadata;
  constraints: constraint_triple list;
  wires: wire_info list;
  input_wires: wire_id list;
  private_wires: wire_id list;
  output_wires: wire_id list;
}

type wire_manager = {
  mutable next_id: wire_id;
  mutable wires: (wire_id, wire_info) Hashtbl.t;
  mutable name_to_id: (string, wire_id) Hashtbl.t;
}

let wire_type_to_string = function
  | Input -> "input"
  | Private -> "private"
  | Intermediate -> "intermediate"
  | Output -> "output"
  | Constant -> "constant"

let create_wire_manager () = {
  next_id = 1;
  wires = Hashtbl.create 32;
  name_to_id = Hashtbl.create 32;
}

let allocate_wire manager name wire_type ?value () =
  if Hashtbl.mem manager.name_to_id name then
    Result.Error (circuit_error ("Wire already exists: " ^ name) ())
  else
    let id = manager.next_id in
    manager.next_id <- manager.next_id + 1;
    let wire = { id; name; wire_type; value } in
    Hashtbl.add manager.wires id wire;
    Hashtbl.add manager.name_to_id name id;
    Ok id

let get_wire manager id =
  try Ok (Hashtbl.find manager.wires id)
  with Not_found -> Result.Error (circuit_error ("Wire not found: " ^ string_of_int id) ())

let get_wire_by_name manager name =
  try 
    let id = Hashtbl.find manager.name_to_id name in
    get_wire manager id
  with Not_found -> Result.Error (circuit_error ("Wire not found: " ^ name) ())

let set_wire_value manager id value =
  match get_wire manager id with
  | Ok wire ->
      let updated_wire = { wire with value = Some value } in
      Hashtbl.replace manager.wires id updated_wire;
      Ok ()
  | Error e -> Error e

let get_all_wires manager =
  Hashtbl.fold (fun _id wire acc -> wire :: acc) manager.wires []

let create_constraint a b c = { a; b; c }

let empty_linear_combination = []

let add_term wire_id coefficient combination =
  (wire_id, coefficient) :: combination

let create_linear_combination terms = terms

let get_coefficient wire_id combination =
  try
    Some (List.assoc wire_id combination)
  with Not_found -> None

let eval_simple_field_expr expr =
  if String.contains expr '(' && String.contains expr '*' then
    let regex = Str.regexp "(\\([0-9]+\\) \\* \\([0-9]+\\) mod.*)" in
    if Str.string_match regex expr 0 then
      try
        let a = int_of_string (Str.matched_group 1 expr) in
        let b = int_of_string (Str.matched_group 2 expr) in
        Field.of_string (string_of_int (a * b))
      with _ -> Result.Error (field_error ("Cannot evaluate multiplication: " ^ expr) ())
    else
      Result.Error (field_error ("Cannot parse expression format: " ^ expr) ())
  else if String.contains expr '(' && String.contains expr '+' then
    let regex = Str.regexp "(\\([0-9]+\\) \\+ \\([0-9]+\\) mod.*)" in
    if Str.string_match regex expr 0 then
      try
        let a = int_of_string (Str.matched_group 1 expr) in
        let b = int_of_string (Str.matched_group 2 expr) in
        Field.of_string (string_of_int (a + b))
      with _ -> Result.Error (field_error ("Cannot evaluate addition: " ^ expr) ())
    else
      Result.Error (field_error ("Cannot parse expression format: " ^ expr) ())
  else
    Ok expr

let field_equal_evaluated a b =
  let* a_eval = eval_simple_field_expr a in
  let* b_eval = eval_simple_field_expr b in
  Field.equal a_eval b_eval

let evaluate_linear_combination combination witness =
  let total_sum = List.fold_left (fun acc (wire_id, coeff) ->
    let wire_value = match List.assoc_opt wire_id witness with
    | Some wire_value -> wire_value
    | None when wire_id = 0 -> Field.one  (* wire 0 is implicit constant 1 *)
    | None -> Field.zero
    in
    let coeff_str = Field.to_string coeff in
    let wire_str = Field.to_string wire_value in
    (try
      let c = if coeff_str = "1" then 1 else int_of_string coeff_str in
      let w = if wire_str = "1" then 1 else int_of_string wire_str in
      acc + (c * w)
    with _ -> acc)
  ) 0 combination in
  Field.of_string (string_of_int total_sum)

let validate_constraint constraint_triple witness =
  let* a_val = evaluate_linear_combination constraint_triple.a witness in
  let* b_val = evaluate_linear_combination constraint_triple.b witness in
  let* c_val = evaluate_linear_combination constraint_triple.c witness in
  let* ab_product = Field.mul a_val b_val in
  let* equal_result = field_equal_evaluated (Field.to_string ab_product) (Field.to_string c_val) in
  if equal_result then Ok () else
    Result.Error (constraint_unsatisfiable 
      (Printf.sprintf "Constraint not satisfied: (%s) * (%s) != (%s)" 
        (Field.to_string a_val) (Field.to_string b_val) (Field.to_string c_val)) ())

let create_empty_r1cs () = {
  metadata = { num_variables = 0; num_constraints = 0; num_inputs = 0; 
               num_private = 0; num_outputs = 0 };
  constraints = [];
  wires = [];
  input_wires = [];
  private_wires = [];
  output_wires = [];
}

let add_constraint_to_r1cs (r1cs : r1cs_system) constraint_triple = {
  r1cs with 
  constraints = constraint_triple :: r1cs.constraints;
  metadata = { r1cs.metadata with num_constraints = r1cs.metadata.num_constraints + 1 }
}

let add_wire_to_r1cs (r1cs : r1cs_system) (wire : wire_info) =
  let updated_wires = wire :: r1cs.wires in
  let updated_metadata = { r1cs.metadata with num_variables = r1cs.metadata.num_variables + 1 } in
  let (updated_inputs, updated_private, updated_outputs, updated_meta) = 
    match wire.wire_type with
    | Input -> 
        (wire.id :: r1cs.input_wires, r1cs.private_wires, r1cs.output_wires,
         { updated_metadata with num_inputs = updated_metadata.num_inputs + 1 })
    | Private ->
        (r1cs.input_wires, wire.id :: r1cs.private_wires, r1cs.output_wires,
         { updated_metadata with num_private = updated_metadata.num_private + 1 })
    | Output ->
        (r1cs.input_wires, r1cs.private_wires, wire.id :: r1cs.output_wires,
         { updated_metadata with num_outputs = updated_metadata.num_outputs + 1 })
    | Intermediate | Constant ->
        (r1cs.input_wires, r1cs.private_wires, r1cs.output_wires, updated_metadata)
  in
  {
    metadata = updated_meta;
    constraints = r1cs.constraints;
    wires = updated_wires;
    input_wires = updated_inputs;
    private_wires = updated_private;
    output_wires = updated_outputs;
  }

let validate_r1cs (r1cs : r1cs_system) witness =
  List.fold_left (fun acc_result constraint_triple ->
    let* () = acc_result in
    validate_constraint constraint_triple witness
  ) (Ok ()) r1cs.constraints

let get_r1cs_statistics (r1cs : r1cs_system) = {
  num_variables = List.length r1cs.wires;
  num_constraints = List.length r1cs.constraints;
  num_inputs = List.length r1cs.input_wires;
  num_private = List.length r1cs.private_wires;
  num_outputs = List.length r1cs.output_wires;
}

let linear_combination_to_string combination =
  let term_to_string (wire_id, coeff) =
    Printf.sprintf "%s*w%d" (Field.to_string coeff) wire_id
  in
  match combination with
  | [] -> "0"
  | terms -> String.concat " + " (List.map term_to_string terms)

let constraint_to_string constraint_triple =
  Printf.sprintf "(%s) * (%s) = (%s)"
    (linear_combination_to_string constraint_triple.a)
    (linear_combination_to_string constraint_triple.b)
    (linear_combination_to_string constraint_triple.c)

let r1cs_to_string (r1cs : r1cs_system) =
  let header = Printf.sprintf "R1CS System: %d variables, %d constraints\n"
    r1cs.metadata.num_variables r1cs.metadata.num_constraints in
  let constraints_str = String.concat "\n" 
    (List.mapi (fun i c -> Printf.sprintf "C%d: %s" i (constraint_to_string c)) 
      (List.rev r1cs.constraints)) in
  header ^ constraints_str

let create_addition_constraint a_wire b_wire sum_wire =
  let a_lc = add_term a_wire Field.one (add_term b_wire Field.one empty_linear_combination) in
  let b_lc = add_term 0 Field.one empty_linear_combination in
  let c_lc = add_term sum_wire Field.one empty_linear_combination in
  create_constraint a_lc b_lc c_lc

let create_multiplication_constraint a_wire b_wire product_wire =
  let a_lc = add_term a_wire Field.one empty_linear_combination in
  let b_lc = add_term b_wire Field.one empty_linear_combination in
  let c_lc = add_term product_wire Field.one empty_linear_combination in
  create_constraint a_lc b_lc c_lc

let create_constant_constraint wire_id constant_value =
  let one_lc = add_term 0 Field.one empty_linear_combination in
  let c_lc = add_term wire_id constant_value empty_linear_combination in
  create_constraint one_lc one_lc c_lc

let serialize_linear_combination_json combination =
  let terms = List.map (fun (wire_id, coeff) ->
    `Assoc [("wire", `Int wire_id); ("coeff", `String (Field.to_string coeff))]
  ) combination in
  `List terms

let serialize_constraint_json constraint_triple =
  `Assoc [
    ("a", serialize_linear_combination_json constraint_triple.a);
    ("b", serialize_linear_combination_json constraint_triple.b);
    ("c", serialize_linear_combination_json constraint_triple.c);
  ]

let serialize_wire_json wire =
  let value_json = match wire.value with
    | Some v -> `String (Field.to_string v)
    | None -> `Null
  in
  `Assoc [
    ("id", `Int wire.id);
    ("name", `String wire.name);
    ("type", `String (wire_type_to_string wire.wire_type));
    ("value", value_json);
  ]

let serialize_r1cs_json (r1cs : r1cs_system) =
  `Assoc [
    ("metadata", `Assoc [
      ("num_variables", `Int r1cs.metadata.num_variables);
      ("num_constraints", `Int r1cs.metadata.num_constraints);
      ("num_inputs", `Int r1cs.metadata.num_inputs);
      ("num_private", `Int r1cs.metadata.num_private);
      ("num_outputs", `Int r1cs.metadata.num_outputs);
    ]);
    ("constraints", `List (List.map serialize_constraint_json (List.rev r1cs.constraints)));
    ("wires", `List (List.map serialize_wire_json r1cs.wires));
    ("input_wires", `List (List.map (fun id -> `Int id) r1cs.input_wires));
    ("private_wires", `List (List.map (fun id -> `Int id) r1cs.private_wires));
    ("output_wires", `List (List.map (fun id -> `Int id) r1cs.output_wires));
  ]

let export_r1cs_json (r1cs : r1cs_system) filename =
  let json = serialize_r1cs_json r1cs in
  let json_string = Yojson.Safe.pretty_to_string json in
  let oc = open_out filename in
  output_string oc json_string;
  close_out oc

let analyze_constraint_density (r1cs : r1cs_system) =
  let total_possible_terms = r1cs.metadata.num_variables * r1cs.metadata.num_constraints * 3 in
  let actual_terms = List.fold_left (fun acc constraint_triple ->
    acc + List.length constraint_triple.a + List.length constraint_triple.b + List.length constraint_triple.c
  ) 0 r1cs.constraints in
  if total_possible_terms = 0 then 0.0 else
    float_of_int actual_terms /. float_of_int total_possible_terms

let get_constraint_complexity (r1cs : r1cs_system) =
  List.map (fun constraint_triple ->
    List.length constraint_triple.a + List.length constraint_triple.b + List.length constraint_triple.c
  ) r1cs.constraints

let get_wire_usage_stats (r1cs : r1cs_system) =
  let wire_usage = Hashtbl.create (List.length r1cs.wires) in
  List.iter (fun constraint_triple ->
    let count_wire_usage combination =
      List.iter (fun (wire_id, _) ->
        let current = try Hashtbl.find wire_usage wire_id with Not_found -> 0 in
        Hashtbl.replace wire_usage wire_id (current + 1)
      ) combination
    in
    count_wire_usage constraint_triple.a;
    count_wire_usage constraint_triple.b;
    count_wire_usage constraint_triple.c
  ) r1cs.constraints;
  wire_usage

let trace_r1cs_to_debug ctx (r1cs : r1cs_system) =
  log ctx Info "R1CS System Analysis:";
  log ctx Info "  Variables: %d" r1cs.metadata.num_variables;
  log ctx Info "  Constraints: %d" r1cs.metadata.num_constraints;
  log ctx Info "  Inputs: %d" r1cs.metadata.num_inputs;
  log ctx Info "  Private: %d" r1cs.metadata.num_private;
  log ctx Info "  Outputs: %d" r1cs.metadata.num_outputs;
  
  let density = analyze_constraint_density r1cs in
  log ctx Info "  Constraint density: %.2f%%" (density *. 100.0);
  
  List.iteri (fun i constraint_triple ->
    log ctx Trace "Constraint %d: %s" i (constraint_to_string constraint_triple)
  ) (List.rev r1cs.constraints)