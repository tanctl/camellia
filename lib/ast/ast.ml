type expr =
  | Var of string
  | Const of string
  | Add of expr * expr
  | Mul of expr * expr
  | Equal of expr * expr
  | Poseidon of expr list

type stmt =
  | Constraint of expr
  | Assign of string * expr

type circuit = {
  name: string;
  inputs: string list;
  private_inputs: string list;
  body: stmt list;
}

let rec pp_expr fmt = function
  | Var s -> Format.fprintf fmt "%s" s
  | Const s -> Format.fprintf fmt "%s" s
  | Add (e1, e2) -> Format.fprintf fmt "(%a + %a)" pp_expr e1 pp_expr e2
  | Mul (e1, e2) -> Format.fprintf fmt "(%a * %a)" pp_expr e1 pp_expr e2
  | Equal (e1, e2) -> Format.fprintf fmt "(%a == %a)" pp_expr e1 pp_expr e2
  | Poseidon exprs -> 
      Format.fprintf fmt "poseidon(%a)" 
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr) 
        exprs

let pp_stmt fmt = function
  | Constraint expr -> Format.fprintf fmt "assert %a;" pp_expr expr
  | Assign (var, expr) -> Format.fprintf fmt "%s = %a;" var pp_expr expr

let pp_circuit fmt circuit =
  Format.fprintf fmt "circuit %s {\n" circuit.name;
  Format.fprintf fmt "  inputs: [%a]\n"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") 
     (fun fmt s -> Format.fprintf fmt "%s" s))
    circuit.inputs;
  Format.fprintf fmt "  private: [%a]\n"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") 
     (fun fmt s -> Format.fprintf fmt "%s" s))
    circuit.private_inputs;
  Format.fprintf fmt "  body:\n";
  List.iter (fun stmt -> Format.fprintf fmt "    %a\n" pp_stmt stmt) circuit.body;
  Format.fprintf fmt "}"

let expr_to_string expr =
  Format.asprintf "%a" pp_expr expr

let stmt_to_string stmt =
  Format.asprintf "%a" pp_stmt stmt

let circuit_to_string circuit =
  Format.asprintf "%a" pp_circuit circuit

let rec get_vars = function
  | Var s -> [s]
  | Const _ -> []
  | Add (e1, e2) | Mul (e1, e2) | Equal (e1, e2) -> 
      (get_vars e1) @ (get_vars e2)
  | Poseidon exprs -> 
      List.fold_left (fun acc e -> acc @ (get_vars e)) [] exprs

let rec get_constants = function
  | Var _ -> []
  | Const s -> [s]
  | Add (e1, e2) | Mul (e1, e2) | Equal (e1, e2) -> 
      (get_constants e1) @ (get_constants e2)
  | Poseidon exprs -> 
      List.fold_left (fun acc e -> acc @ (get_constants e)) [] exprs

let rec expr_depth = function
  | Var _ | Const _ -> 1
  | Add (e1, e2) | Mul (e1, e2) | Equal (e1, e2) -> 
      1 + max (expr_depth e1) (expr_depth e2)
  | Poseidon exprs -> 
      1 + (List.fold_left (fun acc e -> max acc (expr_depth e)) 0 exprs)

let rec contains_var var = function
  | Var s -> s = var
  | Const _ -> false
  | Add (e1, e2) | Mul (e1, e2) | Equal (e1, e2) -> 
      (contains_var var e1) || (contains_var var e2)
  | Poseidon exprs -> 
      List.exists (contains_var var) exprs

let get_assigned_vars circuit =
  List.fold_left (fun acc stmt ->
    match stmt with
    | Assign (var, _) -> var :: acc
    | Constraint _ -> acc
  ) [] circuit.body |> List.rev

let get_all_circuit_vars circuit =
  let assigned = get_assigned_vars circuit in
  circuit.inputs @ circuit.private_inputs @ assigned

let var s = Var s
let const s = Const s
let add e1 e2 = Add (e1, e2)
let mul e1 e2 = Mul (e1, e2)
let equal e1 e2 = Equal (e1, e2)
let poseidon exprs = Poseidon exprs

let constraint_ expr = Constraint expr
let assign var expr = Assign (var, expr)

let empty_circuit name = {
  name;
  inputs = [];
  private_inputs = [];
  body = [];
}

let add_input input circuit = {
  circuit with inputs = circuit.inputs @ [input]
}

let add_private_input input circuit = {
  circuit with private_inputs = circuit.private_inputs @ [input]
}

let add_stmt stmt circuit = {
  circuit with body = circuit.body @ [stmt]
}