open Error

type field_type = Field

type camellia_type =
  | FieldType
  | UnknownType

type typed_expr = {
  expr: Ast.expr;
  type_: camellia_type;
  pos: position;
}

type type_env = {
  vars: (string * camellia_type) list;
  inputs: string list;
  private_inputs: string list;
  assigned_vars: string list;
}

let empty_env = {
  vars = [];
  inputs = [];
  private_inputs = [];
  assigned_vars = [];
}

let add_var name type_ env =
  { env with vars = (name, type_) :: env.vars }

let find_var name env =
  try Some (List.assoc name env.vars)
  with Not_found -> None

let is_input name env =
  List.mem name env.inputs

let is_private_input name env =
  List.mem name env.private_inputs

let is_assigned name env =
  List.mem name env.assigned_vars

let add_input name env =
  { env with 
    inputs = name :: env.inputs;
    vars = (name, FieldType) :: env.vars 
  }

let add_private_input name env =
  { env with 
    private_inputs = name :: env.private_inputs;
    vars = (name, FieldType) :: env.vars 
  }

let add_assigned_var name env =
  { env with 
    assigned_vars = name :: env.assigned_vars;
    vars = (name, FieldType) :: env.vars 
  }

let pp_type fmt = function
  | FieldType -> Format.fprintf fmt "Field"
  | UnknownType -> Format.fprintf fmt "Unknown"

let type_to_string t =
  Format.asprintf "%a" pp_type t

let type_error msg pos =
  type_error msg ~pos ()

let unbound_variable_error var pos =
  unbound_variable var ~pos ()

let type_mismatch_error expected actual pos =
  type_mismatch expected actual ~pos ()

let arity_mismatch_error expected actual pos =
  arity_mismatch expected actual ~pos ()