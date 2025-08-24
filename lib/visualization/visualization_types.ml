(* visualization types and configuration *)

type node_type =
  | Input of string
  | PrivateInput of string
  | Variable of string
  | Constant of string
  | Operation of string * string list (* op_type, inputs *)
  | Output of string
  | Constraint of string

type edge_type =
  | DataFlow
  | ConstraintDep
  | Reference

type graph_node = {
  id: string;
  label: string;
  node_type: node_type;
  position: (float * float) option;
  metadata: (string * string) list;
}

type graph_edge = {
  from_node: string;
  to_node: string;
  edge_type: edge_type;
  label: string option;
  weight: float option;
}

type circuit_graph = {
  nodes: graph_node list;
  edges: graph_edge list;
  metadata: (string * string) list;
}

type visualization_format =
  | GraphViz_DOT
  | SVG
  | PNG
  | HTML_Interactive
  | JSON_Graph

type layout_algorithm =
  | Hierarchical
  | ForceDirected
  | Circular
  | Grid
  | Custom

type visualization_config = {
  format: visualization_format;
  layout: layout_algorithm;
  show_constraints: bool;
  show_intermediate_vars: bool;
  show_wire_labels: bool;
  color_by_type: bool;
  highlight_critical_path: bool;
  group_by_operation: bool;
  max_nodes: int;
  output_file: string option;
}

type constraint_view = {
  constraint_id: int;
  constraint_type: string;
  variables: string list;
  coefficients: string list;
  description: string;
  satisfied: bool option;
}

type inspection_result = {
  circuit_name: string;
  total_constraints: int;
  total_variables: int;
  constraints: constraint_view list;
  critical_path: string list;
  bottlenecks: string list;
  timestamp: float;
}

let default_visualization_config = {
  format = GraphViz_DOT;
  layout = Hierarchical;
  show_constraints = true;
  show_intermediate_vars = true;
  show_wire_labels = false;
  color_by_type = true;
  highlight_critical_path = false;
  group_by_operation = true;
  max_nodes = 1000;
  output_file = None;
}

let node_type_to_string = function
  | Input s -> "input:" ^ s
  | PrivateInput s -> "private:" ^ s
  | Variable s -> "var:" ^ s
  | Constant s -> "const:" ^ s
  | Operation (op, _) -> "op:" ^ op
  | Output s -> "output:" ^ s
  | Constraint s -> "constraint:" ^ s

let edge_type_to_string = function
  | DataFlow -> "dataflow"
  | ConstraintDep -> "constraint"
  | Reference -> "reference"