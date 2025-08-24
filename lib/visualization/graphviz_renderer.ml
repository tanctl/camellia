open Visualization_types
open Error

(* GraphViz DOT format generator *)

let escape_label label =
  let escaped = String.map (function
    | '"' -> '\''
    | '\n' -> ' '
    | '\t' -> ' '
    | c -> c
  ) label in
  "\"" ^ escaped ^ "\""

let node_color = function
  | Input _ -> "lightblue"
  | PrivateInput _ -> "lightcoral"
  | Variable _ -> "lightgray"
  | Constant _ -> "lightyellow"
  | Operation (_, _) -> "lightgreen"
  | Output _ -> "orange"
  | Constraint _ -> "pink"

let node_shape = function
  | Input _ | PrivateInput _ -> "ellipse"
  | Variable _ -> "box"
  | Constant _ -> "diamond"
  | Operation (_, _) -> "hexagon"
  | Output _ -> "doubleoctagon"
  | Constraint _ -> "trapezium"

let edge_style = function
  | DataFlow -> "solid"
  | ConstraintDep -> "dashed"
  | Reference -> "dotted"

let edge_color = function
  | DataFlow -> "black"
  | ConstraintDep -> "red"
  | Reference -> "blue"

let generate_dot_header config =
  let layout_engine = match config.layout with
    | Hierarchical -> "dot"
    | ForceDirected -> "neato"
    | Circular -> "circo"
    | Grid -> "fdp"
    | Custom -> "sfdp"
  in
  Printf.sprintf "digraph circuit {\n  layout=%s;\n  rankdir=TB;\n  node [fontname=\"Arial\", fontsize=10];\n  edge [fontname=\"Arial\", fontsize=8];\n\n" layout_engine

let generate_dot_node config node =
  let color = if config.color_by_type then node_color node.node_type else "lightgray" in
  let shape = node_shape node.node_type in
  let label = escape_label node.label in
  
  let attributes = [
    "label=" ^ label;
    "shape=" ^ shape;
    "fillcolor=" ^ color;
    "style=filled";
  ] in
  
  let attr_string = String.concat ", " attributes in
  Printf.sprintf "  %s [%s];\n" node.id attr_string

let generate_dot_edge _config edge =
  let style = edge_style edge.edge_type in
  let color = edge_color edge.edge_type in
  
  let attributes = [
    "style=" ^ style;
    "color=" ^ color;
  ] in
  
  let attributes = match edge.label with
    | Some label -> ("label=" ^ escape_label label) :: attributes
    | None -> attributes
  in
  
  let attributes = match edge.weight with
    | Some w -> ("weight=" ^ string_of_float w) :: attributes
    | None -> attributes
  in
  
  let attr_string = String.concat ", " attributes in
  Printf.sprintf "  %s -> %s [%s];\n" edge.from_node edge.to_node attr_string

let generate_dot_subgraphs config graph =
  if not config.group_by_operation then ""
  else
    let operation_groups = Hashtbl.create 16 in
    
    (* group nodes by operation type *)
    List.iter (fun node ->
      match node.node_type with
      | Operation (op_type, _) ->
          let existing = Hashtbl.find_opt operation_groups op_type |> Option.value ~default:[] in
          Hashtbl.replace operation_groups op_type (node.id :: existing)
      | _ -> ()
    ) graph.nodes;
    
    let subgraphs = Hashtbl.fold (fun op_type node_ids acc ->
      if List.length node_ids > 1 then
        let subgraph = Printf.sprintf "  subgraph cluster_%s {\n    label=\"%s Operations\";\n    style=rounded;\n    color=lightgray;\n    %s\n  }\n"
          op_type op_type (String.concat "; " node_ids) in
        subgraph :: acc
      else acc
    ) operation_groups [] in
    
    String.concat "\n" subgraphs

let render_graphviz config graph =
  let buffer = Buffer.create 4096 in
  
  (* header *)
  Buffer.add_string buffer (generate_dot_header config);
  
  (* metadata as comments *)
  List.iter (fun (key, value) ->
    Buffer.add_string buffer (Printf.sprintf "  // %s: %s\n" key value)
  ) graph.metadata;
  Buffer.add_string buffer "\n";
  
  (* subgraphs for grouping *)
  Buffer.add_string buffer (generate_dot_subgraphs config graph);
  Buffer.add_string buffer "\n";
  
  (* nodes *)
  List.iter (fun node ->
    Buffer.add_string buffer (generate_dot_node config node)
  ) graph.nodes;
  Buffer.add_string buffer "\n";
  
  (* edges *)
  List.iter (fun edge ->
    Buffer.add_string buffer (generate_dot_edge config edge)
  ) graph.edges;
  
  (* footer *)
  Buffer.add_string buffer "}\n";
  
  Buffer.contents buffer

let export_to_file config graph filename =
  try
    let dot_content = render_graphviz config graph in
    let oc = open_out filename in
    output_string oc dot_content;
    close_out oc;
    Ok ()
  with
  | exn -> Error (circuit_error ("Failed to export GraphViz: " ^ Printexc.to_string exn) ())

let generate_formats config graph base_filename =
  let dot_file = base_filename ^ ".dot" in
  let* () = export_to_file config graph dot_file in
  
  let results = ref [] in
  
  (* generate SVG if requested *)
  if config.format = SVG then (
    let svg_file = base_filename ^ ".svg" in
    let cmd = Printf.sprintf "dot -Tsvg %s -o %s" dot_file svg_file in
    match Sys.command cmd with
    | 0 -> results := ("SVG", svg_file) :: !results
    | _ -> results := ("SVG (failed)", svg_file) :: !results
  );
  
  (* generate PNG if requested *)
  if config.format = PNG then (
    let png_file = base_filename ^ ".png" in
    let cmd = Printf.sprintf "dot -Tpng %s -o %s" dot_file png_file in
    match Sys.command cmd with
    | 0 -> results := ("PNG", png_file) :: !results
    | _ -> results := ("PNG (failed)", png_file) :: !results
  );
  
  results := ("DOT", dot_file) :: !results;
  Ok (List.rev !results)

let create_legend _config =
  let legend_nodes = [
    { id = "legend_input"; label = "Input"; node_type = Input "example"; position = None; metadata = [] };
    { id = "legend_private"; label = "Private Input"; node_type = PrivateInput "example"; position = None; metadata = [] };
    { id = "legend_var"; label = "Variable"; node_type = Variable "example"; position = None; metadata = [] };
    { id = "legend_const"; label = "Constant"; node_type = Constant "5"; position = None; metadata = [] };
    { id = "legend_op"; label = "Operation"; node_type = Operation ("add", []); position = None; metadata = [] };
    { id = "legend_output"; label = "Output"; node_type = Output "result"; position = None; metadata = [] };
    { id = "legend_constraint"; label = "Constraint"; node_type = Constraint "c1"; position = None; metadata = [] };
  ] in
  
  let legend_edges = [
    { from_node = "legend_input"; to_node = "legend_op"; edge_type = DataFlow; label = Some "data"; weight = None };
    { from_node = "legend_op"; to_node = "legend_constraint"; edge_type = ConstraintDep; label = Some "constraint"; weight = None };
  ] in
  
  {
    nodes = legend_nodes;
    edges = legend_edges;
    metadata = [("type", "legend")];
  }