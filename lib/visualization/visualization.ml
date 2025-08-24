(* main visualization module - public interface *)

open Error

module Types = Visualization_types
module GraphViz = Graphviz_renderer
module GraphBuilder = Circuit_graph_builder
module Inspector = Constraint_inspector
module Debugger = Interactive_debugger
module Profiler = Performance_profiler

(* re-export key types *)
type visualization_config = Visualization_types.visualization_config
type circuit_graph = Visualization_types.circuit_graph
type inspection_result = Visualization_types.inspection_result
type debug_session = Interactive_debugger.debug_session

(* re-export key functions *)
let default_config = Types.default_visualization_config

let create_circuit_visualization ?(config=default_config) circuit =
  GraphBuilder.build_circuit_graph config circuit

let create_r1cs_visualization ?(config=default_config) r1cs =
  GraphBuilder.build_r1cs_constraint_graph config r1cs

let export_visualization config graph filename =
  match config.Types.format with
  | Types.GraphViz_DOT -> GraphViz.export_to_file config graph filename
  | Types.SVG | Types.PNG -> 
      let base_name = Filename.remove_extension filename in
      (match GraphViz.generate_formats config graph base_name with
       | Ok files -> 
           Printf.printf "Generated files: %s\n" 
             (String.concat ", " (List.map snd files));
           Ok ()
       | Error e -> Error e)
  | _ -> Error (Error.circuit_error "Unsupported export format" ())

let inspect_constraints r1cs = Inspector.inspect_all_constraints r1cs

let inspect_single_constraint r1cs constraint_idx =
  Inspector.inspect_constraint r1cs constraint_idx

let find_constraint_usages r1cs wire_idx =
  Inspector.find_constraints_using_variable r1cs wire_idx

let start_interactive_debug circuit r1cs =
  let session = Debugger.create_debug_session circuit r1cs in
  Debugger.run_interactive_session session

let start_batch_debug circuit r1cs commands =
  let session = Debugger.create_debug_session ~interactive:false circuit r1cs in
  Debugger.batch_execute_commands session commands

let profile_compilation circuit =
  Profiler.profile_circuit_compilation circuit

(* convenience functions *)
let quick_visualize circuit filename =
  let config = { default_config with 
    format = Types.SVG; 
    layout = Types.Hierarchical;
    show_constraints = true;
  } in
  let graph = create_circuit_visualization ~config circuit in
  export_visualization config graph filename

let detailed_visualize circuit r1cs base_filename =
  (* create circuit visualization *)
  let circuit_config = { default_config with 
    format = Types.SVG;
    layout = Types.Hierarchical;
    show_constraints = false;
  } in
  let circuit_graph = create_circuit_visualization ~config:circuit_config circuit in
  let circuit_file = base_filename ^ "_circuit.svg" in
  let* () = export_visualization circuit_config circuit_graph circuit_file in
  
  (* create R1CS visualization *)
  let r1cs_config = { default_config with 
    format = Types.SVG;
    layout = Types.ForceDirected;
    show_constraints = true;
    max_nodes = 500;
  } in
  let r1cs_graph = create_r1cs_visualization ~config:r1cs_config r1cs in
  let r1cs_file = base_filename ^ "_r1cs.svg" in
  let* () = export_visualization r1cs_config r1cs_graph r1cs_file in
  
  (* export constraint inspection *)
  let inspection = inspect_constraints r1cs in
  let inspection_file = base_filename ^ "_constraints.md" in
  let* () = Inspector.export_constraint_inspection inspection inspection_file in
  
  Printf.printf "Detailed visualization complete:\n";
  Printf.printf "  Circuit: %s\n" circuit_file;
  Printf.printf "  R1CS: %s\n" r1cs_file;
  Printf.printf "  Constraints: %s\n" inspection_file;
  
  Ok ()

let create_debug_report circuit r1cs filename =
  let inspection = inspect_constraints r1cs in
  let circuit_graph = create_circuit_visualization circuit in
  let r1cs_graph = create_r1cs_visualization r1cs in
  
  try
    let oc = open_out filename in
    Printf.fprintf oc "# Circuit Debug Report\n\n";
    Printf.fprintf oc "**Circuit:** %s\n" circuit.Ast.name;
    Printf.fprintf oc "**Generated:** %s\n\n" (string_of_float (Unix.time ()));
    
    Printf.fprintf oc "## Circuit Overview\n\n";
    Printf.fprintf oc "- **Inputs:** %d\n" (List.length circuit.Ast.inputs);
    Printf.fprintf oc "- **Private Inputs:** %d\n" (List.length circuit.Ast.private_inputs);
    Printf.fprintf oc "- **Statements:** %d\n" (List.length circuit.Ast.body);
    Printf.fprintf oc "- **Graph Nodes:** %d\n" (List.length circuit_graph.nodes);
    Printf.fprintf oc "- **Graph Edges:** %d\n\n" (List.length circuit_graph.edges);
    
    Printf.fprintf oc "## R1CS System\n\n";
    Printf.fprintf oc "- **Variables:** %d\n" inspection.total_variables;
    Printf.fprintf oc "- **Constraints:** %d\n" inspection.total_constraints;
    Printf.fprintf oc "- **R1CS Graph Nodes:** %d\n" (List.length r1cs_graph.nodes);
    Printf.fprintf oc "- **R1CS Graph Edges:** %d\n\n" (List.length r1cs_graph.edges);
    
    Printf.fprintf oc "## Constraint Summary\n\n";
    let constraint_types = Hashtbl.create 16 in
    List.iter (fun constraint_view ->
      let current = Hashtbl.find_opt constraint_types constraint_view.Types.constraint_type |> Option.value ~default:0 in
      Hashtbl.replace constraint_types constraint_view.Types.constraint_type (current + 1)
    ) inspection.constraints;
    
    Hashtbl.iter (fun ctype count ->
      Printf.fprintf oc "- **%s:** %d constraints\n" (String.capitalize_ascii ctype) count
    ) constraint_types;
    
    Printf.fprintf oc "\n## Visualization Commands\n\n";
    Printf.fprintf oc "To generate visualizations:\n\n";
    Printf.fprintf oc "```bash\n";
    Printf.fprintf oc "# Circuit visualization\n";
    Printf.fprintf oc "camellia visualize --type circuit %s\n\n" circuit.Ast.name;
    Printf.fprintf oc "# R1CS visualization\n";
    Printf.fprintf oc "camellia visualize --type r1cs %s\n\n" circuit.Ast.name;
    Printf.fprintf oc "# Interactive debugging\n";
    Printf.fprintf oc "camellia debug --interactive %s\n" circuit.Ast.name;
    Printf.fprintf oc "```\n\n";
    
    Printf.fprintf oc "## Debug Tips\n\n";
    Printf.fprintf oc "1. **High constraint count?** Look for optimization opportunities\n";
    Printf.fprintf oc "2. **Complex constraint types?** Consider simplifying expressions\n";
    Printf.fprintf oc "3. **Many intermediate variables?** Check for unnecessary computations\n";
    Printf.fprintf oc "4. **Performance issues?** Use the profiler to identify bottlenecks\n\n";
    
    close_out oc;
    Ok ()
  with
  | exn -> Error (Error.circuit_error ("Failed to create debug report: " ^ Printexc.to_string exn) ())

(* helper to format file sizes *)
let format_file_size bytes =
  let kb = float_of_int bytes /. 1024.0 in
  if kb < 1024.0 then Printf.sprintf "%.1fKB" kb
  else Printf.sprintf "%.1fMB" (kb /. 1024.0)

(* get file size for export reporting *)
let get_file_size filename =
  try
    let stats = Unix.stat filename in
    Some stats.st_size
  with _ -> None