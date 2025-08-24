open Visualization_types

(* interactive debugging mode with command interface *)

type debug_command =
  | ShowConstraint of int
  | ShowVariable of int
  | ShowDependencies of int
  | ListConstraints of int option (* optional limit *)
  | ListVariables of int option
  | FindUsages of int (* wire index *)
  | VisualizePath of int list
  | ExportGraph of string
  | SetConfig of string * string
  | Help
  | Quit

type debug_session = {
  circuit: Ast.circuit option;
  r1cs: R1cs.r1cs_system option;
  config: visualization_config;
  current_graph: circuit_graph option;
  history: string list;
  interactive: bool;
}

let parse_command input =
  let parts = String.split_on_char ' ' (String.trim input) in
  match parts with
  | [] -> Help
  | "help" :: _ -> Help
  | "quit" :: _ | "exit" :: _ -> Quit
  | "show" :: "constraint" :: idx :: _ -> 
      (try ShowConstraint (int_of_string idx)
       with _ -> Help)
  | "show" :: "variable" :: idx :: _ | "show" :: "var" :: idx :: _ ->
      (try ShowVariable (int_of_string idx)
       with _ -> Help)
  | "show" :: "deps" :: idx :: _ | "deps" :: idx :: _ ->
      (try ShowDependencies (int_of_string idx)
       with _ -> Help)
  | "list" :: "constraints" :: [] -> ListConstraints None
  | "list" :: "constraints" :: limit :: _ ->
      (try ListConstraints (Some (int_of_string limit))
       with _ -> ListConstraints None)
  | "list" :: "variables" :: [] | "list" :: "vars" :: [] -> ListVariables None
  | "list" :: "variables" :: limit :: _ | "list" :: "vars" :: limit :: _ ->
      (try ListVariables (Some (int_of_string limit))
       with _ -> ListVariables None)
  | "find" :: "usages" :: idx :: _ | "usages" :: idx :: _ ->
      (try FindUsages (int_of_string idx)
       with _ -> Help)
  | "visualize" :: "path" :: indices ->
      let wire_indices = List.filter_map (fun s ->
        try Some (int_of_string s) with _ -> None
      ) indices in
      VisualizePath wire_indices
  | "export" :: filename :: _ -> ExportGraph filename
  | "set" :: key :: value :: _ -> SetConfig (key, value)
  | _ -> Help

let create_debug_session ?(interactive=true) circuit r1cs =
  {
    circuit = Some circuit;
    r1cs = Some r1cs;
    config = default_visualization_config;
    current_graph = None;
    history = [];
    interactive;
  }

let update_config session key value =
  let new_config = match (key, value) with
    | ("format", "dot") -> { session.config with format = GraphViz_DOT }
    | ("format", "svg") -> { session.config with format = SVG }
    | ("format", "png") -> { session.config with format = PNG }
    | ("layout", "hierarchical") -> { session.config with layout = Hierarchical }
    | ("layout", "force") -> { session.config with layout = ForceDirected }
    | ("layout", "circular") -> { session.config with layout = Circular }
    | ("constraints", "true") -> { session.config with show_constraints = true }
    | ("constraints", "false") -> { session.config with show_constraints = false }
    | ("intermediate", "true") -> { session.config with show_intermediate_vars = true }
    | ("intermediate", "false") -> { session.config with show_intermediate_vars = false }
    | ("max_nodes", n) ->
        (try { session.config with max_nodes = int_of_string n }
         with _ -> session.config)
    | _ -> session.config
  in
  { session with config = new_config }

let print_help () =
  print_endline "Interactive Debugger Commands:";
  print_endline "";
  print_endline "Inspection:";
  print_endline "  show constraint <n>     - Show details of constraint n";
  print_endline "  show variable <n>       - Show details of variable/wire n";
  print_endline "  show deps <n>          - Show dependencies of constraint n";
  print_endline "";
  print_endline "Listing:";
  print_endline "  list constraints [n]   - List all constraints (limit to n)";
  print_endline "  list variables [n]     - List all variables (limit to n)";
  print_endline "";
  print_endline "Search:";
  print_endline "  find usages <n>        - Find constraints using wire n";
  print_endline "  visualize path <n1 n2> - Visualize path between variables";
  print_endline "";
  print_endline " Configuration:";
  print_endline "  set format [dot|svg|png]     - Set output format";
  print_endline "  set layout [hierarchical|force|circular] - Set layout";
  print_endline "  set constraints [true|false] - Show/hide constraints";
  print_endline "  set intermediate [true|false] - Show/hide intermediate vars";
  print_endline "  set max_nodes <n>            - Limit visualization nodes";
  print_endline "";
  print_endline "Export:";
  print_endline "  export <filename>      - Export current visualization";
  print_endline "";
  print_endline " Other:";
  print_endline "  help                   - Show this help";
  print_endline "  quit                   - Exit debugger";
  print_endline ""

let execute_command session command =
  match (session.r1cs, command) with
  | (None, _) -> 
      print_endline "ERROR: No R1CS system loaded";
      session
  
  | (Some r1cs, ShowConstraint idx) ->
      (match Constraint_inspector.inspect_constraint r1cs idx with
       | Ok constraint_view ->
           print_endline (Printf.sprintf " Constraint %d:" idx);
           print_endline (Printf.sprintf "  Type: %s" constraint_view.constraint_type);
           print_endline (Printf.sprintf "  Variables: %s" (String.concat ", " constraint_view.variables));
           print_endline (Printf.sprintf "  Equation: %s" constraint_view.description);
           { session with history = (Printf.sprintf "show constraint %d" idx) :: session.history }
       | Error err ->
           print_endline (Printf.sprintf "ERROR: %s" (Error.error_to_string err));
           session)
  
  | (Some r1cs, ShowVariable wire_idx) ->
      if wire_idx >= 0 && wire_idx < r1cs.metadata.num_variables then (
        let var_type = 
          if wire_idx = 0 then "ONE (constant)"
          else if wire_idx <= r1cs.metadata.num_inputs then "Public Input"
          else if wire_idx <= r1cs.metadata.num_inputs + r1cs.metadata.num_private then "Private Input"
          else "Intermediate Variable"
        in
        let usages = Constraint_inspector.find_constraints_using_variable r1cs wire_idx in
        print_endline (Printf.sprintf " Variable w_%d:" wire_idx);
        print_endline (Printf.sprintf "  Type: %s" var_type);
        print_endline (Printf.sprintf "  Used in constraints: [%s]" 
          (String.concat ", " (List.map string_of_int usages)));
        { session with history = (Printf.sprintf "show variable %d" wire_idx) :: session.history }
      ) else (
        print_endline (Printf.sprintf "ERROR: Invalid wire index %d (max: %d)" wire_idx (r1cs.metadata.num_variables - 1));
        session
      )
  
  | (Some r1cs, ListConstraints limit) ->
      let max_constraints = Option.value limit ~default:(List.length r1cs.constraints) in
      let constraint_count = min max_constraints (List.length r1cs.constraints) in
      print_endline (Printf.sprintf " Constraints (showing %d of %d):" constraint_count (List.length r1cs.constraints));
      for i = 0 to constraint_count - 1 do
        match Constraint_inspector.inspect_constraint r1cs i with
        | Ok constraint_view ->
            print_endline (Printf.sprintf "  C%d (%s): %s" 
              i constraint_view.constraint_type 
              (if String.length constraint_view.description > 60 then 
                String.sub constraint_view.description 0 57 ^ "..."
               else constraint_view.description))
        | Error _ -> ()
      done;
      { session with history = "list constraints" :: session.history }
  
  | (Some r1cs, ListVariables limit) ->
      let max_vars = Option.value limit ~default:r1cs.metadata.num_variables in
      let var_count = min max_vars r1cs.metadata.num_variables in
      print_endline (Printf.sprintf " Variables (showing %d of %d):" var_count r1cs.metadata.num_variables);
      for i = 0 to var_count - 1 do
        let var_type = 
          if i = 0 then "ONE"
          else if i <= r1cs.metadata.num_inputs then "INPUT"
          else if i <= r1cs.metadata.num_inputs + r1cs.metadata.num_private then "PRIVATE"
          else "INTER"
        in
        print_endline (Printf.sprintf "  w_%d (%s)" i var_type)
      done;
      { session with history = "list variables" :: session.history }
  
  | (Some r1cs, FindUsages wire_idx) ->
      let usages = Constraint_inspector.find_constraints_using_variable r1cs wire_idx in
      print_endline (Printf.sprintf "Search: Wire w_%d is used in %d constraints:" wire_idx (List.length usages));
      List.iter (fun constraint_idx ->
        print_endline (Printf.sprintf "  - C%d" constraint_idx)
      ) usages;
      { session with history = (Printf.sprintf "find usages %d" wire_idx) :: session.history }
  
  | (_, ExportGraph filename) ->
      (match session.current_graph with
       | Some graph ->
           (match Graphviz_renderer.export_to_file session.config graph filename with
            | Ok () ->
                print_endline (Printf.sprintf "SUCCESS: Exported visualization to %s" filename);
                { session with history = (Printf.sprintf "export %s" filename) :: session.history }
            | Error err ->
                print_endline (Printf.sprintf "ERROR: Export failed: %s" (Error.error_to_string err));
                session)
       | None ->
           print_endline "ERROR: No visualization loaded. Generate a graph first.";
           session)
  
  | (_, SetConfig (key, value)) ->
      let new_session = update_config session key value in
      print_endline (Printf.sprintf "Set %s = %s" key value);
      { new_session with history = (Printf.sprintf "set %s %s" key value) :: session.history }
  
  | (Some r1cs, ShowDependencies constraint_idx) ->
      (match Constraint_inspector.inspect_constraint r1cs constraint_idx with
       | Ok constraint_view ->
           print_endline (Printf.sprintf "Dependencies for Constraint %d:" constraint_idx);
           let vars = List.filter_map (fun var_str ->
             if String.get var_str 0 = 'w' && String.get var_str 1 = '_' then
               try Some (int_of_string (String.sub var_str 2 (String.length var_str - 2)))
               with _ -> None
             else None
           ) constraint_view.variables in
           List.iter (fun var_idx ->
             let usages = Constraint_inspector.find_constraints_using_variable r1cs var_idx in
             print_endline (Printf.sprintf "  w_%d used in: [%s]" var_idx 
               (String.concat ", " (List.map (Printf.sprintf "C%d") usages)))
           ) vars;
           { session with history = (Printf.sprintf "show deps %d" constraint_idx) :: session.history }
       | Error err ->
           print_endline (Printf.sprintf "ERROR: %s" (Error.error_to_string err));
           session)
  
  | (_, VisualizePath _) ->
      print_endline "Path visualization not yet implemented";
      session
  
  | (_, Help) ->
      print_help ();
      session
  
  | (_, Quit) ->
      print_endline "Goodbye!";
      session

let run_interactive_session session =
  if not session.interactive then session
  else (
    print_endline "Camellia Interactive Debugger";
    print_endline "Type 'help' for commands, 'quit' to exit";
    print_endline "";
    
    let rec loop current_session =
      print_string "debug> ";
      flush stdout;
      match read_line () with
      | input ->
          let command = parse_command input in
          if command = Quit then current_session
          else loop (execute_command current_session command)
      | exception End_of_file ->
          print_endline "\nGoodbye!";
          current_session
    in
    
    loop session
  )

let batch_execute_commands session commands =
  List.fold_left (fun acc_session command_str ->
    let command = parse_command command_str in
    execute_command acc_session command
  ) session commands