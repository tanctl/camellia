open Visualization_types

(* build visualization graphs from AST and R1CS *)

let node_id_counter = ref 0

let fresh_node_id prefix =
  incr node_id_counter;
  Printf.sprintf "%s_%d" prefix !node_id_counter

let reset_node_counter () =
  node_id_counter := 0

let build_expression_graph expr =
  let nodes = ref [] in
  let edges = ref [] in
  
  let rec build_expr_nodes = function
    | Ast.Var name ->
        let node_id = "var_" ^ name in
        let node = {
          id = node_id;
          label = name;
          node_type = Variable name;
          position = None;
          metadata = [("type", "variable")];
        } in
        nodes := node :: !nodes;
        node_id
    
    | Ast.Const value ->
        let node_id = fresh_node_id "const" in
        let node = {
          id = node_id;
          label = value;
          node_type = Constant value;
          position = None;
          metadata = [("value", value)];
        } in
        nodes := node :: !nodes;
        node_id
    
    | Ast.Add (e1, e2) ->
        let node_id = fresh_node_id "add" in
        let left_id = build_expr_nodes e1 in
        let right_id = build_expr_nodes e2 in
        
        let node = {
          id = node_id;
          label = "+";
          node_type = Operation ("add", [left_id; right_id]);
          position = None;
          metadata = [("operation", "addition")];
        } in
        nodes := node :: !nodes;
        
        let edge1 = {
          from_node = left_id;
          to_node = node_id;
          edge_type = DataFlow;
          label = Some "left";
          weight = Some 1.0;
        } in
        let edge2 = {
          from_node = right_id;
          to_node = node_id;
          edge_type = DataFlow;
          label = Some "right";
          weight = Some 1.0;
        } in
        edges := edge2 :: edge1 :: !edges;
        node_id
    
    | Ast.Sub (e1, e2) ->
        let node_id = fresh_node_id "sub" in
        let left_id = build_expr_nodes e1 in
        let right_id = build_expr_nodes e2 in
        
        let node = {
          id = node_id;
          label = "-";
          node_type = Operation ("sub", [left_id; right_id]);
          position = None;
          metadata = [("operation", "subtraction")];
        } in
        nodes := node :: !nodes;
        
        let edge1 = {
          from_node = left_id;
          to_node = node_id;
          edge_type = DataFlow;
          label = Some "left";
          weight = Some 1.0;
        } in
        let edge2 = {
          from_node = right_id;
          to_node = node_id;
          edge_type = DataFlow;
          label = Some "right";
          weight = Some 1.0;
        } in
        edges := edge2 :: edge1 :: !edges;
        node_id
    
    | Ast.Mul (e1, e2) ->
        let node_id = fresh_node_id "mul" in
        let left_id = build_expr_nodes e1 in
        let right_id = build_expr_nodes e2 in
        
        let node = {
          id = node_id;
          label = "*";
          node_type = Operation ("mul", [left_id; right_id]);
          position = None;
          metadata = [("operation", "multiplication")];
        } in
        nodes := node :: !nodes;
        
        let edge1 = {
          from_node = left_id;
          to_node = node_id;
          edge_type = DataFlow;
          label = Some "left";
          weight = Some 2.0;
        } in
        let edge2 = {
          from_node = right_id;
          to_node = node_id;
          edge_type = DataFlow;
          label = Some "right";
          weight = Some 2.0;
        } in
        edges := edge2 :: edge1 :: !edges;
        node_id
    
    | Ast.Equal (e1, e2) ->
        let node_id = fresh_node_id "eq" in
        let left_id = build_expr_nodes e1 in
        let right_id = build_expr_nodes e2 in
        
        let node = {
          id = node_id;
          label = "=";
          node_type = Operation ("equal", [left_id; right_id]);
          position = None;
          metadata = [("operation", "equality")];
        } in
        nodes := node :: !nodes;
        
        let edge1 = {
          from_node = left_id;
          to_node = node_id;
          edge_type = DataFlow;
          label = Some "left";
          weight = Some 1.0;
        } in
        let edge2 = {
          from_node = right_id;
          to_node = node_id;
          edge_type = DataFlow;
          label = Some "right";
          weight = Some 1.0;
        } in
        edges := edge2 :: edge1 :: !edges;
        node_id
    
    | Ast.Poseidon args ->
        let node_id = fresh_node_id "poseidon" in
        let arg_ids = List.mapi (fun i arg ->
          let arg_id = build_expr_nodes arg in
          let edge = {
            from_node = arg_id;
            to_node = node_id;
            edge_type = DataFlow;
            label = Some ("arg" ^ string_of_int i);
            weight = Some 3.0;
          } in
          edges := edge :: !edges;
          arg_id
        ) args in
        
        let node = {
          id = node_id;
          label = "Poseidon";
          node_type = Operation ("poseidon", arg_ids);
          position = None;
          metadata = [("operation", "hash"); ("args", string_of_int (List.length args))];
        } in
        nodes := node :: !nodes;
        node_id
  in
  
  let root_id = build_expr_nodes expr in
  (!nodes, !edges, root_id)

let build_circuit_graph config circuit =
  reset_node_counter ();
  let all_nodes = ref [] in
  let all_edges = ref [] in
  let variable_map = Hashtbl.create 64 in
  
  (* add input nodes *)
  List.iter (fun input_name ->
    let node_id = "input_" ^ input_name in
    let node = {
      id = node_id;
      label = input_name;
      node_type = Input input_name;
      position = None;
      metadata = [("type", "public_input")];
    } in
    all_nodes := node :: !all_nodes;
    Hashtbl.add variable_map input_name node_id
  ) circuit.Ast.inputs;
  
  (* add private input nodes *)
  List.iter (fun private_name ->
    let node_id = "private_" ^ private_name in
    let node = {
      id = node_id;
      label = private_name;
      node_type = PrivateInput private_name;
      position = None;
      metadata = [("type", "private_input")];
    } in
    all_nodes := node :: !all_nodes;
    Hashtbl.add variable_map private_name node_id
  ) circuit.Ast.private_inputs;
  
  (* process statements *)
  List.iteri (fun stmt_idx stmt ->
    match stmt with
    | Ast.Assign (var_name, expr) ->
        let (expr_nodes, expr_edges, root_id) = build_expression_graph expr in
        all_nodes := expr_nodes @ !all_nodes;
        all_edges := expr_edges @ !all_edges;
        
        (* create assignment edge *)
        let var_node_id = "var_" ^ var_name in
        if not (Hashtbl.mem variable_map var_name) then (
          let var_node = {
            id = var_node_id;
            label = var_name;
            node_type = Variable var_name;
            position = None;
            metadata = [("assigned_at", string_of_int stmt_idx)];
          } in
          all_nodes := var_node :: !all_nodes;
          Hashtbl.add variable_map var_name var_node_id
        );
        
        let assign_edge = {
          from_node = root_id;
          to_node = var_node_id;
          edge_type = DataFlow;
          label = Some "assign";
          weight = Some 1.0;
        } in
        all_edges := assign_edge :: !all_edges
    
    | Ast.Constraint constraint_expr ->
        let constraint_id = fresh_node_id "constraint" in
        let (expr_nodes, expr_edges, root_id) = build_expression_graph constraint_expr in
        all_nodes := expr_nodes @ !all_nodes;
        all_edges := expr_edges @ !all_edges;
        
        if config.show_constraints then (
          let constraint_node = {
            id = constraint_id;
            label = Printf.sprintf "C%d" stmt_idx;
            node_type = Constraint constraint_id;
            position = None;
            metadata = [("constraint_index", string_of_int stmt_idx)];
          } in
          all_nodes := constraint_node :: !all_nodes;
          
          let constraint_edge = {
            from_node = root_id;
            to_node = constraint_id;
            edge_type = ConstraintDep;
            label = Some "enforces";
            weight = Some 1.0;
          } in
          all_edges := constraint_edge :: !all_edges
        )
  ) circuit.Ast.body;
  
  (* filter nodes based on config *)
  let filtered_nodes = if config.show_intermediate_vars then !all_nodes
    else List.filter (fun node ->
      match node.node_type with
      | Variable _ -> false
      | _ -> true
    ) !all_nodes in
  
  let final_nodes = if List.length filtered_nodes > config.max_nodes then
    List.take config.max_nodes filtered_nodes
  else filtered_nodes in
  
  let node_ids = List.fold_left (fun acc node -> node.id :: acc) [] final_nodes in
  let filtered_edges = List.filter (fun edge ->
    List.mem edge.from_node node_ids && List.mem edge.to_node node_ids
  ) !all_edges in
  
  {
    nodes = List.rev final_nodes;
    edges = List.rev filtered_edges;
    metadata = [
      ("circuit_name", circuit.Ast.name);
      ("total_nodes", string_of_int (List.length final_nodes));
      ("total_edges", string_of_int (List.length filtered_edges));
      ("inputs", string_of_int (List.length circuit.Ast.inputs));
      ("private_inputs", string_of_int (List.length circuit.Ast.private_inputs));
      ("statements", string_of_int (List.length circuit.Ast.body));
    ];
  }

let build_r1cs_constraint_graph _config (r1cs: R1cs.r1cs_system) =
  reset_node_counter ();
  let nodes = ref [] in
  let edges = ref [] in
  
  (* create nodes for each variable *)
  for i = 0 to r1cs.metadata.num_variables - 1 do
    let var_id = Printf.sprintf "w%d" i in
    let var_type = 
      if i = 0 then Variable "ONE"
      else if i <= r1cs.metadata.num_inputs then Variable ("input_" ^ string_of_int i)
      else if i <= r1cs.metadata.num_inputs + r1cs.metadata.num_private then Variable ("private_" ^ string_of_int (i - r1cs.metadata.num_inputs))
      else Variable ("intermediate_" ^ string_of_int i)
    in
    
    let node = {
      id = var_id;
      label = Printf.sprintf "w_%d" i;
      node_type = var_type;
      position = None;
      metadata = [("wire_index", string_of_int i)];
    } in
    nodes := node :: !nodes
  done;
  
  (* create constraint nodes and edges *)
  List.iteri (fun constraint_idx constraint_triple ->
    let constraint_id = Printf.sprintf "c%d" constraint_idx in
    let constraint_node = {
      id = constraint_id;
      label = Printf.sprintf "C_%d" constraint_idx;
      node_type = Constraint constraint_id;
      position = None;
      metadata = [
        ("constraint_index", string_of_int constraint_idx);
        ("type", "r1cs");
      ];
    } in
    nodes := constraint_node :: !nodes;
    
    (* add edges from variables used in A, B, C *)
    let add_constraint_edges lc label =
      List.iter (fun (wire_idx, coeff) ->
        if wire_idx < r1cs.metadata.num_variables then (
          let wire_id = Printf.sprintf "w%d" wire_idx in
          let edge = {
            from_node = wire_id;
            to_node = constraint_id;
            edge_type = ConstraintDep;
            label = Some (label ^ ":" ^ coeff);
            weight = (try Some (abs_float (float_of_string coeff)) with _ -> Some 1.0);
          } in
          edges := edge :: !edges
        )
      ) lc
    in
    
    add_constraint_edges constraint_triple.R1cs.a "A";
    add_constraint_edges constraint_triple.R1cs.b "B";
    add_constraint_edges constraint_triple.R1cs.c "C"
  ) r1cs.constraints;
  
  {
    nodes = List.rev !nodes;
    edges = List.rev !edges;
    metadata = [
      ("type", "r1cs_constraints");
      ("num_variables", string_of_int r1cs.metadata.num_variables);
      ("num_constraints", string_of_int (List.length r1cs.constraints));
      ("num_inputs", string_of_int r1cs.metadata.num_inputs);
      ("num_private", string_of_int r1cs.metadata.num_private);
    ];
  }

(* helper function to take first n elements *)
let rec take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: take (n - 1) xs