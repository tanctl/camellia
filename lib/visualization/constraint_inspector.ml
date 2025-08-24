open Visualization_types
open Error

(* constraint inspection and analysis tools *)

let format_coefficient coeff =
  if String.contains coeff '.' then coeff
  else if coeff = "1" then ""
  else if coeff = "-1" then "-"
  else coeff

let linear_combination_to_string lc =
  let terms = List.map (fun (wire_idx, coeff) ->
    let formatted_coeff = format_coefficient coeff in
    if formatted_coeff = "" then Printf.sprintf "w_%d" wire_idx
    else if formatted_coeff = "-" then Printf.sprintf "-w_%d" wire_idx
    else Printf.sprintf "%s*w_%d" formatted_coeff wire_idx
  ) lc in
  match terms with
  | [] -> "0"
  | [single] -> single
  | _ -> String.concat " + " terms

let constraint_to_equation constraint_triple =
  let a_str = linear_combination_to_string constraint_triple.R1cs.a in
  let b_str = linear_combination_to_string constraint_triple.R1cs.b in
  let c_str = linear_combination_to_string constraint_triple.R1cs.c in
  Printf.sprintf "(%s) * (%s) = %s" a_str b_str c_str

let analyze_constraint_type constraint_triple =
  let a_terms = List.length constraint_triple.R1cs.a in
  let b_terms = List.length constraint_triple.R1cs.b in
  let c_terms = List.length constraint_triple.R1cs.c in
  
  match (a_terms, b_terms, c_terms) with
  | (1, 1, 1) -> "linear" (* simple assignment *)
  | (1, 1, _) -> "linear_combination" (* linear combination *)
  | (_, _, 1) -> "multiplication" (* quadratic *)
  | _ -> "complex"

let get_constraint_variables constraint_triple =
  let collect_vars lc = List.map (fun (wire_idx, _) -> wire_idx) lc in
  let a_vars = collect_vars constraint_triple.R1cs.a in
  let b_vars = collect_vars constraint_triple.R1cs.b in
  let c_vars = collect_vars constraint_triple.R1cs.c in
  
  let all_vars = a_vars @ b_vars @ c_vars in
  let unique_vars = List.sort_uniq compare all_vars in
  List.map (Printf.sprintf "w_%d") unique_vars

let inspect_constraint r1cs constraint_idx =
  if constraint_idx < 0 || constraint_idx >= List.length r1cs.R1cs.constraints then
    Error (circuit_error (Printf.sprintf "Invalid constraint index: %d" constraint_idx) ())
  else
    let constraint_triple = List.nth r1cs.R1cs.constraints constraint_idx in
    let constraint_view = {
      constraint_id = constraint_idx;
      constraint_type = analyze_constraint_type constraint_triple;
      variables = get_constraint_variables constraint_triple;
      coefficients = [
        linear_combination_to_string constraint_triple.R1cs.a;
        linear_combination_to_string constraint_triple.R1cs.b;
        linear_combination_to_string constraint_triple.R1cs.c;
      ];
      description = constraint_to_equation constraint_triple;
      satisfied = None; (* would need witness to check *)
    } in
    Ok constraint_view

let inspect_all_constraints r1cs =
  let constraints = List.mapi (fun idx _ ->
    match inspect_constraint r1cs idx with
    | Ok view -> view
    | Error _ -> {
        constraint_id = idx;
        constraint_type = "error";
        variables = [];
        coefficients = [];
        description = "Failed to analyze constraint";
        satisfied = None;
      }
  ) r1cs.R1cs.constraints in
  
  {
    circuit_name = "R1CS System";
    total_constraints = List.length r1cs.R1cs.constraints;
    total_variables = r1cs.R1cs.metadata.num_variables;
    constraints;
    critical_path = []; (* would need circuit analysis *)
    bottlenecks = []; (* would need performance analysis *)
    timestamp = Unix.time ();
  }

let find_constraints_using_variable r1cs wire_idx =
  let matching_constraints = ref [] in
  List.iteri (fun constraint_idx constraint_triple ->
    let uses_wire lc = List.exists (fun (w, _) -> w = wire_idx) lc in
    if uses_wire constraint_triple.R1cs.a || 
       uses_wire constraint_triple.R1cs.b || 
       uses_wire constraint_triple.R1cs.c then
      matching_constraints := constraint_idx :: !matching_constraints
  ) r1cs.R1cs.constraints;
  List.rev !matching_constraints

let analyze_constraint_dependencies r1cs =
  let dependency_map = Hashtbl.create (List.length r1cs.R1cs.constraints) in
  
  List.iteri (fun constraint_idx constraint_triple ->
    let get_vars lc = List.map fst lc in
    let input_vars = get_vars constraint_triple.R1cs.a @ get_vars constraint_triple.R1cs.b in
    let output_vars = get_vars constraint_triple.R1cs.c in
    
    Hashtbl.add dependency_map constraint_idx (input_vars, output_vars)
  ) r1cs.R1cs.constraints;
  
  dependency_map

let constraint_complexity_score constraint_triple =
  let count_terms lc = List.length lc in
  let count_unique_vars lc = 
    List.map fst lc 
    |> List.sort_uniq compare 
    |> List.length 
  in
  
  let a_terms = count_terms constraint_triple.R1cs.a in
  let b_terms = count_terms constraint_triple.R1cs.b in
  let c_terms = count_terms constraint_triple.R1cs.c in
  let unique_vars = count_unique_vars (constraint_triple.R1cs.a @ constraint_triple.R1cs.b @ constraint_triple.R1cs.c) in
  
  (* complexity score based on terms and variables *)
  (a_terms + b_terms + c_terms) * unique_vars

let rank_constraints_by_complexity r1cs =
  let scored_constraints = List.mapi (fun idx constraint_triple ->
    (idx, constraint_complexity_score constraint_triple)
  ) r1cs.R1cs.constraints in
  
  List.sort (fun (_, score1) (_, score2) -> compare score2 score1) scored_constraints

let format_constraint_summary constraint_view =
  Printf.sprintf "C%d (%s): %s\n  Variables: [%s]\n  Equation: %s"
    constraint_view.constraint_id
    constraint_view.constraint_type
    (match constraint_view.satisfied with
     | Some true -> "SUCCESS: SATISFIED"
     | Some false -> "ERROR: UNSATISFIED"
     | None -> "UNKNOWN")
    (String.concat ", " constraint_view.variables)
    constraint_view.description

let export_constraint_inspection inspection filename =
  try
    let oc = open_out filename in
    Printf.fprintf oc "# Constraint Inspection Report\n\n";
    Printf.fprintf oc "**Circuit:** %s\n" inspection.circuit_name;
    Printf.fprintf oc "**Timestamp:** %s\n" (string_of_float inspection.timestamp);
    Printf.fprintf oc "**Total Constraints:** %d\n" inspection.total_constraints;
    Printf.fprintf oc "**Total Variables:** %d\n\n" inspection.total_variables;
    
    Printf.fprintf oc "## Constraint Details\n\n";
    List.iteri (fun _idx constraint_view ->
      Printf.fprintf oc "### Constraint %d\n\n" constraint_view.constraint_id;
      Printf.fprintf oc "- **Type:** %s\n" constraint_view.constraint_type;
      Printf.fprintf oc "- **Variables:** %s\n" (String.concat ", " constraint_view.variables);
      Printf.fprintf oc "- **Equation:** `%s`\n" constraint_view.description;
      (match constraint_view.satisfied with
       | Some true -> Printf.fprintf oc "- **Status:** SUCCESS: Satisfied\n"
       | Some false -> Printf.fprintf oc "- **Status:** ERROR: Unsatisfied\n"
       | None -> Printf.fprintf oc "- **Status:** UNKNOWN\n");
      Printf.fprintf oc "\n"
    ) inspection.constraints;
    
    if List.length inspection.critical_path > 0 then (
      Printf.fprintf oc "## Critical Path\n\n";
      List.iter (fun step ->
        Printf.fprintf oc "- %s\n" step
      ) inspection.critical_path;
      Printf.fprintf oc "\n"
    );
    
    if List.length inspection.bottlenecks > 0 then (
      Printf.fprintf oc "## Identified Bottlenecks\n\n";
      List.iter (fun bottleneck ->
        Printf.fprintf oc "- %s\n" bottleneck
      ) inspection.bottlenecks;
      Printf.fprintf oc "\n"
    );
    
    close_out oc;
    Ok ()
  with
  | exn -> Error (circuit_error ("Failed to export inspection: " ^ Printexc.to_string exn) ())