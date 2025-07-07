open Ast

type log_level = 
  | Silent
  | Error 
  | Warning
  | Info
  | Debug
  | Trace

type phase = 
  | Parse
  | TypeCheck
  | AstProcess
  | R1CSGen
  | Optimize
  | Backend

type constraint_info = {
  id: int;
  left: string;
  right: string;
  output: string;
  description: string;
}

type wire_info = {
  id: int;
  name: string;
  wire_type: string;
  value: string option;
}

type timing_info = {
  phase: phase;
  start_time: float;
  end_time: float;
  duration: float;
}

type statistics = {
  constraint_count: int;
  wire_count: int;
  public_input_count: int;
  private_input_count: int;
  intermediate_count: int;
  total_operations: int;
}

type debug_context = {
  level: log_level;
  verbose: bool;
  circuit_name: string;
  output_channel: out_channel;
  mutable stats: statistics;
  mutable constraints: constraint_info list;
  mutable wires: wire_info list;
  mutable timers: timing_info list;
  mutable current_phase: phase option;
  mutable phase_start: float option;
}

let log_level_to_string = function
  | Silent -> "SILENT"
  | Error -> "ERROR"
  | Warning -> "WARN"
  | Info -> "INFO"
  | Debug -> "DEBUG"
  | Trace -> "TRACE"

let phase_to_string = function
  | Parse -> "PARSE"
  | TypeCheck -> "TYPECHECK"
  | AstProcess -> "AST_PROCESS"
  | R1CSGen -> "R1CS_GEN"
  | Optimize -> "OPTIMIZE"
  | Backend -> "BACKEND"

let log_level_priority = function
  | Silent -> 0
  | Error -> 1
  | Warning -> 2
  | Info -> 3
  | Debug -> 4
  | Trace -> 5

let empty_stats = {
  constraint_count = 0;
  wire_count = 0;
  public_input_count = 0;
  private_input_count = 0;
  intermediate_count = 0;
  total_operations = 0;
}

let create_context ?(level=Info) ?(verbose=false) ?(output=stdout) circuit_name = {
  level;
  verbose;
  circuit_name;
  output_channel = output;
  stats = empty_stats;
  constraints = [];
  wires = [];
  timers = [];
  current_phase = None;
  phase_start = None;
}

let should_log ctx level =
  log_level_priority level <= log_level_priority ctx.level

let timestamp () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02d:%02d:%02d.%03d" 
    tm.tm_hour tm.tm_min tm.tm_sec 
    (int_of_float (mod_float (Unix.gettimeofday ()) 1.0 *. 1000.0))

let log ctx level fmt =
  if should_log ctx level then
    let ts = timestamp () in
    let level_str = log_level_to_string level in
    let phase_str = match ctx.current_phase with
      | Some p -> Printf.sprintf "[%s]" (phase_to_string p)
      | None -> ""
    in
    Printf.fprintf ctx.output_channel "[%s] %s %s %s: " 
      ts level_str ctx.circuit_name phase_str;
    Printf.kfprintf (fun oc -> Printf.fprintf oc "\n"; flush oc) 
      ctx.output_channel fmt
  else
    Printf.ifprintf ctx.output_channel fmt

let start_phase ctx phase =
  let current_time = Unix.gettimeofday () in
  ctx.current_phase <- Some phase;
  ctx.phase_start <- Some current_time;
  log ctx Info "Starting phase %s" (phase_to_string phase)

let end_phase ctx =
  match ctx.current_phase, ctx.phase_start with
  | Some phase, Some start_time ->
      let end_time = Unix.gettimeofday () in
      let duration = end_time -. start_time in
      let timing = { phase; start_time; end_time; duration } in
      ctx.timers <- timing :: ctx.timers;
      log ctx Info "Completed phase %s in %.3fms" 
        (phase_to_string phase) (duration *. 1000.0);
      ctx.current_phase <- None;
      ctx.phase_start <- None
  | _ ->
      log ctx Warning "end_phase called without matching start_phase"

let trace_constraint ctx ~id ~left ~right ~output ~description =
  let constraint_info = { id; left; right; output; description } in
  ctx.constraints <- constraint_info :: ctx.constraints;
  ctx.stats <- { ctx.stats with 
    constraint_count = ctx.stats.constraint_count + 1;
    total_operations = ctx.stats.total_operations + 1 
  };
  log ctx Trace "Constraint %d: %s = %s * %s (%s)" 
    id output left right description

let trace_wire ctx ~id ~name ~wire_type ?value () =
  let wire_info = { id; name; wire_type; value } in
  ctx.wires <- wire_info :: ctx.wires;
  ctx.stats <- { ctx.stats with wire_count = ctx.stats.wire_count + 1 };
  let value_str = match value with
    | Some v -> Printf.sprintf " = %s" v
    | None -> ""
  in
  log ctx Trace "Wire %d: %s [%s]%s" id name wire_type value_str

let trace_ast ctx node_type description = 
  log ctx Debug "AST: Processing %s - %s" node_type description

let trace_ast_expr ctx expr =
  let expr_str = expr_to_string expr in
  let depth = expr_depth expr in
  let vars = get_vars expr in
  let var_count = List.length vars in
  trace_ast ctx "Expression" 
    (Printf.sprintf "%s (depth=%d, vars=%d)" expr_str depth var_count)

let trace_ast_stmt ctx stmt =
  let stmt_str = stmt_to_string stmt in
  trace_ast ctx "Statement" stmt_str

let trace_ast_circuit ctx circuit =
  let input_count = List.length circuit.inputs in
  let private_count = List.length circuit.private_inputs in
  let stmt_count = List.length circuit.body in
  ctx.stats <- { ctx.stats with 
    public_input_count = input_count;
    private_input_count = private_count;
  };
  trace_ast ctx "Circuit" 
    (Printf.sprintf "%s (inputs=%d, private=%d, statements=%d)" 
      circuit.name input_count private_count stmt_count)

let update_stats ctx ~public_inputs ~private_inputs ~intermediates =
  ctx.stats <- { ctx.stats with
    public_input_count = public_inputs;
    private_input_count = private_inputs;
    intermediate_count = intermediates;
  }

let add_operation ctx =
  ctx.stats <- { ctx.stats with 
    total_operations = ctx.stats.total_operations + 1 
  }

let print_statistics ctx =
  let total_time = List.fold_left (fun acc timing -> 
    acc +. timing.duration) 0.0 ctx.timers in
  
  Printf.fprintf ctx.output_channel "\n=== Compilation Statistics for %s ===\n" 
    ctx.circuit_name;
  Printf.fprintf ctx.output_channel "Constraints: %d\n" ctx.stats.constraint_count;
  Printf.fprintf ctx.output_channel "Wires: %d\n" ctx.stats.wire_count;
  Printf.fprintf ctx.output_channel "Public Inputs: %d\n" ctx.stats.public_input_count;
  Printf.fprintf ctx.output_channel "Private Inputs: %d\n" ctx.stats.private_input_count;
  Printf.fprintf ctx.output_channel "Intermediate Variables: %d\n" ctx.stats.intermediate_count;
  Printf.fprintf ctx.output_channel "Total Operations: %d\n" ctx.stats.total_operations;
  Printf.fprintf ctx.output_channel "Total Compilation Time: %.3fms\n" 
    (total_time *. 1000.0);
  
  if ctx.verbose then begin
    Printf.fprintf ctx.output_channel "\n=== Phase Timings ===\n";
    List.rev ctx.timers |> List.iter (fun timing ->
      Printf.fprintf ctx.output_channel "%s: %.3fms\n" 
        (phase_to_string timing.phase) (timing.duration *. 1000.0)
    );
  end;
  
  flush ctx.output_channel

let print_constraint_trace ctx =
  Printf.fprintf ctx.output_channel "\n=== Detailed Constraint Trace ===\n";
  let constraint_count = List.length ctx.constraints in
  Printf.fprintf ctx.output_channel "Total constraints: %d\n" constraint_count

let print_wire_trace ctx =
  Printf.fprintf ctx.output_channel "\n=== Wire Allocation Trace ===\n";
  let print_single_wire wire_rec =
    let value_str = match wire_rec.value with
      | Some v -> Printf.sprintf " = %s" v
      | None -> ""
    in
    Printf.fprintf ctx.output_channel "W%d: %s [%s]%s\n" 
      wire_rec.id wire_rec.name wire_rec.wire_type value_str
  in
  List.iter print_single_wire (List.rev ctx.wires)

let print_detailed_trace ctx =
  if not ctx.verbose then () else begin
    print_constraint_trace ctx;
    print_wire_trace ctx;
    flush ctx.output_channel
  end

let print_summary ctx =
  print_statistics ctx;
  print_detailed_trace ctx

let measure_time ctx phase f =
  start_phase ctx phase;
  let result = f () in
  end_phase ctx;
  result

let measure_time_result ctx phase f =
  start_phase ctx phase;
  let result = 
    try f () with
    | exn ->
        end_phase ctx;
        raise exn
  in
  end_phase ctx;
  result

let get_stats ctx = ctx.stats

let get_constraint_count ctx = ctx.stats.constraint_count
let get_wire_count ctx = ctx.stats.wire_count
let get_timing_info ctx = List.rev ctx.timers

let clear_context ctx =
  ctx.stats <- empty_stats;
  ctx.constraints <- [];
  ctx.wires <- [];
  ctx.timers <- [];
  ctx.current_phase <- None;
  ctx.phase_start <- None

let with_debug_context ?(level=Info) ?(verbose=false) circuit_name f =
  let ctx = create_context ~level ~verbose circuit_name in
  let result = f ctx in
  print_summary ctx;
  result

let format_constraint_summary constraints =
  let count = List.length constraints in
  let types = List.fold_left (fun acc c ->
    let parts = String.split_on_char ' ' c.description in
    let op_type = if List.length parts > 0 then List.hd parts else "unknown" in
    let current_count = try List.assoc op_type acc with Not_found -> 0 in
    (op_type, current_count + 1) :: (List.remove_assoc op_type acc)
  ) [] constraints in
  
  Printf.sprintf "Total: %d constraints (%s)" count
    (String.concat ", " (List.map (fun (t, c) -> Printf.sprintf "%s: %d" t c) types))

let export_debug_info ctx filename =
  let oc = open_out filename in
  Printf.fprintf oc "# Debug Export for Circuit: %s\n" ctx.circuit_name;
  Printf.fprintf oc "# Generated at: %s\n\n" (timestamp ());
  
  Printf.fprintf oc "## Statistics\n";
  Printf.fprintf oc "constraints=%d\n" ctx.stats.constraint_count;
  Printf.fprintf oc "wires=%d\n" ctx.stats.wire_count;
  Printf.fprintf oc "public_inputs=%d\n" ctx.stats.public_input_count;
  Printf.fprintf oc "private_inputs=%d\n" ctx.stats.private_input_count;
  Printf.fprintf oc "intermediates=%d\n" ctx.stats.intermediate_count;
  Printf.fprintf oc "operations=%d\n" ctx.stats.total_operations;
  
  Printf.fprintf oc "\n## Constraints\n";
  Printf.fprintf oc "constraint_count=%d\n" (List.length ctx.constraints);
  
  Printf.fprintf oc "\n## Wires\n";
  List.iter (fun w ->
    let value_str = match w.value with Some v -> v | None -> "" in
    Printf.fprintf oc "wire,%d,%s,%s,%s\n" 
      w.id w.name w.wire_type value_str
  ) (List.rev ctx.wires);
  
  Printf.fprintf oc "\n## Timings\n";
  List.rev ctx.timers |> List.iter (fun t ->
    Printf.fprintf oc "timing,%s,%.6f\n" (phase_to_string t.phase) t.duration
  );
  
  close_out oc;
  log ctx Info "Debug info exported to %s" filename