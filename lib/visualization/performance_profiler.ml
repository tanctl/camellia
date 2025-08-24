open Error

(* performance profiling for circuit compilation and execution *)

type profiling_event = {
  name: string;
  start_time: float;
  end_time: float;
  duration_ms: float;
  memory_before: int;
  memory_after: int;
  memory_delta: int;
  metadata: (string * string) list;
}

type profiling_session = {
  session_id: string;
  start_time: float;
  events: profiling_event list;
  current_events: (string * float * int) list; (* name, start_time, memory_before *)
  total_memory_allocated: int;
  peak_memory_usage: int;
}

let get_memory_usage () =
  try
    let gc_stats = Gc.stat () in
    gc_stats.heap_words * (Sys.word_size / 8)
  with _ -> 0

let create_profiling_session name =
  {
    session_id = name ^ "_" ^ (string_of_float (Unix.time ()));
    start_time = Unix.gettimeofday ();
    events = [];
    current_events = [];
    total_memory_allocated = 0;
    peak_memory_usage = get_memory_usage ();
  }

let start_event session event_name _metadata =
  let current_time = Unix.gettimeofday () in
  let current_memory = get_memory_usage () in
  let new_current = (event_name, current_time, current_memory) :: session.current_events in
  { session with current_events = new_current }

let end_event session event_name =
  let current_time = Unix.gettimeofday () in
  let current_memory = get_memory_usage () in
  
  let rec find_and_remove name = function
    | [] -> (None, [])
    | (n, start_time, memory_before) :: rest when n = name ->
        (Some (start_time, memory_before), rest)
    | head :: rest ->
        let (found, remaining) = find_and_remove name rest in
        (found, head :: remaining)
  in
  
  match find_and_remove event_name session.current_events with
  | (Some (start_time, memory_before), remaining_events) ->
      let event = {
        name = event_name;
        start_time;
        end_time = current_time;
        duration_ms = (current_time -. start_time) *. 1000.0;
        memory_before;
        memory_after = current_memory;
        memory_delta = current_memory - memory_before;
        metadata = [];
      } in
      let new_peak = max session.peak_memory_usage current_memory in
      {
        session with 
        events = event :: session.events;
        current_events = remaining_events;
        peak_memory_usage = new_peak;
        total_memory_allocated = session.total_memory_allocated + max 0 event.memory_delta;
      }
  | (None, _) ->
      (* event not found, create a synthetic event *)
      let event = {
        name = event_name;
        start_time = current_time;
        end_time = current_time;
        duration_ms = 0.0;
        memory_before = current_memory;
        memory_after = current_memory;
        memory_delta = 0;
        metadata = [("error", "start_not_found")];
      } in
      { session with events = event :: session.events }

let profile_function session name f =
  let profiling_session = start_event session name [] in
  let result = 
    try Ok (f ())
    with exn -> Error (circuit_error ("Profiled function failed: " ^ Printexc.to_string exn) ())
  in
  let final_session = end_event profiling_session name in
  (result, final_session)

type session_summary = {
  total_duration_ms: float;
  event_count: int;
  total_event_time_ms: float;
  overhead_ms: float;
  peak_memory_mb: float;
  total_allocated_mb: float;
}

let get_session_summary session =
  let total_duration = Unix.gettimeofday () -. session.start_time in
  let completed_events = List.rev session.events in
  let event_count = List.length completed_events in
  let total_event_time = List.fold_left (fun acc event -> acc +. event.duration_ms) 0.0 completed_events in
  
  let memory_mb = float_of_int session.peak_memory_usage /. (1024.0 *. 1024.0) in
  
  {
    session_id = session.session_id;
    start_time = session.start_time;
    events = completed_events;
    current_events = [];
    total_memory_allocated = session.total_memory_allocated;
    peak_memory_usage = session.peak_memory_usage;
  }, {
    total_duration_ms = total_duration *. 1000.0;
    event_count;
    total_event_time_ms = total_event_time;
    overhead_ms = (total_duration *. 1000.0) -. total_event_time;
    peak_memory_mb = memory_mb;
    total_allocated_mb = float_of_int session.total_memory_allocated /. (1024.0 *. 1024.0);
  }

let format_duration_ms ms =
  if ms < 1.0 then Printf.sprintf "%.3fms" ms
  else if ms < 1000.0 then Printf.sprintf "%.1fms" ms
  else Printf.sprintf "%.2fs" (ms /. 1000.0)

let format_memory_bytes bytes =
  let mb = float_of_int bytes /. (1024.0 *. 1024.0) in
  if mb < 1.0 then
    let kb = float_of_int bytes /. 1024.0 in
    Printf.sprintf "%.1fKB" kb
  else
    Printf.sprintf "%.2fMB" mb

let print_profiling_report session =
  let (final_session, summary) = get_session_summary session in
  
  print_endline "PERFORMANCE PROFILING REPORT";
  print_endline "═══════════════════════════════";
  print_endline (Printf.sprintf "Session: %s" final_session.session_id);
  print_endline (Printf.sprintf "Total Duration: %s" (format_duration_ms summary.total_duration_ms));
  print_endline (Printf.sprintf "Events: %d" summary.event_count);
  print_endline (Printf.sprintf "Peak Memory: %s" (format_memory_bytes session.peak_memory_usage));
  print_endline (Printf.sprintf "Total Allocated: %s" (format_memory_bytes session.total_memory_allocated));
  print_endline "";
  
  print_endline "EVENT BREAKDOWN:";
  print_endline "┌─────────────────────────┬──────────────┬─────────────┬──────────────┐";
  print_endline "│ Event Name              │ Duration     │ Memory Δ    │ % of Total   │";
  print_endline "├─────────────────────────┼──────────────┼─────────────┼──────────────┤";
  
  let sorted_events = List.sort (fun e1 e2 -> compare e2.duration_ms e1.duration_ms) final_session.events in
  List.iter (fun event ->
    let percentage = if summary.total_event_time_ms > 0.0 then
      (event.duration_ms /. summary.total_event_time_ms) *. 100.0
    else 0.0 in
    let name_truncated = if String.length event.name > 23 then
      String.sub event.name 0 20 ^ "..."
    else event.name in
    print_endline (Printf.sprintf "│ %-23s │ %12s │ %11s │ %11.1f%% │"
      name_truncated
      (format_duration_ms event.duration_ms)
      (if event.memory_delta >= 0 then "+" ^ format_memory_bytes event.memory_delta 
       else format_memory_bytes event.memory_delta)
      percentage)
  ) sorted_events;
  
  print_endline "└─────────────────────────┴──────────────┴─────────────┴──────────────┘";
  print_endline "";
  
  if summary.overhead_ms > 10.0 then (
    print_endline (Printf.sprintf "WARNING: Profiling Overhead: %s (%.1f%% of total time)"
      (format_duration_ms summary.overhead_ms)
      ((summary.overhead_ms /. summary.total_duration_ms) *. 100.0))
  )

let export_profiling_json session filename =
  let (final_session, summary) = get_session_summary session in
  
  let event_to_json event =
    `Assoc [
      ("name", `String event.name);
      ("duration_ms", `Float event.duration_ms);
      ("memory_delta", `Int event.memory_delta);
      ("start_time", `Float event.start_time);
      ("end_time", `Float event.end_time);
      ("metadata", `Assoc (List.map (fun (k, v) -> (k, `String v)) event.metadata));
    ]
  in
  
  let json_data = `Assoc [
    ("session_id", `String final_session.session_id);
    ("summary", `Assoc [
      ("total_duration_ms", `Float summary.total_duration_ms);
      ("event_count", `Int summary.event_count);
      ("total_event_time_ms", `Float summary.total_event_time_ms);
      ("peak_memory_mb", `Float summary.peak_memory_mb);
      ("total_allocated_mb", `Float summary.total_allocated_mb);
    ]);
    ("events", `List (List.map event_to_json final_session.events));
  ] in
  
  try
    let oc = open_out filename in
    Yojson.Safe.pretty_to_channel oc json_data;
    close_out oc;
    Ok ()
  with
  | exn -> Error (circuit_error ("Failed to export profiling data: " ^ Printexc.to_string exn) ())

let profile_circuit_compilation circuit =
  let session = create_profiling_session ("compilation_" ^ circuit.Ast.name) in
  
  (* this would be integrated into the actual compilation process *)
  let session = start_event session "parse_circuit" [] in
  (* parsing happens here *)
  let session = end_event session "parse_circuit" in
  
  let session = start_event session "type_check" [] in
  (* type checking happens here *)
  let session = end_event session "type_check" in
  
  let session = start_event session "compile_to_r1cs" [] in
  (* R1CS compilation happens here *)
  let session = end_event session "compile_to_r1cs" in
  
  let session = start_event session "optimize" [] in
  (* optimization passes happen here *)
  let session = end_event session "optimize" in
  
  session

let analyze_hotspots session threshold_ms =
  let (final_session, _) = get_session_summary session in
  let hotspots = List.filter (fun event -> 
    event.duration_ms > threshold_ms
  ) final_session.events in
  
  List.sort (fun e1 e2 -> compare e2.duration_ms e1.duration_ms) hotspots

let memory_usage_timeline (session : profiling_session) =
  let completed_events = List.rev session.events in
  let events_by_time = List.sort (fun (e1 : profiling_event) (e2 : profiling_event) -> compare e1.start_time e2.start_time) completed_events in
  let timeline = List.map (fun (event : profiling_event) ->
    (event.start_time, event.memory_before, event.name ^ "_start")
  ) events_by_time in
  let timeline = timeline @ List.map (fun (event : profiling_event) ->
    (event.end_time, event.memory_after, event.name ^ "_end")
  ) events_by_time in
  List.sort (fun (t1, _, _) (t2, _, _) -> compare t1 t2) timeline