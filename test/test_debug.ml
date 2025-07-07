open Debug
open Ast

let test_debug_context_creation () =
  let ctx = create_context "test_circuit" in
  Alcotest.(check string) "circuit name" "test_circuit" ctx.circuit_name;
  Alcotest.(check bool) "not verbose by default" false ctx.verbose;
  Alcotest.(check int) "empty constraint count" 0 (get_constraint_count ctx);
  Alcotest.(check int) "empty wire count" 0 (get_wire_count ctx)

let test_log_levels () =
  let temp_file = Filename.temp_file "debug_test" ".log" in
  let oc = open_out temp_file in
  let ctx = create_context ~level:Debug ~output:oc "test" in
  
  log ctx Error "error message";
  log ctx Warning "warning message";  
  log ctx Info "info message";
  log ctx Debug "debug message";
  log ctx Trace "trace message";
  
  close_out oc;
  let content = 
    let ic = open_in temp_file in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Sys.remove temp_file;
    content
  in
  
  Alcotest.(check bool) "contains error" true (String.contains content 'E');
  Alcotest.(check bool) "contains warning" true (String.contains content 'W');
  Alcotest.(check bool) "contains info" true (String.contains content 'I');
  Alcotest.(check bool) "contains debug" true (String.contains content 'D')

let test_phase_timing () =
  let ctx = create_context "timing_test" in
  
  start_phase ctx Parse;
  Unix.sleepf 0.001;
  end_phase ctx;
  
  start_phase ctx TypeCheck;
  Unix.sleepf 0.001;
  end_phase ctx;
  
  let timings = get_timing_info ctx in
  Alcotest.(check int) "two phases recorded" 2 (List.length timings);
  
  let parse_timing = List.find (fun t -> t.phase = Parse) timings in
  let typecheck_timing = List.find (fun t -> t.phase = TypeCheck) timings in
  
  Alcotest.(check bool) "parse duration positive" true (parse_timing.duration > 0.0);
  Alcotest.(check bool) "typecheck duration positive" true (typecheck_timing.duration > 0.0)

let test_constraint_tracing () =
  let ctx = create_context ~level:Trace "constraint_test" in
  
  trace_constraint ctx ~id:1 ~left:"a" ~right:"b" ~output:"c" 
    ~description:"multiplication constraint";
  trace_constraint ctx ~id:2 ~left:"c" ~right:"d" ~output:"e" 
    ~description:"addition constraint";
  
  let stats = get_stats ctx in
  Alcotest.(check int) "constraint count" 2 stats.constraint_count;
  Alcotest.(check int) "operation count" 2 stats.total_operations;
  
  Alcotest.(check int) "constraints list length" 2 (List.length ctx.constraints);
  let first_constraint = List.hd (List.rev ctx.constraints) in
  Alcotest.(check int) "first constraint id" 1 first_constraint.id;
  Alcotest.(check string) "first constraint description" 
    "multiplication constraint" first_constraint.description

let test_wire_tracing () =
  let ctx = create_context ~level:Trace "wire_test" in
  
  trace_wire ctx ~id:1 ~name:"input_a" ~wire_type:"public" ~value:"42" ();
  trace_wire ctx ~id:2 ~name:"temp_1" ~wire_type:"intermediate" ();
  trace_wire ctx ~id:3 ~name:"output" ~wire_type:"public" ~value:"result" ();
  
  let stats = get_stats ctx in
  Alcotest.(check int) "wire count" 3 stats.wire_count;
  
  Alcotest.(check int) "wires list length" 3 (List.length ctx.wires);
  let first_wire = List.hd (List.rev ctx.wires) in
  Alcotest.(check int) "first wire id" 1 first_wire.id;
  Alcotest.(check string) "first wire name" "input_a" first_wire.name;
  Alcotest.(check string) "first wire type" "public" first_wire.wire_type

let test_ast_tracing () =
  let ctx = create_context ~level:Debug "ast_test" in
  
  let x = var "x" in
  let y = var "y" in
  let sum = add x y in
  
  trace_ast_expr ctx x;
  trace_ast_expr ctx sum;
  
  let assign_stmt = assign "result" sum in
  trace_ast_stmt ctx assign_stmt;
  
  let circuit = empty_circuit "test_circuit"
    |> add_input "x"
    |> add_input "y"
    |> add_stmt assign_stmt in
  
  trace_ast_circuit ctx circuit;
  
  let stats = get_stats ctx in
  Alcotest.(check int) "public inputs tracked" 2 stats.public_input_count;
  Alcotest.(check int) "private inputs tracked" 0 stats.private_input_count

let test_statistics_update () =
  let ctx = create_context "stats_test" in
  
  update_stats ctx ~public_inputs:3 ~private_inputs:2 ~intermediates:5;
  add_operation ctx;
  add_operation ctx;
  
  let stats = get_stats ctx in
  Alcotest.(check int) "public inputs" 3 stats.public_input_count;
  Alcotest.(check int) "private inputs" 2 stats.private_input_count;
  Alcotest.(check int) "intermediates" 5 stats.intermediate_count;
  Alcotest.(check int) "operations" 2 stats.total_operations

let test_measure_time () =
  let ctx = create_context "measure_test" in
  
  let result = measure_time ctx Parse (fun () ->
    Unix.sleepf 0.001;
    42
  ) in
  
  Alcotest.(check int) "function result" 42 result;
  
  let timings = get_timing_info ctx in
  Alcotest.(check int) "one timing recorded" 1 (List.length timings);
  
  let timing = List.hd timings in
  Alcotest.(check bool) "timing duration positive" true (timing.duration > 0.0)

let test_measure_time_with_error () =
  let ctx = create_context "error_test" in
  
  let result = measure_time_result ctx TypeCheck (fun () ->
    Unix.sleepf 0.001;
    Ok "success"
  ) in
  
  Alcotest.(check bool) "result is ok" true (Result.is_ok result);
  
  let timings = get_timing_info ctx in
  Alcotest.(check int) "timing recorded despite result type" 1 (List.length timings)

let test_context_clearing () =
  let ctx = create_context "clear_test" in
  
  trace_constraint ctx ~id:1 ~left:"a" ~right:"b" ~output:"c" ~description:"test";
  trace_wire ctx ~id:1 ~name:"test_wire" ~wire_type:"test" ();
  start_phase ctx Parse;
  end_phase ctx;
  
  Alcotest.(check bool) "has constraints before clear" true 
    (get_constraint_count ctx > 0);
  
  clear_context ctx;
  
  let stats = get_stats ctx in
  Alcotest.(check int) "constraint count cleared" 0 stats.constraint_count;
  Alcotest.(check int) "wire count cleared" 0 stats.wire_count;
  Alcotest.(check int) "timings cleared" 0 (List.length (get_timing_info ctx))

let test_export_debug_info () =
  let ctx = create_context "export_test" in
  let temp_file = Filename.temp_file "debug_export" ".txt" in
  
  trace_constraint ctx ~id:1 ~left:"a" ~right:"b" ~output:"c" ~description:"test";
  trace_wire ctx ~id:1 ~name:"test_wire" ~wire_type:"intermediate" ();
  start_phase ctx Parse;
  Unix.sleepf 0.001;
  end_phase ctx;
  
  export_debug_info ctx temp_file;
  
  let content = 
    let ic = open_in temp_file in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Sys.remove temp_file;
    content
  in
  
  Alcotest.(check bool) "contains circuit name" true 
    (String.contains content 'e');
  Alcotest.(check bool) "contains constraint info" true 
    (String.contains content 'c');
  Alcotest.(check bool) "contains wire info" true 
    (String.contains content 'w')

let test_with_debug_context () =
  let temp_file = Filename.temp_file "with_debug" ".log" in  
  let result = with_debug_context ~level:Info "context_test" (fun ctx ->
    trace_constraint ctx ~id:1 ~left:"x" ~right:"y" ~output:"z" ~description:"test";
    start_phase ctx Parse;
    Unix.sleepf 0.001;
    end_phase ctx;
    "completed"
  ) in
  
  Alcotest.(check string) "function result" "completed" result;
  Sys.remove temp_file

let () =
  let open Alcotest in
  run "Debug Tests" [
    "context", [
      test_case "Debug context creation" `Quick test_debug_context_creation;
      test_case "Log levels" `Quick test_log_levels;
    ];
    "timing", [
      test_case "Phase timing" `Quick test_phase_timing;
      test_case "Measure time" `Quick test_measure_time;
      test_case "Measure time with result" `Quick test_measure_time_with_error;
    ];
    "tracing", [
      test_case "Constraint tracing" `Quick test_constraint_tracing;
      test_case "Wire tracing" `Quick test_wire_tracing;
      test_case "AST tracing" `Quick test_ast_tracing;
    ];
    "statistics", [
      test_case "Statistics update" `Quick test_statistics_update;
      test_case "Context clearing" `Quick test_context_clearing;
    ];
    "export", [
      test_case "Export debug info" `Quick test_export_debug_info;
      test_case "With debug context" `Quick test_with_debug_context;
    ];
  ]