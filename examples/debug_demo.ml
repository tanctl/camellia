open Debug
open Ast

let demo_basic_debug () =
  Printf.printf "=== Basic Debug Context Demo ===\n";
  let ctx = create_context ~level:Info ~verbose:true "demo_circuit" in
  
  log ctx Info "Starting debug demonstration";
  log ctx Debug "This is a debug message";
  log ctx Warning "This is a warning";
  log ctx Error "This is an error message";
  
  Printf.printf "\nDebug context created for circuit: %s\n" ctx.circuit_name;
  Printf.printf "Verbose mode: %b\n" ctx.verbose;
  Printf.printf "\n"

let demo_constraint_and_wire_tracing () =
  Printf.printf "=== Constraint and Wire Tracing Demo ===\n";
  let ctx = create_context ~level:Trace ~verbose:true "tracing_demo" in
  
  trace_wire ctx ~id:1 ~name:"input_a" ~wire_type:"public" ~value:"42" ();
  trace_wire ctx ~id:2 ~name:"input_b" ~wire_type:"public" ~value:"17" ();
  trace_wire ctx ~id:3 ~name:"temp_sum" ~wire_type:"intermediate" ();
  trace_wire ctx ~id:4 ~name:"output" ~wire_type:"public" ();
  
  trace_constraint ctx ~id:1 ~left:"input_a" ~right:"input_b" ~output:"temp_sum" 
    ~description:"addition constraint";
  trace_constraint ctx ~id:2 ~left:"temp_sum" ~right:"constant_2" ~output:"output" 
    ~description:"multiplication constraint";
  
  let stats = get_stats ctx in
  Printf.printf "\nCurrent statistics:\n";
  Printf.printf "  Constraints: %d\n" stats.constraint_count;
  Printf.printf "  Wires: %d\n" stats.wire_count;
  Printf.printf "  Operations: %d\n" stats.total_operations;
  Printf.printf "\n"

let demo_phase_timing () =
  Printf.printf "=== Phase Timing Demo ===\n";
  let ctx = create_context ~level:Info ~verbose:true "timing_demo" in
  
  let simulate_parsing () =
    log ctx Debug "Tokenizing input...";
    Unix.sleepf 0.002;
    log ctx Debug "Building AST...";
    Unix.sleepf 0.003;
    "parsed successfully"
  in
  
  let simulate_typechecking () =
    log ctx Debug "Checking variable bindings...";
    Unix.sleepf 0.001;
    log ctx Debug "Validating expressions...";
    Unix.sleepf 0.002;
    Ok ()
  in
  
  let parse_result = measure_time ctx Parse simulate_parsing in
  Printf.printf "Parse result: %s\n" parse_result;
  
  let typecheck_result = measure_time_result ctx TypeCheck simulate_typechecking in
  Printf.printf "Typecheck result: %s\n" 
    (match typecheck_result with Ok _ -> "success" | Error _ -> "failed");
  
  let r1cs_result = measure_time ctx R1CSGen (fun () ->
    log ctx Debug "Generating R1CS constraints...";
    Unix.sleepf 0.005;
    trace_constraint ctx ~id:1 ~left:"a" ~right:"b" ~output:"c" ~description:"R1CS gen";
    trace_constraint ctx ~id:2 ~left:"c" ~right:"d" ~output:"e" ~description:"R1CS gen";
    42
  ) in
  Printf.printf "R1CS generated %d constraints\n" r1cs_result;
  
  let timings = get_timing_info ctx in
  Printf.printf "\nTiming summary:\n";
  List.iter (fun timing ->
    Printf.printf "  %s: %.3fms\n" 
      (phase_to_string timing.phase) (timing.duration *. 1000.0)
  ) timings;
  Printf.printf "\n"

let demo_ast_tracing () =
  Printf.printf "=== AST Tracing Demo ===\n";
  let ctx = create_context ~level:Debug ~verbose:true "ast_demo" in
  
  let x = var "x" in
  let y = var "y" in
  let sum = add x y in
  let product = mul sum (const "2") in
  let hash = poseidon [x; y; sum] in
  
  trace_ast_expr ctx x;
  trace_ast_expr ctx sum;
  trace_ast_expr ctx product;
  trace_ast_expr ctx hash;
  
  let assign_stmt = assign "result" product in
  let constraint_stmt = constraint_ (equal hash (const "expected")) in
  
  trace_ast_stmt ctx assign_stmt;
  trace_ast_stmt ctx constraint_stmt;
  
  let circuit = empty_circuit "example_circuit"
    |> add_input "x" 
    |> add_input "y"
    |> add_private_input "secret"
    |> add_stmt assign_stmt
    |> add_stmt constraint_stmt in
  
  trace_ast_circuit ctx circuit;
  
  let stats = get_stats ctx in
  Printf.printf "\nAST analysis results:\n";
  Printf.printf "  Public inputs: %d\n" stats.public_input_count;
  Printf.printf "  Private inputs: %d\n" stats.private_input_count;
  Printf.printf "\n"

let demo_statistics_and_export () =
  Printf.printf "=== Statistics and Export Demo ===\n";
  let ctx = create_context ~level:Info ~verbose:true "export_demo" in
  
  start_phase ctx Parse;
  trace_wire ctx ~id:1 ~name:"a" ~wire_type:"input" ();
  trace_wire ctx ~id:2 ~name:"b" ~wire_type:"input" ();
  Unix.sleepf 0.001;
  end_phase ctx;
  
  start_phase ctx R1CSGen;
  trace_constraint ctx ~id:1 ~left:"a" ~right:"b" ~output:"c" ~description:"multiplication";
  trace_constraint ctx ~id:2 ~left:"c" ~right:"1" ~output:"d" ~description:"addition";
  trace_wire ctx ~id:3 ~name:"c" ~wire_type:"intermediate" ();
  trace_wire ctx ~id:4 ~name:"d" ~wire_type:"output" ();
  Unix.sleepf 0.002;
  end_phase ctx;
  
  update_stats ctx ~public_inputs:2 ~private_inputs:1 ~intermediates:2;
  add_operation ctx;
  add_operation ctx;
  
  print_statistics ctx;
  
  let export_file = "debug_export_demo.txt" in
  export_debug_info ctx export_file;
  Printf.printf "\nDebug info exported to %s\n" export_file;
  
  if Sys.file_exists export_file then begin
    Printf.printf "Export file contents preview:\n";
    let ic = open_in export_file in
    let rec print_lines count =
      if count > 0 then
        try
          let line = input_line ic in
          Printf.printf "  %s\n" line;
          print_lines (count - 1)
        with End_of_file -> ()
    in
    print_lines 10;
    close_in ic;
    Sys.remove export_file
  end;
  Printf.printf "\n"

let demo_with_debug_context () =
  Printf.printf "=== With Debug Context Demo ===\n";
  
  let result = with_debug_context ~level:Info ~verbose:false "wrapper_demo" (fun ctx ->
    log ctx Info "Inside debug context wrapper";
    start_phase ctx TypeCheck;
    trace_constraint ctx ~id:1 ~left:"x" ~right:"y" ~output:"z" ~description:"test constraint";
    Unix.sleepf 0.001;
    end_phase ctx;
    
    add_operation ctx;
    update_stats ctx ~public_inputs:1 ~private_inputs:1 ~intermediates:1;
    
    "computation completed successfully"
  ) in
  
  Printf.printf "Wrapper result: %s\n" result;
  Printf.printf "\n"

let () =
  demo_basic_debug ();
  demo_constraint_and_wire_tracing ();
  demo_phase_timing ();
  demo_ast_tracing ();
  demo_statistics_and_export ();
  demo_with_debug_context ();
  Printf.printf "Debug infrastructure demonstration completed!\n"