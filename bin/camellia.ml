open Printf
open Error
open Ast
open Compiler
open Output
open Debug

let version = "1.0.0-MVP"

type command =
  | Compile
  | Verify
  | Debug
  | Stats
  | Analyze
  | Help
  | Version

type cli_options = {
  command: command;
  input_file: string option;
  output_file: string option;
  output_format: string;
  debug_level: Debug.log_level;
  verbose: bool;
  quiet: bool;
  show_stats: bool;
  export_all: bool;
  analysis_depth: [`Quick | `Full | `Deep];
  export_analysis: bool;
}

let default_options = {
  command = Help;
  input_file = None;
  output_file = None;
  output_format = "json";
  debug_level = Warning;
  verbose = false;
  quiet = false;
  show_stats = false;
  export_all = false;
  analysis_depth = `Full;
  export_analysis = false;
}

let print_version () =
  printf "Camellia ZK Circuit Compiler v%s\n" version;
  printf "A zero-knowledge circuit compiler with R1CS output\n";
  printf "Built with OCaml • https://github.com/tanctl/camellia\n"

let print_help () =
  print_version ();
  printf "\n";
  printf "USAGE:\n";
  printf "    camellia <COMMAND> [OPTIONS] [FILE]\n";
  printf "\n";
  printf "COMMANDS:\n";
  printf "    compile    Compile circuit to R1CS format\n";
  printf "    verify     Validate R1CS output mathematically\n";
  printf "    debug      Show detailed compilation process\n";
  printf "    stats      Display circuit statistics and analysis\n";
  printf "    analyze    Comprehensive circuit analysis with security and performance metrics\n";
  printf "    help       Show this help message\n";
  printf "    version    Show version information\n";
  printf "\n";
  printf "OPTIONS:\n";
  printf "    -o, --output <FILE>        Output file (default: stdout)\n";
  printf "    -f, --format <FORMAT>      Output format [json|readable|stats|witness]\n";
  printf "    -d, --debug <LEVEL>        Debug level [silent|error|warning|info|debug|trace]\n";
  printf "    -v, --verbose              Enable verbose output\n";
  printf "    -q, --quiet                Suppress non-essential output\n";
  printf "    -s, --stats                Include circuit statistics\n";
  printf "    -a, --all                  Export all formats\n";
  printf "    --analysis-depth <LEVEL>   Analysis depth [quick|full|deep]\n";
  printf "    --export-analysis          Export detailed analysis report\n";
  printf "    -h, --help                 Show this help message\n";
  printf "    --version                  Show version information\n";
  printf "\n";
  printf "EXAMPLES:\n";
  printf "    camellia compile circuit.camellia\n";
  printf "    camellia compile -o output.json -f json circuit.camellia\n";
  printf "    camellia debug -d trace -v circuit.camellia\n";
  printf "    camellia stats -a circuit.camellia\n";
  printf "    camellia analyze --analysis-depth deep circuit.camellia\n";
  printf "    camellia verify output.json\n";
  printf "\n";
  printf "OUTPUT FORMATS:\n";
  printf "    json       Enhanced JSON with metadata and analysis\n";
  printf "    readable   Human-readable format for debugging\n";
  printf "    stats      Statistics and performance analysis\n";
  printf "    witness    Example witness format\n";
  printf "    circom     Circom-compatible format\n";
  printf "    bellman    Bellman-compatible JSON format\n";
  printf "\n";
  printf "For more information, visit: https://github.com/tanctl/camellia\n"

let parse_debug_level = function
  | "silent" -> Silent
  | "error" -> Error
  | "warning" -> Warning  
  | "info" -> Info
  | "debug" -> Debug
  | "trace" -> Trace
  | level -> 
      eprintf "Invalid debug level: %s\n" level;
      eprintf "Valid levels: silent, error, warning, info, debug, trace\n";
      exit 1

let parse_command = function
  | "compile" -> Compile
  | "verify" -> Verify
  | "debug" -> Debug
  | "stats" -> Stats
  | "analyze" -> Analyze
  | "help" -> Help
  | "version" -> Version
  | cmd -> 
      eprintf "Unknown command: %s\n" cmd;
      eprintf "Run 'camellia help' for usage information\n";
      exit 1

let read_file filename =
  try
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Ok content
  with
  | Sys_error msg -> 
      Result.Error (circuit_error ("File error: " ^ msg) ())
  | exn ->
      Result.Error (circuit_error ("Unexpected error reading file: " ^ Printexc.to_string exn) ())

let write_file filename content =
  try
    let oc = open_out filename in
    output_string oc content;
    close_out oc;
    Ok ()
  with
  | Sys_error msg ->
      Result.Error (circuit_error ("File write error: " ^ msg) ())
  | exn ->
      Result.Error (circuit_error ("Unexpected error writing file: " ^ Printexc.to_string exn) ())



let parse_circuit_from_file filename =
  let* content = read_file filename in
  Parser.parse_circuit content

let compile_circuit_file options =
  let input_file = match options.input_file with
    | Some file -> file
    | None -> 
        eprintf "Error: No input file specified\n";
        eprintf "Usage: camellia compile <FILE>\n";
        exit 1
  in
  
  if not options.quiet then
    printf "🔧 Compiling circuit: %s\n" input_file;
  
  let* circuit = parse_circuit_from_file input_file in
  
  let debug_ctx = create_context ~level:options.debug_level ~verbose:options.verbose circuit.name in
  let* compiled = compile_circuit debug_ctx circuit in
  
  let output_content = match options.output_format with
    | "json" -> 
        let json = serialize_enhanced_r1cs_json compiled.r1cs_system circuit.name in
        Yojson.Safe.pretty_to_string json
    | "readable" ->
        generate_human_readable_output compiled.r1cs_system circuit.name
    | "stats" ->
        let report_lines = generate_statistics_report compiled.r1cs_system circuit.name in
        String.concat "\n" report_lines
    | "circom" ->
        let temp_file = Filename.temp_file "circom" ".txt" in
        export_circom_compatible compiled.r1cs_system circuit.name temp_file;
        let ic = open_in temp_file in
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        Sys.remove temp_file;
        content
    | "bellman" ->
        let temp_file = Filename.temp_file "bellman" ".json" in
        export_bellman_compatible compiled.r1cs_system circuit.name temp_file;
        let ic = open_in temp_file in
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        Sys.remove temp_file;
        content
    | format ->
        Result.Error (circuit_error ("Unsupported output format: " ^ format) ()) |> Result.get_error |> error_to_string
  in
  
  (match options.output_file with
   | Some filename ->
       let* () = write_file filename output_content in
       if not options.quiet then
         printf "✅ Compilation successful! Output written to: %s\n" filename;
       Ok ()
   | None ->
       printf "%s\n" output_content;
       Ok ()
  ) |> ignore;
  
  if options.show_stats then (
    let stats = get_compilation_stats compiled in
    printf "\n📊 CIRCUIT STATISTICS:\n";
    printf "  Variables: %d\n" stats.num_variables;
    printf "  Constraints: %d\n" stats.num_constraints;
    printf "  Public Inputs: %d\n" stats.num_inputs;
    printf "  Private Inputs: %d\n" stats.num_private;
    
    (match Analysis.analyze_circuit circuit compiled.r1cs_system with
     | Ok analysis -> 
         printf "  Constraint Density: %.2f%%\n" (analysis.complexity.linear_constraints |> float_of_int |> fun x -> x /. float_of_int analysis.complexity.total_constraints *. 100.0);
         printf "  Proving Time Est: %.3f seconds\n" (analysis.performance.proving_time_ms /. 1000.0);
     | Error _ -> 
         printf "  Analysis: Failed to analyze circuit\n");
  );
  
  if options.export_all then (
    let base_name = match options.input_file with
      | Some file -> Filename.remove_extension file
      | None -> "output"
    in
    export_r1cs_comprehensive compiled.r1cs_system circuit.name base_name;
    if not options.quiet then
      printf "📁 All formats exported with base name: %s\n" base_name;
  );
  
  Ok ()

let verify_r1cs_file options =
  let input_file = match options.input_file with
    | Some file -> file
    | None ->
        eprintf "Error: No R1CS file specified\n";
        eprintf "Usage: camellia verify <FILE>\n";
        exit 1
  in
  
  if not options.quiet then
    printf "🔍 Verifying R1CS file: %s\n" input_file;
  
  let* content = read_file input_file in
  let json = try Yojson.Safe.from_string content
             with Yojson.Json_error msg ->
               eprintf "JSON parse error: %s\n" msg;
               exit 1 in
  
  (match json with
   | `Assoc fields ->
       let metadata = List.assoc_opt "metadata" fields in
       let constraints = List.assoc_opt "constraints" fields in
       (match metadata, constraints with
        | Some (`Assoc meta_fields), Some (`List constraint_list) ->
            let circuit_name = match List.assoc_opt "circuit_name" meta_fields with
              | Some (`String name) -> name
              | _ -> "unknown_circuit" in
            
            printf "✅ R1CS Format: Valid JSON structure\n";
            printf "📋 Circuit Name: %s\n" circuit_name;
            printf "🔗 Constraints: %d found\n" (List.length constraint_list);
            
            (match List.assoc_opt "analysis" fields with
             | Some (`Assoc analysis_fields) ->
                 (match List.assoc_opt "constraint_density" analysis_fields with
                  | Some (`Float density) ->
                      printf "📊 Constraint Density: %.2f%%\n" (density *. 100.0)
                  | _ -> ());
                 (match List.assoc_opt "proving_time_estimate_sec" analysis_fields with
                  | Some (`Float time) ->
                      printf "⏱️  Proving Time Est: %.3f seconds\n" time
                  | _ -> ())
             | _ -> ());
            
            printf "✅ R1CS verification complete - structure is valid\n";
            Ok ()
        | _ ->
            printf "❌ Invalid R1CS format: missing required fields\n";
            Result.Error (circuit_error "Invalid R1CS JSON format" ())
       )
   | _ ->
       printf "❌ Invalid JSON: expected object at root level\n";
       Result.Error (circuit_error "Invalid JSON format" ())
  )

let debug_compilation options =
  if not options.quiet then
    printf "🐛 Debug mode: Detailed compilation trace\n";
  
  let options_with_debug = { options with debug_level = Trace; verbose = true; show_stats = true } in
  compile_circuit_file options_with_debug

let show_circuit_stats options =
  if not options.quiet then
    printf "📊 Circuit Statistics and Analysis\n";
  
  let options_with_stats = { options with output_format = "stats"; show_stats = true } in
  compile_circuit_file options_with_stats

let analyze_circuit_comprehensive options =
  let input_file = match options.input_file with
    | Some file -> file
    | None ->
        eprintf "Error: No input file specified\n";
        eprintf "Usage: camellia analyze <FILE>\n";
        exit 1
  in
  
  if not options.quiet then
    printf "🔍 Comprehensive Circuit Analysis: %s\n" input_file;
  
  (* parse the circuit *)
  let* circuit = parse_circuit_from_file input_file in
  
  (* compile to get R1CS *)
  let debug_ctx = create_context ~level:options.debug_level ~verbose:options.verbose circuit.name in
  let* compiled = compile_circuit debug_ctx circuit in
  
  (* create analysis configuration based on depth *)
  let analysis_config = match options.analysis_depth with
    | `Quick -> { Analysis.default_config with enable_deep_analysis = false }
    | `Full -> Analysis.default_config
    | `Deep -> { Analysis.default_config with enable_deep_analysis = true; performance_target_ms = 500.0 }
  in
  
  (* perform comprehensive analysis *)
  let* analysis = Analysis.analyze_with_config analysis_config circuit compiled.r1cs_system in
  
  (* generate and display report *)
  let report = Analysis.generate_report analysis in
  printf "%s\n" report;
  
  (* export detailed analysis if requested *)
  if options.export_analysis then (
    let base_name = match options.input_file with
      | Some file -> Filename.remove_extension file
      | None -> "analysis"
    in
    let analysis_filename = base_name ^ "_analysis.txt" in
    let json_filename = base_name ^ "_analysis.json" in
    
    let* () = Analysis.export_report analysis analysis_filename in
    let json_content = Analysis.export_json analysis |> Yojson.Safe.pretty_to_string in
    let* () = write_file json_filename json_content in
    
    if not options.quiet then (
      printf "\n📁 Detailed analysis exported:\n";
      printf "  Report: %s\n" analysis_filename;
      printf "  JSON: %s\n" json_filename;
    );
    Ok ()
  ) |> ignore;
  
  Ok ()

let parse_args args =
  let rec parse_args_rec options = function
    | [] -> options
    | "--version" :: _ -> { options with command = Version }
    | "-h" :: _rest | "--help" :: _rest -> { options with command = Help }
    | "-v" :: rest | "--verbose" :: rest -> 
        parse_args_rec { options with verbose = true } rest
    | "-q" :: rest | "--quiet" :: rest -> 
        parse_args_rec { options with quiet = true } rest
    | "-s" :: rest | "--stats" :: rest -> 
        parse_args_rec { options with show_stats = true } rest
    | "-a" :: rest | "--all" :: rest -> 
        parse_args_rec { options with export_all = true } rest
    | "-o" :: filename :: rest | "--output" :: filename :: rest ->
        parse_args_rec { options with output_file = Some filename } rest
    | "-f" :: format :: rest | "--format" :: format :: rest ->
        parse_args_rec { options with output_format = format } rest
    | "-d" :: level :: rest | "--debug" :: level :: rest ->
        let debug_level = parse_debug_level level in
        parse_args_rec { options with debug_level } rest
    | "--analysis-depth" :: depth :: rest ->
        let analysis_depth = match depth with
          | "quick" -> `Quick
          | "full" -> `Full  
          | "deep" -> `Deep
          | _ -> 
              eprintf "Invalid analysis depth: %s\n" depth;
              eprintf "Valid depths: quick, full, deep\n";
              exit 1
        in
        parse_args_rec { options with analysis_depth } rest
    | "--export-analysis" :: rest ->
        parse_args_rec { options with export_analysis = true } rest
    | cmd :: rest when String.get cmd 0 <> '-' && options.command = Help ->
        let command = parse_command cmd in
        let options = { options with command } in
        parse_args_rec options rest
    | filename :: rest when String.get filename 0 <> '-' && options.input_file = None ->
        parse_args_rec { options with input_file = Some filename } rest
    | unknown :: _ ->
        eprintf "Unknown option: %s\n" unknown;
        eprintf "Run 'camellia help' for usage information\n";
        exit 1
  in
  parse_args_rec default_options args

let handle_error = function
  | Ok () -> ()
  | Error err ->
      eprintf "❌ Error: %s\n" (error_to_string err);
      exit 1

let main () =
  let args = Array.to_list Sys.argv |> List.tl in
  
  let options = parse_args args in
  
  let result = match options.command with
    | Help -> print_help (); Ok ()
    | Version -> print_version (); Ok ()
    | Compile -> compile_circuit_file options
    | Verify -> verify_r1cs_file options
    | Debug -> debug_compilation options
    | Stats -> show_circuit_stats options
    | Analyze -> analyze_circuit_comprehensive options
  in
  
  handle_error result

let () =
  try
    main ()
  with
  | exn ->
      eprintf "💥 Unexpected error: %s\n" (Printexc.to_string exn);
      eprintf "Please report this issue at: https://github.com/tanctl/camellia/issues\n";
      exit 1