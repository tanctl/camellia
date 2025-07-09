
let usage_msg = "camellia [OPTIONS] <file>"

let input_file = ref ""
let output_format = ref "r1cs"
let verbose = ref false

let set_input_file filename = input_file := filename

let spec_list = [
  ("-o", Arg.Set_string output_format, " Set output format (r1cs|circom|bellman)");
  ("-v", Arg.Set verbose, " Enable verbose output");
]

let compile_file filename =
  if !verbose then Printf.printf "Compiling %s...\n" filename;
  let _pos = Error.make_pos filename 1 1 in
  match Parser.parse_circuit "valid" with
  | Ok circuit ->
      let debug_level = if !verbose then Debug.Debug else Debug.Warning in
      let debug_ctx = Debug.create_context ~level:debug_level filename in
      (match Compiler.compile_circuit debug_ctx circuit with
      | Ok compiled ->
          let format = match !output_format with
            | "r1cs" -> Backend.R1CS
            | "circom" -> Backend.Circom
            | "bellman" -> Backend.Bellman
            | _ -> failwith "Invalid output format"
          in
          let output = Backend.export_circuit format compiled in
          print_endline output
      | Error err ->
          Printf.eprintf "Compilation Error: %s\n" (Error.error_to_string err);
          exit 1)
  | Error err ->
      Printf.eprintf "Parse Error: %s\n" (Error.error_to_string err);
      exit 1

let () =
  Arg.parse spec_list set_input_file usage_msg;
  if !input_file = "" then begin
    Printf.printf "Camellia ZK Circuit DSL v0.1.0\n";
    Printf.printf "Usage: %s\n" usage_msg;
    Arg.usage spec_list usage_msg
  end else
    compile_file !input_file