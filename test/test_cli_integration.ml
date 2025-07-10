open Alcotest

let contains_substring s sub =
  try ignore (Str.search_forward (Str.regexp_string sub) s 0); true 
  with Not_found -> false

let test_cli_help () =
  let exit_code = Sys.command "dune exec bin/camellia.exe -- help > /dev/null 2>&1" in
  check int "help command exits successfully" 0 exit_code

let test_cli_version () =
  let exit_code = Sys.command "dune exec bin/camellia.exe -- version > /dev/null 2>&1" in
  check int "version command exits successfully" 0 exit_code

let test_compile_hash_preimage () =
  let exit_code = Sys.command "dune exec bin/camellia.exe -- compile examples/hash_preimage.camellia > /tmp/test_output.json 2>/dev/null" in
  check int "hash preimage compilation succeeds" 0 exit_code;
  
  let ic = open_in "/tmp/test_output.json" in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  
  check bool "output contains constraints" true (contains_substring content "constraints");
  check bool "output contains metadata" true (contains_substring content "metadata");
  
  Sys.remove "/tmp/test_output.json"

let test_compile_arithmetic () =
  let exit_code = Sys.command "dune exec bin/camellia.exe -- compile -q examples/test_arithmetic.camellia > /tmp/test_arithmetic.json 2>/dev/null" in
  check int "arithmetic circuit compilation succeeds" 0 exit_code;
  
  let ic = open_in "/tmp/test_arithmetic.json" in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  
  let json = Yojson.Safe.from_string content in
  (match json with
   | `Assoc fields ->
       let constraints = List.assoc "constraints" fields in
       (match constraints with
        | `List constraint_list -> 
            check bool "has constraints" true (List.length constraint_list > 0)
        | _ -> check bool "constraints format" false true)
   | _ -> check bool "json format" false true);
  
  Sys.remove "/tmp/test_arithmetic.json"

let test_readable_output () =
  let exit_code = Sys.command "dune exec bin/camellia.exe -- compile -f readable examples/hash_preimage.camellia > /tmp/test_readable.txt 2>/dev/null" in
  check int "readable output succeeds" 0 exit_code;
  
  let ic = open_in "/tmp/test_readable.txt" in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  
  check bool "contains camellia header" true (contains_substring content "CAMELLIA");
  check bool "contains constraint system" true (contains_substring content "CONSTRAINT");
  check bool "reasonable length" true (String.length content > 500);
  
  Sys.remove "/tmp/test_readable.txt"

let test_stats_output () =
  let exit_code = Sys.command "dune exec bin/camellia.exe -- stats examples/test_arithmetic.camellia > /tmp/test_stats.txt 2>/dev/null" in
  check int "stats output succeeds" 0 exit_code;
  
  let ic = open_in "/tmp/test_stats.txt" in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  
  check bool "contains statistics" true (contains_substring content "STATISTICS");
  check bool "contains metrics" true (contains_substring content "METRICS");
  
  Sys.remove "/tmp/test_stats.txt"

let test_debug_output () =
  let exit_code = Sys.command "dune exec bin/camellia.exe -- debug examples/hash_preimage.camellia > /tmp/test_debug.txt 2>&1" in
  check int "debug output succeeds" 0 exit_code;
  
  let ic = open_in "/tmp/test_debug.txt" in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  
  check bool "contains trace info" true (contains_substring content "TRACE");
  check bool "contains compilation steps" true (contains_substring content "Compiling");
  check bool "detailed output" true (String.length content > 1000);
  
  Sys.remove "/tmp/test_debug.txt"

let test_verify_command () =
  let exit_code1 = Sys.command "dune exec bin/camellia.exe -- compile -o /tmp/verify_test.json examples/hash_preimage.camellia 2>/dev/null" in
  check int "create json for verification" 0 exit_code1;
  
  let exit_code2 = Sys.command "dune exec bin/camellia.exe -- verify /tmp/verify_test.json > /tmp/verify_output.txt 2>/dev/null" in
  check int "verification succeeds" 0 exit_code2;
  
  let ic = open_in "/tmp/verify_output.txt" in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  
  check bool "verification output present" true (String.length content > 50);
  
  Sys.remove "/tmp/verify_test.json";
  Sys.remove "/tmp/verify_output.txt"

let test_output_file_option () =
  let exit_code = Sys.command "dune exec bin/camellia.exe -- compile -o /tmp/custom_output.json examples/hash_preimage.camellia 2>/dev/null" in
  check int "custom output file succeeds" 0 exit_code;
  
  check bool "output file created" true (Sys.file_exists "/tmp/custom_output.json");
  
  let ic = open_in "/tmp/custom_output.json" in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  
  check bool "output file has content" true (String.length content > 100);
  
  Sys.remove "/tmp/custom_output.json"

let test_verbose_flag () =
  let exit_code = Sys.command "dune exec bin/camellia.exe -- compile -v examples/hash_preimage.camellia > /tmp/verbose_output.txt 2>&1" in
  check int "verbose compilation succeeds" 0 exit_code;
  
  let ic = open_in "/tmp/verbose_output.txt" in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  
  check bool "verbose output present" true (contains_substring content "Compiling");
  
  Sys.remove "/tmp/verbose_output.txt"

let test_error_handling () =
  let exit_code = Sys.command "dune exec bin/camellia.exe -- compile nonexistent.camellia > /dev/null 2>&1" in
  check bool "non-existent file fails" true (exit_code <> 0);
  
  let exit_code2 = Sys.command "dune exec bin/camellia.exe -- invalid_command > /dev/null 2>&1" in
  check bool "invalid command fails" true (exit_code2 <> 0)

let test_all_formats () =
  let formats = ["json"; "readable"; "stats"] in
  List.iter (fun format ->
    let cmd = Printf.sprintf "dune exec bin/camellia.exe -- compile -f %s examples/hash_preimage.camellia > /tmp/test_%s_format.out 2>/dev/null" format format in
    let exit_code = Sys.command cmd in
    check int (Printf.sprintf "%s format succeeds" format) 0 exit_code;
    
    let filename = Printf.sprintf "/tmp/test_%s_format.out" format in
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    
    check bool (Printf.sprintf "%s format has content" format) true (String.length content > 50);
    
    Sys.remove filename
  ) formats

let integration_tests = [
  "cli_help", `Quick, test_cli_help;
  "cli_version", `Quick, test_cli_version;
  "compile_hash_preimage", `Quick, test_compile_hash_preimage;
  "compile_arithmetic", `Quick, test_compile_arithmetic;
  "readable_output", `Quick, test_readable_output;
  "stats_output", `Quick, test_stats_output;
  "debug_output", `Quick, test_debug_output;
  "verify_command", `Quick, test_verify_command;
  "output_file_option", `Quick, test_output_file_option;
  "verbose_flag", `Quick, test_verbose_flag;
  "error_handling", `Quick, test_error_handling;
  "all_formats", `Quick, test_all_formats;
]

let () = run "CLI Integration Tests" [
  "integration", integration_tests;
]