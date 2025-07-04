type output_format = R1CS | Circom | Bellman

let export_circuit format _compiled_circuit =
  match format with
  | R1CS -> "/* R1CS output */"
  | Circom -> "/* Circom output */"
  | Bellman -> "/* Bellman output */"