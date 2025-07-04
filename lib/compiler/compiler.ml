type compiled_circuit = {
  gates: (string * string * string) list; (* (output, input1, input2) *)
  constraints: string list;
}

let compile_circuit _circuit =
  { gates = []; constraints = [] }