# Camellia ZK Circuit Examples

This directory contains comprehensive examples of Camellia ZK circuits demonstrating various language features and use cases.

## Basic Examples

### `minimal.cam`
The simplest possible circuit with a single input and constraint:
```bash
camellia compile minimal.cam
```

### `hash_preimage.cam` 
Classic hash preimage proof - prove knowledge of a secret that hashes to a known value:
```bash
camellia compile hash_preimage.cam -f readable
camellia debug hash_preimage.cam
```

### `arithmetic_proof.cam`
Prove knowledge of factors of a number without revealing the factors:
```bash
camellia compile arithmetic_proof.cam
```

## Syntax Examples

### `syntax_showcase.cam`
Comprehensive demonstration of all Camellia language features:
- Public and private inputs
- Arithmetic operations
- Field constants  
- Poseidon hash function
- Multiple constraints

### `working_demo.cam`
A more complex circuit showing practical ZK proof patterns:
```bash
camellia compile working_demo.cam -f readable
camellia stats working_demo.cam
```

## Error Examples

### `error_demo.cam`
Demonstrates error handling with undefined variables:
```bash
camellia compile error_demo.cam  # Will show error message
```

## CLI Usage Examples

### Compilation
```bash
# Basic compilation to JSON
camellia compile circuit.cam

# Compile to different formats
camellia compile circuit.cam -f readable
camellia compile circuit.cam -f circom
camellia compile circuit.cam -f stats

# Save to file
camellia compile circuit.cam -o output.json
```

### Analysis and Debugging
```bash
# Detailed debug trace
camellia debug circuit.cam

# Circuit statistics
camellia stats circuit.cam

# All output formats
camellia compile circuit.cam -a
```

### Verification
```bash
# Verify R1CS output
camellia verify output.json
```

## Output Formats

- **JSON**: Machine-readable R1CS with complete metadata
- **Readable**: Human-readable constraint listing for analysis
- **Stats**: Performance analysis and circuit metrics
- **Circom**: Circom-compatible format for interoperability
- **Bellman**: Bellman-compatible format for Rust integration

## Circuit Patterns

### Hash Preimage
Prove knowledge of `x` such that `hash(x) = y` without revealing `x`.

### Arithmetic Relations
Prove knowledge of values satisfying arithmetic constraints.

### Range Proofs
Prove a value lies within a specific range (requires additional techniques).

### Private Set Membership  
Prove membership in a set without revealing which element.

For detailed syntax documentation, see `docs/CAMELLIA_SYNTAX.md`.