# Dacite2 Type System

## Overview
Dacite2 now supports a comprehensive type system with the following features:

## Type Kinds

### 1. Basic Types
Simple type identifiers like `u8`, `u16`, `u32`, etc.

```dacite
let x: u32 = 42;
```

### 2. Pointer Types
Pointers to other types, denoted with `*`. Multiple levels of indirection are supported.

```dacite
let ptr: *u32 = 42;           # Pointer to u32
let double_ptr: **u32 = 42;   # Pointer to pointer to u32
let triple_ptr: ***u32 = 42;  # Three levels of indirection
```

### 3. Slice Types
Slices represent a pointer plus a length, denoted with `[]`.

```dacite
let slice: []u32 = 42;        # Slice of u32
let slice_ptr: []*u32 = 42;   # Slice of pointers to u32
```

### 4. Array Types
Fixed-size arrays, denoted with `[N]` where N is the size.

```dacite
let arr: [10]u32 = 42;        # Array of 10 u32 elements
let arr_ptr: [10]*u32 = 42;   # Array of 10 pointers to u32
```

## Complex Type Composition

Types can be composed in complex ways:

```dacite
# Slice of arrays
let slice_arr: [][5]u32 = 42;

# Pointer to array
let ptr_arr: *[10]u32 = 42;

# Array of slices
let arr_slice: [5][]u32 = 42;

# Pointer to pointer to array of pointers
let complex: **[5]*u32 = 42;
```

## Function Type Annotations

Types can be used in function declarations:

```dacite
fun process : (
    basic: u32,
    ptr: *u32,
    slice: []u32,
    arr: [10]u32
) -> *u32 = {
    return 0;
};
```

## Implementation Details

### Token Support
The lexer now recognizes square brackets (`[` and `]`) for array and slice syntax.

### AST Representation
The `Type` AST node has been extended with:
- `Kind` enum: `Basic`, `Pointer`, `Slice`, `Array`
- `element_type`: Reference to the contained type
- `array_size_expr`: Size expression for arrays (must be a number literal)

### Parser
The `parse_type()` function recursively parses type expressions, supporting:
- Prefix `*` for pointers
- Prefix `[]` for slices
- Prefix `[N]` for arrays
- Any combination of the above

## Examples

See the test files for working examples:
- `tests/types_pointer.dc2` - Pointer types
- `tests/types_slice.dc2` - Slice types
- `tests/types_array.dc2` - Array types
- `tests/types_complex.dc2` - Complex type combinations
- `tests/types_showcase.dc2` - Comprehensive type showcase
