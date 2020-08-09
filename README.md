# zox

A programming language based on the book Crafting Interpreters - http://craftinginterpreters.com/

## Added Features
Lists

## Missing Features
Garbage Collector

### Planned
Fixed/Typed Arrays
Switch
Maps
Builtins
Zig-Style errors
Zig-Style tests
Separate method call syntax
Repl-Mode (Allow monkeypatching and method redefinition)
Embedding API
Expose SIMD primitives

### Compilation
Cache compiled bytecode
Validate field accesses

## Optimizations

### Planned
Avoid unnecessary stack manipulation (eg OpCode.Negate)
Small string optimization (4 bytes?)
Single allocation for strings and lists
Compacting Garbage Collectot (Mark and compact?)
A Fast Garbage Compaction Algorithm - H.B.M. Jonkers 

## Performance

### Planned
Benchmarking Suite

### Researching
Fixed vs Dynamic Stack
Increasing Constant (Load long)
Interned constants
Global variable lookup