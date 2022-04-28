# zox

A programming language based on the book Crafting Interpreters - http://craftinginterpreters.com/

## Using

### Build

```
git clone https://github.com/raulgrell/zox.git
cd zox
zig build -Drelease-safe=true

```

### Run

For usage information, run:

```
~ Linux ~
./zox run

~ Windows ~
.\zox.exe
```

- CLI (Execute a script or command from the console)
- Repl-Mode (Allow monkeypatching and method redefinition)
- Embedded Mode (Create a native executable that embeds a project and runs it)

## Planned Features

- Single assignment/const values
- Switch statement - evaluate expression, execute first case with equal value
- Builtin-functions - to avoid polluting the language syntax/grammar/stdlib with unusual operations.
- Struct-scope storage - namespacing
- Extern Types - for native storage
- Extern Functions - for native functions
- Const methods - For optimization and invariants
- Embedding API - for easy use within Zig applications.

### Built in Types

These will be new primitives for the language, implemented in the compiler as extern types, nan-tagged type info

- Enums/Variants
- Arrays
- Lists [In Progress]
- Maps [In Progress]
- SIMD [Nice To have]

### Syntax

The idea is to identify fast operations and incentivize their use through syntax sugar.
This can include expressions that are easy to optimize in a single pass through constant propagation/folding, build-time execution etc

- Separate method call syntax (faster resolution, namespacing and explicit this)

    fn other(obj, str) { #body }
    class A (do = other) { fn do(self) { #body } }
    var a = A();
    A.do(a);                // Call function A.do
    a:do();                 // Call function A.do, passing a as the implicit first parameter.
    a.do(a, "something");   // Call field do as a function that takes a as its first parameter

- Expression function syntax (probably cheap to inline, avoid function call overhead)

    fn five() => 5;

#### Zig Inspired

- Error handling

    fn mightFail() try {} // Definition
    try mightFail(); // Callsite

- Test blocks (keeping tests near code)

    test "addition" {
        assert(5 + 5 == 10);
    }

## Planned Enhancements

- Compiled bytecode representation
- Avoid unnecessary stack manipulation (eg OpCode.Negate)
- Specialized opcodes
- Small string optimization
- Single allocation for strings and lists
- Compacting Garbage Collector (A Fast Garbage Compaction Algorithm - H.B.M. Jonkers)
- Dynamic Stack (use indices instead of pointed for internal references)
- Increase number of available constant slots (Load long)
- Coalesce constants
- Improved Global variable lookup
- Class/Method redefinition and propagation
- Cached common immutable values - Empty string, prototypes
- Platform modules 