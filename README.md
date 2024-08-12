# take-home-project-1

This is a Clojure interpreter for a stack-based DSL. The interpreter is a Clojure macro which takes as input an implementation of the DSL and returns a Clojure function. The DSL is described in doc/rpl-take-home-project-clj.pdf.

This DSL has the following criteria:

- The `defstackfn` macro takes the DSL as input and outputs a clojure function.
- Variables are denoted with a leading exclamation point, e.g., `!x`
- Assignment to variables from the top of the stack are denoted with a trailing plus sign, e.g., `!x+`. The top of the stack is assigned to the variable and is not popped.
- Constants are pushed on the stack.
- A function is invoked with the invoke> block, which takes the function and the arity n. This pops n items from the stack and pushes the result of the function.
- The `<pop>` symbol pops the top of the stack.
- Conditionals are denoted with `if>` blocks. The top of the stack is popped and if the value is truthy the code after the `if>` is executed, otherwise the code after the `else>` is executed. The `else>` is required.
- 

Shadowing of variables occurs inside the if> block. If a variable is assigned a value, then it shadows the variable outside of the block, if it exists. See the tests for an example of this.

## Installation

Clone from https://github.com/hadils/take-home-project-1/tree/main

## Usage

The tests can be run from the test directory.

## Implementation

There are two passes: a symbol-gathering pass and the compilation pass. Assignments are used to define local variables. Local variables implemented as let bindings. Each variable is implemented as an atom. The difference between an argument and a variable is that arguments are initialized and variables are set to nil initially. `if>` blocks are recursively processed, creating a let block that shadows any variables for the outer block.

