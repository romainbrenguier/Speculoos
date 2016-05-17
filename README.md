# Speculoos

Speculoos (Specification Logics for Synthesis) is a set of tools for specification that can be converted to AIGER files and synthesized with external tools.

## Installation

To install the tool, ocaml-cudd and ocaml-aiger need to be installed in the parent directory of Speculoos.
To install ocaml-aiger, go to the parent directory of Speculoos then enter the following commands:
> git clone https://github.com/romainbrenguier/ocaml-aiger.git
> cd ocaml-aiger 
> make


To install ocaml-cudd, go to the parent directory of Speculoos then enter the following commands:
> git clone https://github.com/romainbrenguier/ocaml-cudd.git
> cd ocaml-cudd
> ./install_cudd.sh
> make

When this is done run:
> make


To generate some examples, run:
> make cycles
> make matrix

## Compiling with Speculoog

We recommend using ocamlbuild. For instance the command:
> ocamlbuild -tag use_ocaml-cudd -tag use_ocaml-aiger examples/rising_edge.native --

Build and execute the program in examples/rising_edge.ml.
You can basically substitute in this command any ml file that you write.

## Datatypes

For now only Booleans and unsigned integers are supported.
Constant values can be declared in that way:
> let b = bool true
> let i = int 5

The keyword "let" is part of Ocaml and is used for variables.
"true" and "5" are Ocaml Booleans and integers.

Speculoos variables are declared in that way:
> let x = var "x" (Type.int 8)

The string argument is the name the variable will be refered to in the synthesized circuit (which could be different from the name of the Ocaml variable).
The argument after Type.int is the with (number of bits) for this value (8 in the example).
Speculoos will automatically infer whether x should be an inpout, output or register.


## Expressions

To create circuit we use textual expressions:
> let d = (a $& b) $| (neg c $& d)

where $&, $|, neg represent bitwise AND, OR and NOT respectively.
The symbol $ is used to distinguish these from there Ocaml equivalent.
The name d represent a wire whose width is infered from the expression on the right.

It is possible to select a subset of bits from an expression using the function select:
> let e = select d [5,0]

This selects the first 6 bits of d and put them in reverse order in e.

## Outputs and updates

A circuit is generated from the description of register updates and outputs expressions.
For instance:
> let x = var "x" Type.bool 
> let previous = var "previous" Type.bool 
> let aiger = functional_synthesis [var "rising_edge", (x $& neg previous); previous, x]

generates a circuit in which x is an input, previous is a register that record the value of x at the previous clock cycle, and rising_edge is an output true when the current value is true and the previous is false.
Please see the file in examples/rising_edge.ml for the full program.
