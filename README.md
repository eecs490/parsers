Lexer and Parser from Simple Arithmetic Language

Adapted from calc and calc-ast examples in https://gitlab.inria.fr/fpottier/menhir/tree/master/demos

To build, run make.

To run after building, run ./run.native.
The program takes in (repeatedly) an arithmetic expression followed by a newline and produces on separate lines:\
    - the tokens produced by the lexer\
    - the AST produced by the parser
