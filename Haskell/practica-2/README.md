# The Natural Semantics of While

# Files

## Read only file (do not modify them)

Aexp.hs          - abstract syntax and semantics of Aexp
Bexp.hs          - abstract syntax and semantics of Bexp
While.hs         - abstract syntax for While
WhileLexer.hs    - lexer for While
WhileParser.hs   - parser for While
WhileExamples.hs - simple While programs written in abstract syntax
Examples/*.w     - simple While programs written in concrete syntax

## Editable files (to be completed)

NaturalSemantics.hs - Natural Semantics for While
Exercises02.hs      - Exercises 1 to 7

Please note that you need to use the `Update` type and the `fvAexp`,
`fvBexp`, and `update` functions defined in Exercises01.hs. Import or
copy them in Exercises02.hs.

# Plan

1. Complete the implementation of the Natural Semantics (NaturalSemantics.hs)
2. Complete exercises 1.1 to 1.3
3. At this point, you have implemented an interpreter for While based on its
   Natural Semantics. Use exercise 1.4 to test your interpreter. Feel free to
   further experiment with a few more While programs.
4. Complete exercises 2 and 3 to extend the While language with new looping
   statements.
5. Complete exercise 4 to define and implement a Natural Semantics for Aexp.
6. Complete exercise 5 to implement a simple static semantics check for While.
7. [Optional] Complete exercise 6 to build a Natural Semantics derivation tree.
