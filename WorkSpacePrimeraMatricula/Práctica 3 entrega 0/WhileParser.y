-- | WhileParser.y
--
-- A simple happy parser for While
-- December 2020
-- author: Pablo López
--
-- Note: grammar ambiguity is removed by surrounding compound
-- sentences inside of 'if', 'while', etc in 'begin' .. 'end'.
-- For example:
--
--     while (x <= 10) do         -- no begin end required
--         x := x + 1
--
--     while (x <= 10) do begin   -- begin end required
--         x := x + 1;
--         y := y - 1
--     end
--
-- are both valid syntax.

{
module WhileParser ( happyParseWhile
                   , parser
                   ) where

import WhileLexer
import While

}

%name happyParseWhile

%tokentype { Token }

%token
    NUM       { LITERAL_INT $$ }
    ID        { IDENTIFIER $$ }
    '+'       { PLUS }
    '-'       { MINUS }
    '*'       { ASTERISK }
    '('       { LPAREN }
    ')'       { RPAREN }
    'true'    { LITERAL_TRUE }
    'false'   { LITERAL_FALSE }
    '&&'      { AMPERSANDS }
    '!'       { EXCLAMATION }
    '='       { EQUALS }
    '<='      { LESSEQUALS }
    ':='      { ASSIGN }
    'skip'    { SKIP }
    ';'       { SEMICOLON }
    'if'      { IF }
    'then'    { THEN }
    'else'    { ELSE }
    'while'   { WHILE }
    'do'      { DO }
    'abort'   { ABORT }
    'repeat'  { REPEAT }
    'until'   { UNTIL }
    'for'     { FOR }
    'to'      { TO }
    'begin'   { BEGIN }
    'end'     { END }
    'program' { PROGRAM }
    ','       { COMMA }

%nonassoc '=' '<='
%left '&&'
%left '!'
%left '+' '-'
%left '*'

%%

Program :: { (String, [Var], Stm) }
Program : 'program' ID Outputs ';' Stm_List { ($2, reverse $3, $5) }

Outputs :: { [Var] }
Outputs : '(' Var_List ')'   { $2 }
        | {- epsilon -}      { [] }

Var_List :: { [Var] }
Var_List : ID                { [$1] }
         | Var_List ',' ID   { $3:$1 }

Stm_List :: { Stm }
Stm_List : Stm                { $1 }
         | Stm ';' Stm_List   { Comp $1 $3 }

Stm :: { Stm }
Stm : ID ':=' Aexp                            { Ass $1 $3}
    | 'skip'                                  { Skip }
    | 'if' Bexp 'then' Stm 'else' Stm         { If $2 $4 $6 }
    | 'while' Bexp 'do' Stm                   { While $2 $4 }
    | 'abort'                                 { Abort }
    | 'repeat' Stm_List 'until' Bexp          { Repeat $2 $4 }      -- todo
    | 'for' ID ':=' Aexp 'to' Aexp 'do' Stm   { For $2 $4 $6 $8 }   -- todo
    | 'begin' Stm_List 'end'                  { $2 }

Bexp :: { Bexp }
Bexp : 'true'            { TRUE }
     | 'false'           { FALSE }
     | Aexp '=' Aexp     { Eq $1 $3 }
     | Aexp '<=' Aexp    { Le $1 $3 }
     | '!' Bexp          { Neg $2 }
     | Bexp '&&' Bexp    { And $1 $3 }
     | '(' Bexp ')'      { $2 }

Aexp :: { Aexp }
Aexp : NUM               { N $1 }
     | ID                { V $1 }
     | Aexp '+' Aexp     { Add $1 $3 }
     | Aexp '-' Aexp     { Sub $1 $3 }
     | Aexp '*' Aexp     { Mult $1 $3 }
     | '(' Aexp ')'      { $2 }

{

happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

parser :: FilePath -> IO((String, [Var], Stm))
parser filename = do
  s <- readFile filename
  return $ (happyParseWhile . alexScanTokens) s

}
