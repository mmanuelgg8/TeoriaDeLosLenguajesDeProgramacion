-- | WhileLexer.x
--
-- A simple alex lexer for While
-- November 2020
-- author: Pablo López

{
module WhileLexer( Token(..)
                 , alexScanTokens
                 , lexer
                 ) where

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+				;
  "//".*				;

  -- Stm
  \:\=                                  { const ASSIGN }
  skip                                  { const SKIP }
  \;                                    { const SEMICOLON }
  if                                    { const IF }
  then                                  { const THEN }
  else                                  { const ELSE }
  while                                 { const WHILE }
  do                                    { const DO }
  repeat                                { const REPEAT }
  until                                 { const UNTIL }
  for                                   { const FOR }
  to                                    { const TO }
  begin                                 { const BEGIN }
  end                                   { const END }
  program                               { const PROGRAM }
  \,                                    { const COMMA }

  -- Bexp
  true                                  { const LITERAL_TRUE }
  false                                 { const LITERAL_FALSE }
  \&\&                                  { const AMPERSANDS }
  \!                                    { const EXCLAMATION }
  \=                                    { const EQUALS }
  \<\=                                  { const LESSEQUALS }

  -- Aexp
  \+					{ const PLUS }
  \-                                    { const MINUS }
  \*                                    { const ASTERISK }
  \(                                    { const LPAREN }
  \)                                    { const RPAREN }

  $digit+				{ LITERAL_INT . read }
  $alpha [$alpha $digit \_ \']*		{ IDENTIFIER }

{

data Token = LITERAL_INT Integer
           | IDENTIFIER String
           | PLUS
           | MINUS
           | ASTERISK
           | LPAREN
           | RPAREN
           | LITERAL_TRUE
           | LITERAL_FALSE
           | AMPERSANDS
           | EXCLAMATION
           | EQUALS
           | LESSEQUALS
           | ASSIGN
           | SKIP
           | SEMICOLON
           | IF
           | THEN
           | ELSE
           | WHILE
           | DO
           | REPEAT
           | UNTIL
           | FOR
           | TO
           | BEGIN
           | END
           | PROGRAM
           | COMMA
           deriving (Eq,Show)

lexer filename = do
  s <- readFile filename
  print $ alexScanTokens s

}
