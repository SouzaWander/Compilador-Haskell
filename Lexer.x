{
module Lexer where
}

%wrapper "basic"

$alpha = [_a-zA-Z]
$digit = [0-9]
$white = [\ \t\n\r]

tokens :-

"//".*                                                   ;
"/*"(\s|.)[^\/]*"*/"                                     ;
$white+       ;

"+"          { \_ -> PLUS }
"-"          { \_ -> MINUS }
"*"          { \_ -> MULT }
"/"          { \_ -> DIV }
"%"          { \_ -> REST }
"("          { \_ -> LPAREN }
")"          { \_ -> RPAREN }
"{"          { \_ -> LCHAV }
"}"          { \_ -> RCHAV }
"=="         { \_ -> EQUAL_EQUAL }
"!="         { \_ -> DIFF }
"<"          { \_ -> LESS }
"<="         { \_ -> L_EQUAL }
">"          { \_ -> GREATER }
">="         { \_ -> G_EQUAL }
"="          { \_ -> EQUAL}
";"          { \_ -> DOTCOMMA }
","          { \_ -> COMMA }
"++"         { \_ -> INCR }
"--"         { \_ -> DECR }
"true"       { \_ -> TRUE }
"false"      { \_ -> FALSE }
"while"      { \_ -> WHILE }
"for"        { \_ -> FOR }
"if"         { \_ -> IF }
"else"       { \_ -> ELSE}
"int"        { \_ -> INT }
"bool"       { \_ -> BOOL }
"return"     { \_ -> RETURN }
"scan_int"   { \_ -> SCAN_INT }
"print_int"  { \_ -> PRINT_INT }

$alpha($alpha|$digit)*    { \s -> ID s}
$digit+   { \s -> NUM (read s)}


{

data Token = ID String
           | NUM Int
           | PLUS
           | MINUS
           | MULT
           | DIV
           | REST
           | LPAREN
           | RPAREN
           | LCHAV
           | RCHAV
           | EQUAL_EQUAL
           | DIFF
           | LESS
           | L_EQUAL
           | GREATER
           | G_EQUAL
           | EQUAL
           | DOTCOMMA
           | COMMA
           | INCR
           | DECR
           | TRUE
           | FALSE
           | WHILE
           | FOR
           | IF
           | ELSE
           | INT
           | BOOL
           | RETURN
           | SCAN_INT
           | PRINT_INT
           deriving (Eq, Show)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}
