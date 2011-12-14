/******************************************************************************
* description: A lex program that prints the type of each token encountered
*              while scanning an input file. It tokenizes a pascal source file
*              based on the standard defined in ISO 7185.
* author:      Martin Dluhos
* created:     September 25, 2011
* revised:     November 2, 2011
* references:  http://flex.sourceforge.net/manual retrieved on 
*              September 26, 2011
*              http://en.wikipedia.org/wiki/Flex_lexical_analyser retrieved on
*              Pascal ISO 7185:1990 retrieved from
*              http://www.moorecad.com/standardpascal/iso7185_80.txt on
*              November 2, 2011
******************************************************************************/
   
%%
 
        /* Rules Section */
[ \t\n\r]+                               { } /* Skip whitespace. */
(\{|\(\*)([^*}]|\*[^)}])*\**(\}|\*\))    { } /* Comments- the most favorite */

       /* Keywords */
array                                    { return _ARRAY; }
begin                                    { return _BEGIN; }
case                                     { return _CASE; }
const                                    { return _CONST; }
do                                       { return _DO; }
downto                                   { return _DOWNTO; }
else                                     { return _ELSE; }
end                                      { return _END; }
file                                     { return _FILE; }
for                                      { return _FOR; }
function                                 { return _FUNCTION; }
goto                                     { return _GOTO; }
if                                       { return _IF; }
in                                       { return _IN; }
label                                    { return _LABEL; }
nil                                      { return _NIL; }
of                                       { return _OF; }
packed                                   { return _PACKED; }
write                                    { return _WRITE; }
writeln                                  { return _WRITELN; }
procedure                                { return _PROCEDURE; }
program                                  { return _PROGRAM; }
record                                   { return _RECORD; }
repeat                                   { return _REPEAT; }
set                                      { return _SET; }
then                                     { return _THEN; }
to                                       { return _TO; }
type                                     { return _TYPE; }
until                                    { return _UNTIL; }
var                                      { return _VAR; }
while                                    { return _WHILE; }
with                                     { return _WITH; }
        /* Punctuation. */
":="                                     { return _ASSIGN; }
":"                                      { return _COLON; }
","                                      { return _COMMA; }
"."                                      { return _DOT; }
".."                                     { return _ELLIPSES; }
"@"                                      { return _POINTER; }    /* Old style */
"^"                                      { return _POINTER; }    /* New style */
";"                                      { return _SEMICOLON; }
"("                                      { return _LPAREN; }
")"                                      { return _RPAREN; }
"["                                      { return _LBRACKET; }
"]"                                      { return _RBRACKET; }

        /* Operators */
not                                      { return _NOT; }
"!"                                      { return _NOT; }

        /* Multiplicative operators, according to Wirth */
and                                      { return _AND; }
div                                      { return _DIV; }
mod                                      { return _MOD; }
"%"                                      { return _MOD; }
"/"                                      { return _SLASH; }
"*"                                      { return _STAR; }

        /* Additive operators, according to Wirth. */
"-"                                      { return _DASH; }
or                                       { return _OR; }
"+"                                      { return _PLUS; }

        /* Relational operators, according to Wirth. */
"="                                      { return _EQ; }
">="                                     { return _GE; }
"=>"                                     { return _GE; } 
">"                                      { return _GT; }
"<="                                     { return _LE; }
"<"                                      { return _LT; }
"<>"                                     { return _NE; }

[a-zA-Z]([a-zA-Z]|[0-9]|_)*              { return _IDENTIFIER; }
0|([1-9][0-9]*)                          { return _INTEGER; }
[0-9]*(\.[0-9]+(E[+-]?[0-9]+)?|E[+-]?[0-9]+)  { 
                                           return _REAL; }
'(''|[^'])*'                             { return _STRING; }
.                                        { return _ERROR; }
%%
int yywrap () { return 1; } /* Gets called by the lexer at the end of file. */
