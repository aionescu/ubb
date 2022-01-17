%{
#include <stdio.h>
#include <string.h>
#include "y.tab.h"
%}

%option noyywrap
%option caseless

ws [ \t\n]+
comment "--"[^\n]*"\n"

separator "{"|"}"|"["|"]"|"("|")"|","|";"|":"|"."|"'"|"\""

id [_a-zA-Z][_a-zA-Z0-9]*

intLit "0"|["+"|"-"]?[1-9][0-9]*
floatLit ["+"|"-"]?[0-9]+"."[0-9]*

charLit "'"[^\n]"'"
stringLit "\""[^\"\n]*"\""

%%

let { return LET; }
if { return IF; }
else { return ELSE; }
while { return WHILE; }
for { return FOR; }
in { return IN; }
read { return READ; }
print { return PRINT; }
length { return LENGTH; }
false { return FALSE; }
true { return TRUE; }
Bool { return BOOL; }
Int { return INT; }
Float { return FLOAT; }
Char { return CHAR; }
String { return STRING; }

"+" { return PLUS; }
"-" { return MINUS; }
"*" { return MUL; }
"/" { return DIV; }
"%" { return MOD; }

"=" { return ASSIGN; }
"==" { return EQ; }
"!=" { return NEQ; }
"<" { return LT; }
">" { return GT; }
"<=" { return LTE; }
">=" { return GTE; }
"&&" { return AND; }
"||" { return OR; }

{id} { return ID; }

{floatLit} {
  yylval.p_val = yytext;
  return FLOAT_LIT;
}

{intLit} {
  yylval.p_val = yytext;
  return INT_LIT;
}

{charLit} {
  yylval.p_val = yytext;
  return CHAR_LIT;
}

{stringLit} { return STRING_LIT; }

{separator} { return yytext[0]; }

{ws}
{comment}
. printf("Unrecognized token: %s\n", yytext);

%%
