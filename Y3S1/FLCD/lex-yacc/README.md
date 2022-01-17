# Lab 9 - Lex & Yacc

[GitHub](https://github.com/aionescu/flcd/tree/main/lex)

## Lex file

```lex
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
```

## Yacc file

```yacc
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define YYDEBUG 1

int yylex();

int yyerror(char *s)
{
  printf("%s\n",s);
}
%}

%union {
  int l_val;
  char *p_val;
}

%token LET
%token IF
%token ELSE
%token WHILE
%token FOR
%token IN
%token READ
%token PRINT
%token LENGTH
%token FALSE
%token TRUE
%token BOOL
%token INT
%token FLOAT
%token CHAR
%token STRING

%token PLUS
%token MINUS
%token MUL
%token DIV
%token MOD

%token ASSIGN
%token EQ
%token NEQ
%token LT
%token GT
%token LTE
%token GTE
%token AND
%token OR
%token ID
%token <p_val> INT_LIT
%token <p_val> FLOAT_LIT
%token <p_val> CHAR_LIT
%token STRING_LIT

%%
program : stmtList ;

factor : TRUE
  | FALSE
  | INT_LIT
  | FLOAT_LIT
  | CHAR_LIT
  | STRING_LIT
  | ID
  ;

term : factor
  | term MUL factor
  | term DIV factor
  | term MOD factor
  ;

expr : term
  | expr PLUS term
  | expr MINUS term
  | expr AND term
  | expr OR term
  ;

assign : ID ASSIGN expr ;

let : LET ID ASSIGN expr ;

print : PRINT expr ;

simpleStmt : let
  | assign
  | print
  ;

ifStmt : IF expr '{' stmtList '}' ;

whileStmt : WHILE expr '{' stmtList '}' ;

stmt : simpleStmt ';'
  | ifStmt
  | whileStmt
  ;

stmtList : stmt stmtTemp ;
stmtTemp : | stmtList ;

%%
extern FILE *yyin;

int main(int argc, char **argv)
{
  if(argc>1) yyin :  fopen(argv[1],"r");
  if(argc>2 && !strcmp(argv[2],"-d")) yydebug: 1;
  if(!yyparse()) fprintf(stderr, "\tO.K.\n");
}
```
