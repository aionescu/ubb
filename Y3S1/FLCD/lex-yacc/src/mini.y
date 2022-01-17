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