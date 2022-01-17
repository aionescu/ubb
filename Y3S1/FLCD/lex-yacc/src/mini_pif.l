%{
#include <stdio.h>

typedef struct {
  char token[100];
  int index;
} Token;

Token pif[1000];
int pif_len = 0;

char st[1000][100];
int st_len = 0;

int pos(char *token) {
  for (int i = 0; i < st_len; ++i)
    if (!strcmp(st[i], token))
      return i + 1;

  strcpy(st[st_len++], token);
  return st_len;
}

void gen_pif(char *token, int index) {
  strcpy(pif[pif_len].token, token);
  pif[pif_len].index = index;
  pif_len++;
}

void gen_symbol(char *token_type, char *token) {
  int index = pos(token);
  gen_pif(token_type, index);
}

%}

ws [ \t\n]+
comment "--"[^\n]*"\n"

separator "{"|"}"|"["|"]"|"("|")"|","|";"|":"|"."|"'"|"\""
operator "+"|"-"|"*"|"/"|"%"|"="|"=="|"!="|"<"|">"|"<="|">="|"&&"|"||"
keyword "let"|"if"|"else"|"while"|"for"|"in"|"read"|"print"|"length"|"false"|"true"|"Bool"|"Int"|"Float"|"Char"|"String"

id [_a-zA-Z][_a-zA-Z0-9]*

intLit "0"|["+"|"-"]?[1-9][0-9]*
floatLit ["+"|"-"]?[0-9]+"."[0-9]*

charLit "'"[^\n]"'"
stringLit "\""[^\"\n]*"\""

%%

{floatLit} gen_symbol("FLOAT", yytext);
{intLit} gen_symbol("INT", yytext);
{operator} gen_pif(yytext, 0);
{charLit} gen_symbol("CHAR", yytext);
{stringLit} gen_symbol("STRING", yytext);
{separator} gen_pif(yytext, 0);
{keyword} gen_pif(yytext, 0);
{id} gen_symbol("ID", yytext);

{ws}
{comment}
. printf("Unrecognized token: %s\n", yytext);

%%

int main(int argc, char **argv) {
  yyin = stdin;
  yylex();

  FILE *out_pif = fopen("Out/pif.txt", "w");
  FILE *out_st = fopen("Out/st.txt", "w");

  for (int i = 0; i < pif_len; ++i)
    fprintf(out_pif, "(%s, %d)\n", pif[i].token, pif[i].index);

  for (int i = 0; i < st_len; ++i)
    fprintf(out_st, "%d %s\n", i + 1, st[i]);

  fclose(out_pif);
  fclose(out_st);
}
