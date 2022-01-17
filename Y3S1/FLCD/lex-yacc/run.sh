#!/usr/bin/env bash
set -euo pipefail

lex src/mini.l
yacc -d src/mini.y

gcc -lfl lex.yy.c y.tab.c
./a.out

rm -f lex.yy.c y.tab.h y.tab.c a.out
