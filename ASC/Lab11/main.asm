; the program computes the factorial of a number and writes to the console the result
; the function factorial is defined in the file factorial.asm
bits 32

global start

import printf msvcrt.dll
import exit msvcrt.dll
extern printf, exit

; the code defined in factorial.asm will be written here
%include "factorial.asm"

segment data use32 class=data
	format_string db "factorial=%d", 10, 13, 0
    


segment code use32 class=code
start:
	push dword 6
	call factorial

	push eax
	push format_string
	call [printf]
	add esp, 2*4

	push 0
	call [exit]

