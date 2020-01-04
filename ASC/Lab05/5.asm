; Assignment 5
; A character string S is given. Obtain the string D containing all small letters from the string S.
bits 32

global start

extern exit
import exit msvcrt.dll

segment data use32 class=data
  s db 'aAbB2%x'
  len equ $ - s
  d times len db 0

segment  code use32 class=code
start:
  mov ecx, len ; loop counter
  mov esi, 0 ; s index
  mov eax, 0 ; d index

looop:
  ; Move the current byte of s into bl
  mov bl, [s + esi]

  ; if (char < 'a') contiinue;
  cmp bl, 'a'
  jl continue

  ; if (char > 'z') continue;
  cmp bl, 'z'
  jg continue

  ; Set the current byte of d to the current char, then move to the next byte of d
  mov [d + eax], bl
  inc eax
  
continue:
  inc esi
  loop looop

  push dword 0
  call [exit]