bits 32

global start

extern exit, printf, scanf, fopen, fclose, fscanf
import exit msvcrt.dll
import printf msvcrt.dll
import scanf msvcrt.dll
import fopen msvcrt.dll
import fclose msvcrt.dll
import fscanf msvcrt.dll

segment data use32 class=data
  mode db "r", 0
  fmtS db "%s", 0
  fmtSS db "%s ", 0
  fmtD db "%d", 0
  file times 100 db 0
  num dd 0
  desc dd 0
  wrd times 100 db 0
  idx dd 0

segment code use32 class=code
start:
  push dword file
  push dword fmtS
  call [scanf]
  add esp, 8

  cmp eax, 0
  je .exit

  push dword num
  push dword fmtD
  call [scanf]
  add esp, 8

  mov ebx, [num]

  cmp eax, 0
  je .exit

  push dword mode
  push dword file
  call [fopen]
  add esp, 8

  cmp eax, 0
  je .exit

  mov [desc], eax

.looop:
  push dword wrd
  push dword fmtS
  push dword [desc]
  call [fscanf]
  add esp, 12

  cmp eax, -1
  je .close

  cmp dword [idx], 0
  je .print

  cmp [idx], ebx
  je .print

  inc dword [idx]
  jmp .looop

.print:
  push dword wrd
  push dword fmtSS
  call [printf]
  add esp, 8

  mov dword [idx], 1
  jmp .looop

.close:
  push dword [desc]
  call [fclose]
  add esp, 4

.exit:
  push dword 0
  call [exit]