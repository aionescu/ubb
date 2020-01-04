bits 32

global start

extern exit, scanf
import exit msvcrt.dll
import scanf msvcrt.dll

%include "strOps.asm"

segment data use32 class=data
  fmt db "%s", 0
  msgBegin db "Primul sir ", 0
  msgMid db "NU ", 0
  msgEnd db "este subsir in toate celelalte siruri.", 0
  s times 100 db 0
  s2 times 100 db 0
  cond db 1
  
segment code use32 class=code
start:
  push dword s
  push dword fmt
  call [scanf]
  add esp, 8

  cmp eax, 0
  je done

.looop:
  push dword s2
  push dword fmt
  call [scanf]
  add esp, 8

  cmp eax, 0
  je .done

  push s
  push s2
  call [strFind]
  add esp, 8

  cmp eax, 0
  je .false

  jmp .looop

.false:
  mov [cond], 0
  
.done:
  push msgBegin
  push fmt
  call [printf]
  add esp, 8

  cmp cond, 1
  je .printEnd

  push msgMid
  push fmt
  call [printf]
  add esp, 8

.printEnd:
  push msgEnd
  push fmt
  call [printf]
  add esp, 8
  
  push dword 0
  call [exit]