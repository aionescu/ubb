bits 32

global start

extern exit, scanf
import exit msvcrt.dll
import scanf msvcrt.dll

segment data use32 class=data
  fmt db "%s", 0
  msgBegin db "Primul sir ", 0
  msgMid db "NU ", 0
  msgEnd db "este subsir in toate celelalte siruri.", 0
  s times 100 db 0
  cond db 1
  
segment code use32 class=code
start:
  push dword s
  push dword fmt
  call [scanf]

  cmp eax, 0
  je done

  

  jmp start

done:
  push dword 0
  call [exit]