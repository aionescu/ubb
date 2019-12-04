bits 32

global start

extern exit
import exit msvcrt.dll

segment data use32 class=data
    
segment  code use32 class=code
start:
  mov ax, 3
  jmp END

  add ax, 4
  
END:
  push dword 0
  call [exit]