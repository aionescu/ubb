bits 32

global start

extern exit
import exit msvcrt.dll

segment data use32 class=data
  a dw 10h
  b dw 11h
  c dw 12h
    
segment  code use32 class=code
start:
  mov ax, [a]
  mov bx, [b]

  cmp ax, bx
  jg cmp2

  mov ax, bx
  
cmp2:
  mov bx, [c]

  cmp ax, bx
  jg end

  mov ax, bx

end:
  push dword 0
  call [exit]