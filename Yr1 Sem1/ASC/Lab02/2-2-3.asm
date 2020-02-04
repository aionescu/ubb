; (b + b + d) - (c + a)
bits 32

global start

extern exit
import exit msvcrt.dll

segment data use32 class=data
  a dw 1
  b dw 2
  c dw 3
  d dw 4
    
segment  code use32 class=code
start:
  mov AX, [b]
  add AX, [b]
  add AX, [d] ; AX <- b + b + d

  mov DX, [c]
  add DX, [a] ; DX <- c + a

  sub AX, DX ; AX <- (b + b + d) - (c + a)
  
  push dword 0
  call [exit]