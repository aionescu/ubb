; (c + d) - (a + d) + b
bits 32

global start

extern exit
import exit msvcrt.dll

segment data use32 class=data
  a db 1
  b db 2
  c db 3
  d db 4
    
segment  code use32 class=code
start:
  mov AH, [c]
  add AH, [d] ; AH <- c + d
  
  mov DH, [a]
  add DH, [d] ; DH <- a + d
  
  sub AH, DH ; AH <- (c + d) - (a + d)
  
  add AH, [b] ; AH <- (c + d) - (a + d) + b
  
  push dword 0
  call [exit]