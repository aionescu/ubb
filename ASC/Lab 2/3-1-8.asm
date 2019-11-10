; (100 * a + d + 5 - 75 * b) / (c - 5)
bits 32

global start

extern exit
import exit msvcrt.dll

segment data use32 class=data
  a db 1
  b db 2
  c db 3
  d dw 4
    
segment  code use32 class=code
start:
  mov AL, [a]
  mov AH, 100
  mul AH ; AX <- 100 * a

  add AX, [d]
  add AX, 5
  mov DX, AX ; DX <- 100 * a + d + 5

  mov AL, [b]
  mov AH, 75
  mul AH ; AX <- 75 * b

  sub DX, AX ; DX <- 100 * a + d + 5 - 75 * b

  mov DL, [c]
  sub DL, 5 ; DL <- c - 5

  mov AX, DX
  div DL ; AL <- (100 * a + d + 5 - 75 * b) / (c - 5)

  push dword 0
  call [exit]