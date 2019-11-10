; 2 * (a + b) - e
bits 32

global start

extern exit
import exit msvcrt.dll

segment data use32 class=data
  a db 1
  b db 2
  ; c db 3
  ; d db 4
  e dw 5
  ; f dw 6
  ; g dw 7
  ; h dw 8
    
segment  code use32 class=code
start:
  mov AL, [a]
  add AL, [b]

  mov DH, 2
  mul DH ; AX <- 2 * (a + b)

  sub AX, [e] ; AX <- 2 * (a + b) - e

  push dword 0
  call [exit]