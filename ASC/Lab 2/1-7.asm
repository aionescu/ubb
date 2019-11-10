; 256 / 1
bits 32

global start

extern exit
import exit msvcrt.dll

segment data use32 class=data
    
segment  code use32 class=code
start:
  mov DX, 0
  mov AX, 256 ; DX:AX <- 0000_0000_0000_0000:0000_0001_0000_0000 = 256

  mov CX, 1
  div word CX ; AX <- DX:AX / CX = 256 / 1

  push dword 0
  call [exit]