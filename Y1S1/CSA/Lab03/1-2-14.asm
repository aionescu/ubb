; c - b - (a + a) - b, Signed
bits 32

global start

extern exit
import exit msvcrt.dll

segment data use32 class=data
  a db 1
  b dw 2
  c dd 3
  d dq 4
  ; a db 11
  ; b dw 200
  ; c dd 3010
  ; d dq 444
    
segment  code use32 class=code
start:
  mov ax, [b]
  cwde

  ; edx <- c - b
  mov edx, [c]
  sub edx, eax
  
  mov eax, 0
  
  ; ax <- a + a
  mov ax, [a]
  add ax, [a]

  ; edx <- c - b - (a + a)
  sub edx, eax

  mov ax, [b]
  cwde

  ; edx <- c - b - (a + a) - b
  sub edx, eax
  
  push dword 0
  call [exit]