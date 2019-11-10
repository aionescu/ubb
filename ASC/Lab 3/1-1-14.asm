; (a + d) - (c - b) + c, Unsigned
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
  ; edx:eax <- a + d
  mov eax, dword [d]
  add eax, [a]
  adc edx, dword [d + 4]

  ; ecx <- c - b
  mov ecx, [c]
  sub ecx, [b]

  ; edx:eax <- (a + d) - (c - b)
  sub eax, ecx
  sbb edx, 0

  ; edx:eax <- (a + d) - (c - b) + c
  add eax, [c]
  adc edx, 0

  push dword 0
  call [exit]