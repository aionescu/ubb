; x + (2 - a * b) / (a * 3) - a + c, Unsigned
bits 32

global start

extern exit
import exit msvcrt.dll

segment data use32 class=data
  a db 1
  b dw 2
  c dd 3
  x dq 4
  ; a db 11
  ; b dw 200
  ; c dd 3010
  ; x dq 444
    
segment  code use32 class=code
start:
  ; dx:ax <- a * b
  mov ax, [a]
  mov dx, [b]
  mul dx

  ; eax <- a * b
  push dx
  push ax
  pop eax

  ; ecx <- (2 - a * b)
  mov ecx, 2
  sub ecx, eax

  ; dx:ax <- a * 3
  mov ax, word [a]
  mov dx, 3
  mul dx

  ; eax <- a * 3
  push dx
  push ax
  pop ebx

  ; eax <- (2 - a * b) / (a * 3)
  mov edx, 0
  mov eax, ecx
  div ebx

  ; eax <- (2 - a * b) / (a * 3) - a
  sub eax, [a]

  ; edx:eax <- (2 - a * b) / (a * 3) - a + c
  mov edx, 0
  add eax, [c]
  adc edx, 0

  ; edx:eax <- x + (2 - a * b) / (a * 3) - a + c
  add eax, dword [x]
  adc edx, dword [x + 4]

  push dword 0
  call [exit]