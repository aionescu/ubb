bits 32

global start

extern exit
import exit msvcrt.dll

segment data use32 class=data
  s dd 12345678h, 1A2C3C4Dh, 98FCDD76h, 12783A2Bh
  len equ ($ - s) / 4
  d dd 0FFFFFFFFh

segment  code use32 class=code
start:
  mov edx, 3 ; offset into d

  mov esi, s
  cld

  mov ecx, len ; loop counter

looop:
  ; If we already set all 4 bytes of d, break out of the loop
  cmp edx, -1
  je break

  ; Load the next dword into eax
  lodsd

  ; Check that the highest byte of the lowest word is even by ANDing with 1
  ; Highest byte of the lowest word is in ah
  mov bl, ah
  and bl, 1b

  ; If odd, skip this iteration
  cmp bl, 1b
  je continue

  ; If even, set the next byte of d to the value of ah
  mov [d + edx], ah
  dec edx

continue:
  loop looop

break:
  push dword 0
  call [exit]