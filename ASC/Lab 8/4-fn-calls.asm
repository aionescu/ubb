bits 32

global start

extern exit, printf
import exit msvcrt.dll
import printf msvcrt.dll

segment data use32 class=data
  a dd 2147483647
  b dd 2147483647
  format db "%d * %d = %lld", 0

segment code use32 class=code
start:
  ; Multiply a and b, result is stored in EDX:EAX
  mov eax, dword [a]
  mul dword [b]

  push edx
  push eax ; Pushing EDX:EAX onto the stack for printf to print with %lld
  push dword [b]
  push dword [a]
  push dword format
  call [printf]
  add esp, 16 ; Clean up the stack
  
  push dword 0
  call [exit]