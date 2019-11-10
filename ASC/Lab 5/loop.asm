bits 32

global start

extern exit
import exit msvcrt.dll

segment data use32 class=data
  s db 'a2bcdef234'
  ; len_s equ $-s
  len_s dd 10
  letters times 6 db 0
  digits times 4 db 0

segment  code use32 class=code
start:
  mov ecx, [len_s] ; loop counter
  mov esi, 0 ; s idx
  mov eax, 0 ; letters idx
  mov ebx, 0 ; digits idx

looop:
  mov dl, [s + esi]

  cmp dl, '9'
  jl digit

  mov [letters + eax], dl
  inc eax
  jmp done

digit:
  mov [digits + ebx], dl
  inc ebx

done:
  inc esi
  loop looop
  
end:
  push dword 0
  call [exit]