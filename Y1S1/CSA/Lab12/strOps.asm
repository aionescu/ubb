bits 32

extern _printf

; int commonPrefixLength(const char* a, const char* b);
; Returns the length of the longest common prefix of the 2 strings.
global _commonPrefixLength
segment data public data use32
segment code public code use32
_commonPrefixLength:
  push ebp
  mov ebp, esp

  mov ecx, [ebp + 8] ; a
  mov edx, [ebp + 12] ; b

.looop:
  mov al, [ecx]
  mov ah, [edx]

  cmp al, ah
  jne .done

  inc ecx
  inc edx
  jmp .looop

.done:
  sub ecx, [ebp + 8]
  mov eax, ecx

  mov esp, ebp
  pop ebp
  ret