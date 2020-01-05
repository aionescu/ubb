bits 32

extern _printf

global _printN
global _commonPrefixLength

; void printN(const char* s, int n);
; Prints the first n characters of s.
; If n >= strlen(s), the function results in undefined behavior.
segment data public data use32
  fmt db "%s\n", 0

segment code public code use32
_printN:
  push ebp
  mov ebp, esp

  mov eax, [ebp + 8] ; s
  add eax, [ebp + 12] ; s += n

  mov dl, [eax]
  mov byte [eax], 0

  push dword [ebp + 8]
  push fmt
  call _printf
  add esp, 8

  mov [eax], dl

  mov esp, ebp
  pop ebp
  ret

; int commonPrefixLength(const char* a, const char* b);
; Returns the length of the longest common prefix of the 2 strings.
_commonPrefixLength:
  push ebp
  mov ebp, esp
  push ebx
         
  mov eax, 0
  mov ecx, [ebp + 12] ; a
  mov edx, [ebp + 16] ; b

.looop:
  mov bl, [ecx]
  mov bh, [edx]

  cmp bl, bh
  jne .done

  inc eax
  inc ecx
  inc edx
  jmp .looop

.done:
  pop ebx
  mov esp, ebp
  pop ebp
  ret