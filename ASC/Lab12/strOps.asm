bits 32

extern printf
import printf msvcrt.dll

; void printN(const char* s, int n);
; Prints the first n characters of s.
; If n >= strlen(s), the function results in undefined behavior.
global _printN
segment data public data use32
  fmt db "%s\n", 0

segment code public code use32
_printN:
	push ebp
	mov ebp, esp

	mov eax, [ebp + 8] ; s
  add eax, [ebp + 12] ; s += n

  mov edx, [eax]
  mov [eax], 0

  push [ebp + 8]
  push fmt
  call [printf]
  add esp, 8

  mov [eax], edx

	mov esp, ebp
	pop ebp
  ret

; int commonPrefixLength(const char* a, const char* b);
; Returns the length of the longest common prefix of the 2 strings.
global _commonPrefixLength
segment data public data use32
segment code public code use32
_commonPrefixLength:
	push ebp
	mov ebp, esp
  pushad 
         
  mov eax, 0
  mov ecx, [ebp + 8] ; a
  mov edx, [ebp + 12] ; b

.looop:
  cmp [ecx], [edx]
  jne .done

  inc eax
  inc ecx
  inc edx
  jmp .looop

.done
  popad
	mov esp, ebp
	pop ebp
  ret