%ifndef __STRSTR__
%define __STRSTR__

; int strEq(const char* a, const char* b)
; Returns 1 if a is equal to b, otherwise returns 0.
strEq:
  mov ecx, [esp + 4] ; a
  mov edx, [esp + 8] ; b

.looop:
  mov ah, [ecx]
  mov al, [edx]

  cmp ah, 0
  je .done

  cmp al, 0
  je .done

  cmp ah, al
  jne .retFalse

  inc ecx
  inc edx

  jmp .looop
  
.done:
  cmp [edx], 0
  jne .retFalse

  mov eax, 1
  ret

.retFalse:
  mov eax, 0
  ret

; int strFind(char* haystack, char* needle);
; Returns 1 if needle is in haystack, otherwise returns 0.
strFind:
  mov ecx, [esp + 4] ; haystack
  mov edx, [esp + 8] ; needle

.looop:
  mov al, [ecx]
  cmp al, 0
  je .retFalse

  push edx
  push ecx
  call [strEq]
  add esp, 8

  cmp eax, 1
  je .retTrue

  inc ecx
  jmp .looop

.retTrue:
  mov eax, 1
  ret

.retFalse:
  mov eax, 0
  ret
  
%endif