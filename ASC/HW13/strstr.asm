%ifndef __STRSTR__
%define __STRSTR__

; int strstr(char* haystack, char* needle);
; Returns 1 if needle is in haystack, otherwise returns 0.
strstr:
  pop ecx ; haystack
  pop edx ; needle

looop: 
  
retTrue:
  mov eax, 1

done:
  ret 8

%endif