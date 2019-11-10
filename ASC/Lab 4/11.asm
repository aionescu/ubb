; Given the byte A and the word B, compute the byte C as follows:
;   the bits 0-3 are the same as the bits 2-5 of A
;   the bits 4-7 are the same as the bits 6-9 of B.

bits 32

global start

extern exit
import exit msvcrt.dll

segment data use32 class=data
  a db 00101000b
  b dw 0000000110000000b
  ; result: 01101010b = 6Ah in AL
  
segment  code use32 class=code
start:
  mov al, [a]
  and al, 00111100b ; AND with mask to 'isolate' bits 2-5
  shr al, 2 ; Shift right by 2 so bits 2-5 become bits 0-3

  mov bx, [b]
  and bx, 0000001111000000b ; Isolate bits 6-9
  shr bx, 2 ; Bits 6-9 become bits 4-7

  or al, bl ; We only need care about the first byte, so we only use BL
  ; Result is stored in AL

  push dword 0
  call [exit]