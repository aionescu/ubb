bits 32

global start

extern exit, printf, fopen, fread, fclose
import exit msvcrt.dll
import printf msvcrt.dll
import fopen msvcrt.dll
import fread msvcrt.dll
import fclose msvcrt.dll

segment data use32 class=data
  format db "%d", 0
  fileName db "Input.txt", 0
  accessMode db "r", 0
  fileDescriptor dd -1
  buffer db 0

segment code use32 class=code
start:
  mov ebx, 0 ; EBX will be used to count the number of odd digits read

  push dword accessMode     
  push dword fileName
  call [fopen]
  add esp, 8 ; Call fopen to open the file and clean up the stack

  mov [fileDescriptor], eax ; Save file descriptor

  cmp eax, 0
  je done ; If file can't be opened, jump to end

looop:
  push dword [fileDescriptor]
  push dword 1
  push dword 1
  push dword buffer
  call [fread]
  add esp, 16 ; Read the next digit and clean up stack

  cmp eax, 0
  je closeFile ; If read not successful, close the file and exit

  ; Check if last digit read is odd
  mov al, [buffer]
  sub al, '0'
  and al, 1
  cmp al, 0
  je even

  ; If odd, increment EBX
  inc ebx

even:
  ; Loop back to read the next digit
  jmp looop

closeFile:
  ; Call fclose and clean up stack
  push dword [fileDescriptor]
  call [fclose]
  add esp, 4

  ; Print the number of odd digits and clean up stack
  push dword ebx
  push dword format
  call [printf]
  add esp, 8

done:
  push dword 0
  call [exit]