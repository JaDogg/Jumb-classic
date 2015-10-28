format PE

section "JumbCode"  data code readable writable executable

include 'E:\Jumb\Jumb\fasm\INCLUDE\win32ax.inc'

	PUSHAD
	PUSHFW
		
	Dectected:
	
	RDTSC
	XOR ECX,ECX
	ADD ECX,EAX
	RDTSC
	SUB EAX,ECX
	CMP EAX,0FFFh
	JNB SHORT Dectected

	MOV EBX,08AF4h
	AGAIN:
	XOR BYTE [EBX+0401000h],090h
	DEC EBX
	CMP EBX,-1
	JNZ SHORT AGAIN

	invoke VirtualProtect,00400000h,00001000h,40h,OLDProtect
	MOV EBX,0040003Ch
	MOV ECX,DWORD [EBX]
	ADD ECX,00400006h
	XOR EBX,EBX   
	MOV BX,WORD [ECX]
	PUSH ECX
	ADD ECX,0F2h
	clear_section:
	MOV EDX,28h	
	clear_section_s:
	MOV BYTE [ECX],0h
	INC ECX
	DEC EDX
	JNE SHORT clear_section_s
	DEC EBX
	JNE SHORT clear_section
	POP ECX
	MOV WORD [ECX],BX

	
	POPFW
	POPAD
		
	MOV EAX,0401835h
	JMP EAX
	
	OLDProtect dd 02040001h
	
	data import

	library kernel32,'KERNEL32.DLL',\
	user32,'USER32.DLL'
	 
	import_kernel32
	import_user32

	end data