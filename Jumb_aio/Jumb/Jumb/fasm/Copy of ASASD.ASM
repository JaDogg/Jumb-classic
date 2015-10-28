format PE

section "JumbCode"  data code readable writable executable

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
	
	POPFW
	POPAD
		
	MOV EAX,0401835h
	JMP EAX
