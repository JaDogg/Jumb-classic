format pe    
    JMP short skp_banner	
	db ".JaDogg.",0
	skp_banner:
	
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
	
	MOV EAX,DWORD  [FS:18h]
	MOV EAX,DWORD  [DS:EAX+30h]
	MOVZX EAX,BYTE [DS:EAX+2h]
	CMP EAX,1
	JE SHORT Dectected	

	MOV EBX,0401835h
	AGAIN:
	XOR BYTE [EBX+0401000h],090h
	DEC EBX
	CMP EBX,-1
	JNZ SHORT AGAIN
	
	POPFW
	POPAD
		
	MOV EAX,0401835h
	JMP EAX
