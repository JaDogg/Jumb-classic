
'--------

#Include "windows.bi"

Dim Shared As ZString Ptr zstrtemp,zstrtemp1
Dim Shared As Integer inttmp
Dim As String _TITLE
Function init_jumb Pascal  Alias "_jumb_init" ( )As Integer Export
	'init functions goes here
	'exepath
	'datax
	Return 0
End Function
Sub fnc_cls_jumb Pascal  Alias "_cls" ( ) Export
	'init functions goes here
	Cls
End Sub
Sub fnc_sleep_jumb Pascal  Alias "_sleep" ( ) Export
	'sleep functions goes here
	Sleep
End Sub
Sub fnc_end_jumb Pascal  Alias "_term" ( ) Export
	End
	ExitProcess(0)
End Sub
Sub fnc_print_str_eax Pascal  Alias "_print_str_eax" ( ) Export


	Asm
		mov zstrtemp,eax
	End Asm
	? *zstrtemp;

End Sub
Sub fnc_msgbox Pascal  Alias "_jumb_msgbox" ( ) Export


	Asm
		mov zstrtemp,eax
	End Asm
	
	MessageBox(GetActiveWindow(_TITLE),zstrtemp,StrPtr(),MB_OK)


End Sub
Sub fnc_print_int Pascal  Alias "_print_int_eax" ( ) Export


	Asm
		mov [inttmp],eax
	End Asm
	? Str(inttmp);

End Sub
Sub fnc_print_nl Pascal  Alias "_print_newline" ( ) Export

	?

End Sub
Sub fnc_lcase Pascal  Alias "_jumb_lcase" ( ) Export

	Asm
		mov zstrtemp,eax
	End Asm

	*zstrtemp = LCase(*zstrtemp)

	Asm
		mov eax,zstrtemp
	End Asm

End Sub
Sub fnc_add_1 Pascal  Alias "_jumb_add" ( rhs As Integer) Export
	Asm
		mov inttmp,eax
	End Asm
	
	inttmp += rhs
	
	Asm
		mov eax,inttmp
	End Asm
End Sub
Sub fnc_min_1 Pascal  Alias "_jumb_min" ( rhs As Integer) Export
	Asm
		mov inttmp,eax
	End Asm
	
	inttmp -= rhs
	
	Asm
		mov eax,inttmp
	End Asm
End Sub
Sub fnc_dev_1 Pascal  Alias "_jumb_dev" ( rhs As Integer) Export
	Asm
		mov inttmp,eax
	End Asm
	
	inttmp \= rhs
	
	Asm
		mov eax,inttmp
	End Asm
End Sub
Sub fnc_mul_1 Pascal  Alias "_jumb_mul" ( rhs As Integer) Export
	Asm
		mov inttmp,eax
	End Asm
	
	inttmp *= rhs
	
	Asm
		mov eax,inttmp
	End Asm
End Sub
Sub fnc_and_1 Pascal  Alias "_jumb_and" ( rhs As Integer) Export
	Asm
		mov inttmp,eax
	End Asm
	
	inttmp And= rhs
	
	Asm
		mov eax,inttmp
	End Asm
End Sub
Sub fnc_or_1 Pascal  Alias "_jumb_or" ( rhs As Integer) Export
	Asm
		mov inttmp,eax
	End Asm
	
	inttmp or= rhs
	
	Asm
		mov eax,inttmp
	End Asm
End Sub
Sub fnc_xor_1 Pascal  Alias "_jumb_xor" ( rhs As Integer) Export
	Asm
		mov inttmp,eax
	End Asm
	
	inttmp xor= rhs
	
	Asm
		mov eax,inttmp
	End Asm
End Sub
Sub fnc_mod_1 Pascal  Alias "_jumb_mod" ( rhs As Integer) Export
	Asm
		mov inttmp,eax
	End Asm
	
	inttmp mod= rhs
	
	Asm
		mov eax,inttmp
	End Asm
End Sub
Sub fnc_ttl Pascal  Alias "_jumb_title" ( ) Export

	Asm
		mov zstrtemp,eax
	End Asm
	
 _TITLE = *zstrtemp
 
	SetConsoleTitle(zstrtemp)

End Sub
Sub fnc_ucase Pascal  Alias "_jumb_ucase" ( ) Export

	Asm
		mov zstrtemp,eax
	End Asm

	*zstrtemp = UCase(*zstrtemp)

	Asm
		mov eax,zstrtemp
	End Asm

End Sub
Sub fnc_trim Pascal  Alias "_jumb_trim" ( ) Export

	Asm
		mov zstrtemp,eax
	End Asm

	*zstrtemp = Trim(*zstrtemp)

	Asm
		mov eax,zstrtemp
	End Asm

End Sub
Sub fnc_ltrim Pascal  Alias "_jumb_ltrim" ( ) Export

	Asm
		mov zstrtemp,eax
	End Asm

	*zstrtemp = LTrim(*zstrtemp)

	Asm
		mov eax,zstrtemp
	End Asm

End Sub
Sub fnc_rtrim Pascal  Alias "_jumb_rtrim" ( ) Export

	Asm
		mov zstrtemp,eax
	End Asm

	*zstrtemp = RTrim(*zstrtemp)

	Asm
		mov eax,zstrtemp
	End Asm

End Sub

'_print_str_eax