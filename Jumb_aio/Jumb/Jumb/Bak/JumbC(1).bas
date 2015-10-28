'jumb compiler
'jadogg
Type asm_loop_t1
	d As ZString*256
	t As ZString*256
End Type

#Include "stack.bas"
Const c_type_ignore   = 0

Const c_type_str 		 = 1
Const c_type_num 		 = 2
Const c_type_alphanum = 3

Const c_type_err 		 = 4

'Dim Shared As asm_loop_t1 loop_dat_tmp
ReDim Shared As String name_dec_varz(0)
ReDim Shared As asm_loop_t1 asm_data_loop1(0)
Dim Shared As String txt,name_project,o_txt,c_txt,c_txt_d_1,c_txt_d_2,c_txt_d_3,c_txt_sv1,j_pushm_code,loop_t1
Dim Shared As Integer f_inp,f_out,txtpos,linenum,c_typ,cnt_strz,push_method,cnt_dec_vars,cnt_loops,cnt_def_intz,loop_ttw,mov_eax_a_val
Dim Shared As Stack stk_loop,stk_if
Dim Shared As String c_sub_name
Dim Shared As Integer c_sub_curr_params
Function is_num2(a As String)As Integer
	Return  ((a[0] >= Asc("0")) And (a[0] <= Asc("9")))
End Function
Function is_alpha()As Integer
	Return ( (txt[txtpos] >= Asc("a")) And (txt[txtpos] <= Asc("z")) ) Or ( (txt[txtpos] >= Asc("A")) And (txt[txtpos] <= Asc("Z")) )
End Function
Function is_alpha_numeric()As Integer
	Return ( (txt[txtpos] >= Asc("a")) And (txt[txtpos] <= Asc("z")) ) Or ( (txt[txtpos] >= Asc("A")) And (txt[txtpos] <= Asc("Z")) ) Or ( (txt[txtpos] >= Asc("0")) And (txt[txtpos] <= Asc("9")) )
End Function
Function is_num()As Integer
	Return  ((txt[txtpos] >= Asc("0")) And (txt[txtpos] <= Asc("9")))
End Function
Function is_sp_chr()As Integer
	Return txt[txtpos] = Asc("(") Or txt[txtpos] = Asc(")") Or txt[txtpos] = Asc("*") Or txt[txtpos] = Asc("+") Or txt[txtpos] = Asc("-") Or txt[txtpos] = Asc("/")  Or txt[txtpos] = Asc("\") Or txt[txtpos] = Asc("=")
End Function
Function replace (  vdata As String,  rmvchar As String,  rplchar As String)  As String

	Dim px     As Long
	Dim first  As String
	Dim last   As String

	Do
		px = InStr ( 1, vdata, rmvchar )
		If px = 0 Then Exit Do
		first = Mid ( vdata, 1, px - 1 )
		last = Mid ( vdata, px + Len ( rmvchar )  )
		vdata = first + rplchar + last
	Loop

	Return vdata

End Function
Function declared_search() As Integer

	For nn As Integer = 1 To cnt_dec_vars

		If c_txt = name_dec_varz(nn) Then
			'we have a match
			Return 1
			Exit Function
		EndIf

	Next

	Return  0

End Function
Sub jumb_fix_txt


	txtpos = 0
	txt = Trim(txt)

End Sub
Sub add_const

	ReDim Preserve As String name_dec_varz(cnt_dec_vars)
	name_dec_varz(cnt_dec_vars) = c_txt_d_1 ' name of const

End Sub
Sub add_int

	ReDim Preserve As String name_dec_varz(cnt_dec_vars)
	name_dec_varz(cnt_dec_vars) = c_txt ' name of const

End Sub
Sub get_current_delim

	DO_IT_AGAIN:

	c_txt = ""
	c_typ = 0

	If Len(txt) = 0 Then
		'empty ignore
		c_txt = "" : Exit Sub

	ElseIf txtpos = Len(txt) Then
		'end line
		linenum += 1
		c_txt = "" : Exit Sub

	ElseIf txt[txtpos] = Asc(";") Then
		'coment ignore
		c_txt = "" : Exit Sub

	ElseIf txt[txtpos] = Asc(" ") Then
		txtpos += 1
		GoTo DO_IT_AGAIN

	ElseIf txt[txtpos] = 9 Then
		txtpos += 1
		GoTo DO_IT_AGAIN
	ElseIf is_sp_chr Then

		c_txt = Chr(txt[txtpos])
		txtpos += 1
	ElseIf txt[txtpos] = Asc("""") Then

		c_txt = ""
		c_typ = c_type_str

		Do

			c_txt = c_txt & Chr(txt[txtpos])

			txtpos+=1

			If txt[txtpos] = Asc("""") Then
				c_txt = c_txt & Chr(txt[txtpos])
				txtpos+=1
				Exit Do
			ElseIf txtpos = Len(txt) Then
				'? "error no ending quote "
				Exit Do
			EndIf

		Loop

	ElseIf is_alpha Then

		c_txt = ""
		c_typ = c_type_alphanum

		Do
			If txtpos = Len(txt) Then
				Exit Do
			EndIf

			If Not is_alpha_numeric Then
				Exit Do
			EndIf

			c_txt = c_txt & Chr(txt[txtpos])

			txtpos+=1

		Loop

	ElseIf is_num Then

		c_typ = c_type_num
		c_txt = ""

		Do
			If txtpos = Len(txt) Then
				Exit Do
			EndIf

			If Not is_num Then
				Exit Do
			EndIf

			c_txt = c_txt & Chr(txt[txtpos])

			txtpos+=1

		Loop

	Else
		c_txt = ""
	EndIf

End Sub
Sub get_input_item_1

	get_current_delim

	If c_txt = "(" Then



		get_current_delim

		'? c_txt
		'
		'c_txt_sv1 = c_txt

		'get_current_delim
		'
		'? c_txt

		If c_txt = ")" Then
			'hmm complicated
			'enable push method
			push_method = 0
		Else
			push_method = 1
		EndIf

	Else

		If push_method = 0 Then
			get_current_delim
			c_txt_sv1 = c_txt
		EndIf

	EndIf

	'c_txt = c_txt_sv1

	'If Len(c_txt) Then
	'	If c_txt[0] = Asc ( """" ) Then
	'		'direct str save it and make c_txt is number
	'	EndIf
	'EndIf

End Sub
Sub mov_to_eax
	'? "::: called mov_to_eax "
	'get the curr delim
	'get_current_delim

	'lets check what the type it is
	If c_typ = c_type_str Then
		'its a string now we need to to save the string
		cnt_strz += 1
		Print #f_out, Str ( "jmp _JUMB_SKP_TMP_STR" & cnt_strz  )
		Print #f_out, Str ( "_JUMB_TMP_STR" & cnt_strz & " db " & c_txt &  ",0" )
		Print #f_out, Str ( "_JUMB_SKP_TMP_STR" & cnt_strz  & ":")

		Print #f_out, Str ( "MOV EAX,_JUMB_TMP_STR" & cnt_strz )

	ElseIf c_typ = c_type_num Then
		'just move it
		Print #f_out,Str ( "MOV EAX," & c_txt )
	Else
		If declared_search = 1 Then
			'registered item
			If mov_eax_a_val = 0 Then
				Print #f_out, Str ( "MOV EAX," & c_txt  )
			Else
				Print #f_out, Str ( "MOV EAX,[" & c_txt & "]" )
			EndIf
		Else
			'error
			'? ":::err"
		EndIf
	EndIf

	'? "::: end mov_to_eax "
End Sub
Sub jumbeval_1

	Select Case c_txt

		Case "DEFSTR"
			'str


		Case "DEFNUM"
			'int

			cnt_def_intz +=1


			Print #f_out,Str("jmp _jumb_defint_skp" &  cnt_def_intz)

			Do
				cnt_dec_vars += 1
				add_int()

				get_current_delim
				If c_txt <> "" Then
					Print #f_out,Str(c_txt & " dd ?")
				Else
					Print #f_out,Str("_jumb_defint_skp" & cnt_def_intz & ":")
					Exit Do
				EndIf
			Loop

		Case "CLS"

			Print #f_out, "invoke _cls"
			get_current_delim
			If c_txt <> "" Then
				'ERROR
			EndIf

		Case "PAUSE"
			Print #f_out, "invoke _sleep"
			get_current_delim
			If c_txt <> "" Then
				'ERROR
			EndIf
		Case "SUB"
			'find endsub
			'its SUB NAME 0
			'
			get_current_delim
			c_sub_name = c_txt
			
			get_current_delim
			c_sub_curr_params = CUInt(c_txt)
			
			'proc name ABC,ABC1,ABC2
			Print #f_out, "proc " & c_sub_name & " " ;
			If (c_sub_curr_params > 0) Then
				For MN As Integer = 1 To c_sub_curr_params
					Print #f_out,""
				Next
				
			EndIf
		Case "ENDSUB"	
			'
		Case "EXITSUB"
			
		Case "SUBRETURN"
			
		Case "IF"
			'find endif
		Case "TERMINATE"
			'EXIT APP
			Print #f_out, "invoke _term"
			get_current_delim

			If c_txt <> "" Then
				'ERROR
			EndIf

			'include same way := trim,ltrim,rtrim
		Case "LOOP"
			'loop ha now this is baddd
			'dont ya think
			'loooooooooooooooooopy loooooooooopy
			'ha ha ha
			'
			'LOOP INT FROM TO
			'ENDLOOP
			'+1 loop count and asign a num to loop
			cnt_loops += 1
			'Dim As String loop_t2
			'loop_t2 = str("_jumb_loop" & cnt_loops)
			ReDim Preserve As asm_loop_t1 asm_data_loop1(cnt_loops)
			stk_loop.push(Cast(Any Ptr,cnt_loops))
			'ok now we have the jumb loop thing
			'print it
			get_current_delim
			'Print #f_out,Str("mov " & c_txt &",");
			loop_t1 = c_txt
			get_current_delim

			If is_num2 (c_txt) Then
				Print #f_out,Str("mov eax," & c_txt  )
			Else
				Print #f_out,Str("mov eax,[" & c_txt & "]" )
			EndIf

			Print #f_out,Str("mov [" & loop_t1 & "],eax" )
			'Print #f_out,c_txt
			Print #f_out, Str("_jumb_loop" & cnt_loops & "_begin:")
			'---
			'Print #f_out,Str("inc [" & loop_t1 & "]")
			get_current_delim
			asm_data_loop1(cnt_loops).d  = loop_t1
			asm_data_loop1(cnt_loops).t  = c_txt
			'Print #f_out,Str("cmp [" & loop_t1 & "]," & c_txt)
			'Print #f_out,Str("jge " & "_jumb_loop" & cnt_loops & "_end")
		Case "ENDLOOP"
			'loop end here
			'here goes the end of current loop
			loop_t1= ""
			loop_ttw = Cast(Integer ,stk_loop.pop)
			loop_t1 = "_jumb_loop" & Str(loop_ttw)

			loop_t1 = asm_data_loop1(loop_ttw).d



			'this is funn
			If is_num2(asm_data_loop1(loop_ttw).t)  Then
				Print #f_out,Str("cmp [" & loop_t1 & "]," & asm_data_loop1(loop_ttw).t)
			Else
				'mov eax,[asm_data_loop1(loop_ttw).t]
				Print #f_out,Str("mov eax,[" & asm_data_loop1(loop_ttw).t & "]" )
				Print #f_out,Str("cmp [" & loop_t1 & "],eax" )
			EndIf


			Print #f_out,Str("jge " & "_jumb_loop" & loop_ttw & "_end")
			Print #f_out,Str("inc [" & loop_t1 & "]")
			loop_t1 = "_jumb_loop" & loop_ttw
			Print #f_out,Str("jmp " & loop_t1 & "_begin")
			Print #f_out,Str(loop_t1 &  "_end:")
		Case "LCASE"
			'inp str
			'out is str
			'STR = LCASE(STR)
			'store in eax
			'? ":: LCASE DETECTED"
			'get_input_item_1
			get_input_item_1

			If push_method = 1 Then
				mov_to_eax
				j_pushm_code = Str("invoke _jumb_lcase"  & Chr(13,10) &  j_pushm_code)
				'j_pushm_code = Str(& j_pushm_code)
				jumbeval_1

			EndIf

		Case "UCASE"
			'? ":: UCASE DETECTED"
			get_input_item_1

			If push_method = 1 Then

				mov_to_eax
				j_pushm_code = Str("invoke _jumb_ucase"  & Chr(13,10) &  j_pushm_code)
				'j_pushm_code = Str(& j_pushm_code)
				jumbeval_1

			EndIf
		Case "TITLE"
			'? ":: TITLE DETECTED"
			get_input_item_1

			If push_method = 1 Then

				mov_to_eax
				j_pushm_code = Str("invoke _jumb_title"  & Chr(13,10) &  j_pushm_code)
				'j_pushm_code = Str(& j_pushm_code)
				jumbeval_1

			EndIf		
			
		Case "ECHO"
			'Inp str , out none
			'ECHO(STR)
			'? ":: ECHO DETECTED"
			'get_input_item_1
			get_input_item_1
			If push_method = 1 Then
				mov_to_eax
				j_pushm_code = Str("invoke _print_str_eax"  & Chr(13,10) &  j_pushm_code)
				jumbeval_1

			EndIf

		Case "ECHOINT"
			'? ":: ECHOint DETECTED"
			get_input_item_1
			If push_method = 1 Then
				mov_eax_a_val = 1
				mov_to_eax
				mov_eax_a_val = 0
				j_pushm_code = Str("invoke _print_int_eax"  & Chr(13,10) &  j_pushm_code)
				jumbeval_1

			EndIf

		Case "ECHONEWLINE"

			Print #f_out, "invoke _print_newline"
			get_current_delim
			If c_txt <> "" Then
				'ERROR
			EndIf

		Case "TRIM"
			'get_input_item_1
			get_input_item_1
			If push_method = 1 Then
				mov_to_eax
				j_pushm_code = Str("invoke _jumb_trim"  & Chr(13,10) &  j_pushm_code)
				jumbeval_1

			EndIf
			'? c_txt
			'? j_pushm_code
		Case "LTRIM"
			'get_input_item_1
			get_input_item_1
			If push_method = 1 Then
				mov_to_eax
				j_pushm_code = Str("invoke _jumb_ltrim"  & Chr(13,10) &  j_pushm_code)
				jumbeval_1

			EndIf

		Case "RTRIM"
			'Inp str , out none
			'ECHO(STR)

			'get_input_item_1
			get_input_item_1
			If push_method = 1 Then
				mov_to_eax
				j_pushm_code = Str("invoke _jumb_rtrim"  & Chr(13,10) &  j_pushm_code)
				jumbeval_1

			EndIf

		Case "ASM"

			Print #f_out, Mid(txt,txtpos+2)

		Case "KILL"

		Case "ECHOSTREAX"
			Print #f_out, "invoke _print_str_eax"
			get_current_delim

			If c_txt <> "" Then
				'ERROR
			EndIf

		Case "CONSTZSTR"

			cnt_dec_vars += 1

			get_current_delim

			'? c_txt

			c_txt_d_1 = c_txt
			c_txt = ""

			get_current_delim

			c_txt_d_2 = c_txt

			'? c_txt

			get_current_delim

			If c_txt <> "" Then
				'? "ERROR ",c_txt
			EndIf

			Print #f_out, "jmp _jumb_strz_const_skp_n" & cnt_strz

			Print #f_out,c_txt_d_1 & " db "  & c_txt_d_2 & ",0"

			Print #f_out, "_jumb_strz_const_skp_n" & cnt_strz & ":"

			cnt_strz += 1

			add_const()


		Case	Else
			'ignore

	End Select

End Sub

name_project = Trim(Command(1),"""")

If name_project = "" Then End
'open the jmb file
f_inp = FreeFile
Open name_project & ".jum" For Input As #f_inp

'open the asm file
f_out = FreeFile
Open name_project & "_0_.asm" For Output As #f_out

Do
	'if a new line
	If push_method = 1 Then

		Print #f_out, j_pushm_code
		j_pushm_code = ""
		push_method = 0

	EndIf

	Line Input #f_inp, txt
	o_txt = txt
	jumb_fix_txt

	get_current_delim
	jumbeval_1()

Loop Until Eof(1)
'Sleep