#Include Once "crt/string.bi"
'#Include Once "windows.bi"

Type j_str_info
	Len As Integer
	Start As Integer
End Type
Type j_mega_string
	'JaDogg
	_ZData As ZString Ptr
	_ZData_Size As Integer
	_ZData_Size_prev_itm As Integer
	_ZData_prev_itm As j_str_info
	_ZData_Items As j_str_info Ptr
	_P_ID As Integer
	_Z_CNT As Integer
	_Temp As j_str_info
	Declare Sub add_first(zstr As String)
	Declare Sub add_last(zstr As String)
	Declare Sub add_after_prev(zstr As String)
	Declare Sub add_before_prev(zstr As String)
	Declare Sub memforw(p_src As Any Ptr,P_forw As Integer,p_len As Integer)
End Type
Sub j_mega_string.memforw(p_src As Any Ptr,P_forw As Integer,p_len As Integer)
	'? p_src,P_forw,p_len
	Dim As Any Ptr P1
	P1 = Allocate(p_len)
	memcpy(P1,p_src,p_len)
	memcpy(p_src + P_forw,P1,p_len)
	DeAllocate(P1)
	P1 = 0
End Sub
Sub j_mega_string.add_before_prev(zstr As String)

	If _Z_CNT = 0 Then
		'noitm
		add_last(zstr)
		Exit Sub
	EndIf

	If ( _ZData_prev_itm.Start +  _ZData_prev_itm.Len ) = _ZData_Size Then
		'? "'prev = add_last"
		'we only need to move the last itm
		_Z_CNT += 1
		_ZData_Items = ReAllocate(_ZData_Items,_Z_CNT * SizeOf(j_str_info))
		Swap _ZData_Items[_Z_CNT-1].Start,_ZData_Items[_Z_CNT-2].Start
		Swap _ZData_Items[_Z_CNT-1].len,_ZData_Items[_Z_CNT-2].len

		_ZData = ReAllocate(_ZData,_ZData_Size + Len(zstr))
		'move the prev itm forword
		memforw(_ZData + _ZData_prev_itm.Start, Len(zstr),_ZData_Size - _ZData_prev_itm.start)
		'memcpy(_ZData + _ZData_prev_itm.Start + Len(zstr),_ZData + _ZData_prev_itm.Start,_ZData_prev_itm.Len)
		memcpy(_ZData + _ZData_prev_itm.Start,StrPtr(zstr),Len(zstr))
		_ZData_Items[_Z_CNT-2].Start = _ZData_prev_itm.Start
		_ZData_Items[_Z_CNT-2].len = Len(zstr)
		_P_ID = _Z_CNT-2
		If _Z_CNT > 0 Then memcpy(@_ZData_prev_itm,@_ZData_Items[_Z_CNT-2],SizeOf(j_str_info))
		_ZData_Size += Len(zstr)
	Else
		'1234
		'12p4
		_Z_CNT += 1
		'my id = _P_ID - 1
		'how much after the prev one
		';((_Z_CNT-1)-_P_ID)*SizeOf(j_str_info)
		'move this much SizeOf(j_str_info) forward

		_ZData_Items = ReAllocate(_ZData_Items,_Z_CNT * SizeOf(j_str_info))
		'memforw(_ZData_Items +(((_Z_CNT-1)-_P_ID)*SizeOf(j_str_info)), SizeOf(j_str_info),(((_Z_CNT-1)-_P_ID)*SizeOf(j_str_info)))

		_ZData_Items[_P_ID].len = Len(zstr)
		_ZData_Items[_P_ID].len = _ZData_prev_itm.Start
		_ZData = ReAllocate(_ZData,_ZData_Size + Len(zstr))

		'size for the move
		'_ZData_Size - _ZData_prev_itm.Start

		'src
		'_ZData + _ZData_prev_itm.Start

		'dest
		'_ZData + _ZData_prev_itm.Start + len(zstr)
		memforw(_ZData + _ZData_prev_itm.Start, Len(zstr),_ZData_Size - _ZData_prev_itm.start)	
		'memcpy(_ZData + _ZData_prev_itm.Start + Len(zstr),_ZData + _ZData_prev_itm.Start,_ZData_Size - _ZData_prev_itm.Start)
		memcpy(_ZData + _ZData_prev_itm.Start,StrPtr(zstr),Len(zstr))
		_ZData_Size += Len(zstr)
		'
		memcpy(@_ZData_prev_itm,@_ZData_Items[_P_ID],SizeOf(j_str_info))
		'_P_ID is same
	EndIf

End Sub
Sub j_mega_string.add_after_prev(zstr As String)
	If ( _ZData_prev_itm.Start +  _ZData_prev_itm.Len ) = _ZData_Size Then
		'? "'prev = add_last"
		add_last(zstr)
	EndIf
End Sub
Sub j_mega_string.add_last(zstr As String)
	_Z_CNT += 1
	_ZData_Items = ReAllocate(_ZData_Items,_Z_CNT * SizeOf(j_str_info))
	_ZData_Items[_Z_CNT-1].Len = Len(zstr)
	_ZData_Items[_Z_CNT-1].Start = _ZData_Size
	_ZData = ReAllocate(_ZData,_ZData_Size + _ZData_Items[_Z_CNT-1].Len)
	memcpy(_ZData + _ZData_Size,StrPtr(zstr),_ZData_Items[_Z_CNT-1].Len)
	_ZData_Size_prev_itm = _ZData_Size
	_ZData_Size+= _ZData_Items[_Z_CNT-1].Len
	_P_ID = _Z_CNT-1
	If _Z_CNT > 0 Then memcpy(@_ZData_prev_itm,@_ZData_Items[_Z_CNT-1],SizeOf(j_str_info))
End Sub
Sub j_mega_string.add_first(zstr As String)
	Dim As Any Ptr P1,P2
	Dim As Integer _Z_CNT_bak,_ZData_Size_Bak,_ZData_Items_Size_bk

	P1 = Allocate(_Z_CNT * SizeOf(j_str_info))
	memmove(P1,_ZData_Items,_Z_CNT * SizeOf(j_str_info))
	P2 = Allocate(_ZData_Size)
	memmove(P2,_ZData,_ZData_Size)

	_Z_CNT_bak = _Z_CNT
	_ZData_Size_Bak = _ZData_Size
	_ZData_Items_Size_bk = _Z_CNT * SizeOf(j_str_info)

	_Z_CNT = 0
	_ZData_Size = 0
	'DeAllocate(_ZData_Items)
	'DeAllocate(_ZData)
	add_last(zstr)

	_Z_CNT = _Z_CNT_bak + 1
	_ZData_Items = ReAllocate(_ZData_Items,_Z_CNT * SizeOf(j_str_info))
	'_ZData_Items[0].Len = Len(zstr)
	'_ZData_Items[0].Start = 0
	_ZData = ReAllocate(_ZData,_ZData_Size + _ZData_Size_Bak)
	'memcpy(_ZData,StrPtr(zstr),_ZData_Items[0].Len)
	''add_last(zstr)
	'
	memmove(_ZData_Items,P1,_ZData_Items_Size_bk)
	memmove(_ZData + _ZData_Size ,P2,_ZData_Size_Bak)
	'
	'_ZData_Size_prev_itm = _ZData_Items[_Z_CNT].Len
	_ZData_Size+= _ZData_Size_Bak
	_P_ID = 0
End Sub
'
'Dim As j_mega_string aa
'
'cls
'
'For i As Integer = 0 To 9999
'	If (Rnd*5) Mod 2 Then
'	   aa.add_last( "<" & i &"last>" ) ': Cls :? Mid(*aa._ZData,1,aa._ZData_Size) :Sleep
'	Else
'		aa.add_before_prev("[" & i & "before]") ': Cls :? Mid(*aa._ZData,1,aa._ZData_Size) :Sleep
'	EndIf
'Next
': Cls :? Mid(*aa._ZData,1,aa._ZData_Size) :Sleep
'Sleep

