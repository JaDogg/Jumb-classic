Sub game_obj_load_img(ByRef obj As JUMBObject,bmpfile As String)
	'this can get any bmp to the object.img
	'and sets correct w,h
	'---
	Dim As bitmap_header bmp_h

	'get w,h
	Open bmpfile For Binary As #1
	Get #1, , bmp_h
	Close #1
	'crt img
	obj.img = ImageCreate(bmp_h.biWidth,bmp_h.biHeight)
	obj.w   = bmp_h.biWidth
	obj.h   = bmp_h.biHeight
	' load
	BLoad     bmpfile,obj.img
End Sub
Sub add_in_list(i As Integer)

	listbox_CTRL.lbsetitemdata(listbox_CTRL.lbAdd(JUMBOBJZ(i).objprintname),i)

End Sub
Sub add_line(nFrom  As Integer,nTo  As Integer)
	JUMBOBJ_CNT+=1
	ReDim Preserve JUMBOBJZ(JUMBOBJ_CNT) As JUMBObject
	JUMBOBJZ(JUMBOBJ_CNT).is_line = 1
	JUMBOBJZ(JUMBOBJ_CNT).objprintname = "Connector " & nFrom & ":" & nTo
	JUMBOBJZ(JUMBOBJ_CNT).nFrom = nFrom

	JUMBOBJZ(JUMBOBJ_CNT).nTo = nTo
	add_in_list(JUMBOBJ_CNT)
End Sub
Sub add_object(objname As String,objprintname As String,objtype As jumb_obj_typ,a As Integer = 1 ,a1 As Integer =1)
	JUMBOBJ_CNT+=1
	ReDim Preserve JUMBOBJZ(JUMBOBJ_CNT) As JUMBObject
	JUMBOBJZ(JUMBOBJ_CNT).objname = objname
	JUMBOBJZ(JUMBOBJ_CNT).objprintname = objprintname
	JUMBOBJZ(JUMBOBJ_CNT).objtype = objtype
	JUMBOBJZ(JUMBOBJ_CNT).index = JUMBOBJ_CNT
	If objtype = jumb_o_BEGIN Then
		game_obj_load_img(JUMBOBJZ(JUMBOBJ_CNT),"bmps\jumb_code_begin.bmp")
	ElseIf objtype = jumb_o_END Then
		game_obj_load_img(JUMBOBJZ(JUMBOBJ_CNT),"bmps\jumb_code_end.bmp")
	ElseIf objtype = jumb_o_ASM Then
		game_obj_load_img(JUMBOBJZ(JUMBOBJ_CNT),"bmps\jumb_code_asm.bmp")
	ElseIf objtype = jumb_o_FNC Then
		If objname = "FNC_ECHO" Then
			game_obj_load_img(JUMBOBJZ(JUMBOBJ_CNT),"bmps\jumb_code_echo.bmp")
		ElseIf objname = "FNC_PAUSE" Then
			game_obj_load_img(JUMBOBJZ(JUMBOBJ_CNT),"bmps\jumb_code_pause.bmp")

		EndIf
	EndIf

	'add
	'now add the  obj in listbox
	add_in_list(JUMBOBJ_CNT)
End Sub
Sub paint_objects()
	For i As Integer = 1 To JUMBOBJ_CNT
		If JUMBOBJZ(i).is_line = 0 Then
			Put (JUMBOBJZ(i).x,JUMBOBJZ(i).y),JUMBOBJZ(i).img,Trans
		EndIf
	Next
End Sub
Sub paint_lines()
	Static As Integer x1,y1,x2,y2
	For i As Integer = 1 To JUMBOBJ_CNT
		If JUMBOBJZ(i).is_line = 1 Then
			x1 = JUMBOBJZ(JUMBOBJZ(i).nFrom).x + 8
			y1 = JUMBOBJZ(JUMBOBJZ(i).nFrom).y + 8
			x2 = JUMBOBJZ(JUMBOBJZ(i).nTo).x + 8
			y2 = JUMBOBJZ(JUMBOBJZ(i).nTo).y + 8
			If i = listbox_CTRL.lbgetitemdata(listbox_CTRL.lbSelItem) Then
				Line (x1,y1)-(x2,y2),  &HFF0000
			Else
				Line (x1,y1)-(x2,y2),  &HFFFFFF
			EndIf

			Circle (x2, y2), 6,&HFFFFFF, , , , F
			Circle (x1, y1), 3,&H0000FF, , , , F
		EndIf
	Next
End Sub
Sub moveobj(ByRef cFrom As JUMBObject,ByRef cTo As JUMBObject)

	cTo.objtype = cFrom.objtype
	cTo.objname = cFrom.objname
	cTo.objprintname = cFrom.objprintname

	cTo.index = cFrom.index
	cTo.x = cFrom.x
	cTo.y = cFrom.y
	cTo.x2 = cFrom.x2
	cTo.y2 = cFrom.y2
	cTo.is_line = cFrom.is_line
	cTo.img = cFrom.img
	cTo.h = cFrom.h
	cTo.w = cFrom.w
	cTo.nFrom = cFrom.nFrom
	cTo.nTo =  cFrom.nTo

	cTo.nod1 = cFrom.nod1
	cTo.nod2 = cFrom.nod2
	cTo.nod3 = cFrom.nod3

	cTo.zdata1 = cFrom.zdata1
	cTo.zdata2 = cFrom.zdata2
	cTo.zdata3 = cFrom.zdata3
	cTo.ASMCODE = cFrom.ASMCODE

	cTo.idata1 = cFrom.idata1
	cTo.idata2 = cFrom.idata2
	cTo.idata3 = cFrom.idata3
	cTo.idata4 = cFrom.idata4

	cTo.tmp = cFrom.tmp

	cFrom.objtype = jumb_o_NOTHING
	cFrom.objname = ""
	cFrom.objprintname = ""
	cFrom.zdata1 = ""
	cFrom.zdata2 = ""
	cFrom.zdata3 = ""
	cFrom.ASMCODE = ""

	cFrom.index = 0
	cFrom.x = 0
	cFrom.y = 0
	cFrom.x2 = 0
	cFrom.y2 = 0
	cFrom.is_line = 0
	cFrom.img = 0
	cFrom.h = 0
	cFrom.w = 0
	cFrom.nFrom = 0
	cFrom.nTo = 0

	cFrom.nod1 = 0
	cFrom.nod2 = 0
	cFrom.nod3 = 0

	cTo.idata1 = cFrom.idata1
	cTo.idata2 = cFrom.idata2
	cTo.idata3 = cFrom.idata3
	cTo.idata4 = cFrom.idata4

	cTo.tmp = cFrom.tmp
End Sub

