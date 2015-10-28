''--------------------------------------------------------------------------
Function on_paint(ByVal hwnd As HWND,ByVal wparam As WPARAM,ByVal lparam As LPARAM) As Integer

	Dim rct As RECT
	Dim pnt As PAINTSTRUCT
	Dim hDC As HDC

	'draw the gfx buffer to screen
	hDC = BeginPaint(hWnd, @pnt)
	GetClientRect( hWnd, @rct )
	With rct
		StretchDIBits hDC, 0, 0,.Right-.Left+1,.bottom-.top+1, 0, 0, .Right-.Left+1,_
		.bottom-.top+1,ScreenPtr,CPtr(bitmapinfo Ptr, @bmi), DIB_RGB_COLORS, SRCCOPY
	End With

	EndPaint hWnd, @pnt

	Function = 0

End Function

Function on_Create(ByVal hwnd As HWND,ByVal wparam As WPARAM,ByVal lparam As LPARAM) As Integer
	Dim mywin As RECT
	'set a gfxscreen of the size of the client area
	GetClientRect( hWnd, @mywin)
	ScreenRes mywin.right+1,mywin.bottom+1, 32, 1, GFX_NULL
	'and create a bmp header,required to paint it yo screen
	With bmi
		.bV4Size = Len(BITMAPV4HEADER)
		.bv4width=mywin.right+1
		.bv4height=-(mywin.bottom+1)   'negative value=>top to bottom bmp
		'(standard BMP's are bottom to top)
		.bv4planes=  1
		.bv4bitcount=32
		.bv4v4compression=0
		.bv4sizeimage=mywin.right+1*mywin.bottom+1*4
		.bV4RedMask = &h0F00
		.bV4GreenMask = &h00F0
		.bV4BlueMask = &h000F
		.bV4AlphaMask = &hF000
	End With

	Function = 0

End Function

Function on_Destroy(ByVal hwnd As HWND,ByVal wparam As WPARAM,ByVal lparam As LPARAM) As Integer
	'clear arrays....
	PostQuitMessage( 0 )

	Function = 0

End Function

''
''----------------------------------------------------------------------------
Function WndProc ( ByVal hWnd As HWND,ByVal message As UINT, _
	ByVal wParam As WPARAM,ByVal lParam As LPARAM ) As LRESULT
	Dim As Integer id


	Function = 0

	Select Case As Const  message
		Case WM_CREATE
			SetTimer(hWnd,200,10,Cast(Any Ptr,@TimerProc))
			Function = on_Create(hwnd,wparam,lparam)
		Case WM_PAINT
			Function = on_paint(hwnd,wparam,lparam)
		Case WM_DESTROY
			Function = on_Destroy(hwnd,wparam,lparam)
		Case WM_MOUSEMOVE
			
			mouse.x = LoWord(lParam)
			mouse.y = HiWord(lParam)
			
		Case WM_LBUTTONUP
			
			mouse.x = LoWord(lParam)
			mouse.y = HiWord(lParam)
			'left click
		Case WM_RBUTTONUP
			
			mouse.x = LoWord(lParam)
			mouse.y = HiWord(lParam)

			ClientToScreen( hWnd, @mouse )

			id = TrackPopupMenu( g_hMenuPop,TPM_LEFTALIGN Or  TPM_RETURNCMD Or  TPM_TOPALIGN Or    TPM_HORPOSANIMATION, mouse.x, mouse.y,0, hWnd, 0 )

			Select Case id
				Case IDM_DELETE_OBJECT
					'delete
				Case IDM_EDIT_OBJECT
					'edit
			End Select

		Case Else
			Function = DefWindowProc( hWnd, message, wParam, lParam )
	End Select

End Function

Sub TimerProc(ByVal hWin As HWND,ByVal uMsg As UINT,ByVal wParam As WPARAM,ByVal lParam As LPARAM)
	Static As Integer selected_obj
	Cls
	paint_objects
	paint_lines
	If GetActiveWindow <> hWin Then Exit Sub

	selected_obj = listbox_CTRL.lbgetitemdata(listbox_CTRL.lbSelItem )

	If (GetKeyState(VK_LEFT)  And &H80)<>0 Then JUMBOBJZ(selected_obj).x -= 4
	If (GetKeyState(VK_RIGHT) And &H80)<>0 Then JUMBOBJZ(selected_obj).x += 4
	If (GetKeyState(VK_UP)  And &H80)<>0 Then JUMBOBJZ(selected_obj).y -= 4
	If (GetKeyState(VK_DOWN) And &H80)<>0 Then JUMBOBJZ(selected_obj).y += 4

End Sub
