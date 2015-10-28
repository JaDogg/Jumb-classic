'Put (Player.x,Player.y),Player.img,Trans
#define IDM_DELETE_OBJECT  1001
#define IDM_EDIT_OBJECT  1002

#define TPM_HORPOSANIMATION &h0400
#define TPM_HORNEGANIMATION &h0800
#define TPM_VERPOSANIMATION &h1000
#define TPM_VERNEGANIMATION &h2000
#define TPM_NOANIMATION     &h4000

#Include Once "windows.bi"
#Include Once "win/commctrl.bi"
#Include Once "win/commdlg.bi"
#Include Once "win/shellapi.bi"
#Include "fbgfx.bi"

#Include "const.bi"

Using fb
#Include "ssgui.bas"
Enum jumb_obj_typ
'{
	jumb_o_NOTHING = 0
	jumb_o_BEGIN
	jumb_o_END
	jumb_o_FNC
	jumb_o_IF
	jumb_o_VAR
	jumb_o_ASSIGN
	jumb_o_COND
	jumb_o_ASM
	jumb_o_ELSE1
	jumb_o_ELSE2
	jumb_o_ELSE3
	jumb_o_ELSE4
	jumb_o_ELSE5

'}
End Enum

Type bitmap_header Field = 1
	bfType          As UShort
	bfsize          As UInteger
	bfReserved1     As UShort
	bfReserved2     As UShort
	bfOffBits       As UInteger
	biSize          As UInteger
	biWidth         As UInteger
	biHeight        As UInteger
	biPlanes        As UShort
	biBitCount      As UShort
	biCompression   As UInteger
	biSizeImage     As UInteger
	biXPelsPerMeter As UInteger
	biYPelsPerMeter As UInteger
	biClrUsed       As UInteger
	biClrImportant  As UInteger
End Type

Type JUMBObject

	objtype As jumb_obj_typ
	objname As ZString * 255
	objprintname As ZString * 255

	index As Integer
	is_line As Integer

	x As Integer
	y As Integer

	'to
	x2 As Integer
	y2 As Integer

	img As Any Ptr
	h As Byte
	w As Byte


	nFrom As Integer
	nTo As Integer

	nod1 As Integer
	nod2 As Integer
	nod3 As Integer
	'here goes other data
	zdata1 As ZString * 255
	zdata2 As ZString * 255
	zdata3 As ZString * 255
	'HE HE
	ASMCODE As ZString * 4000

	idata1 As Integer
	idata2 As Integer
	idata3 As Integer
	idata4 As Integer

	tmp As UInteger

End Type

Dim Shared hInstance As HMODULE
Dim Shared As Any Ptr jumb_main_h,jumb_toolbox_h,jumb_obj_h,jumb_options_h
Dim Shared CommandLine As ZString Ptr
Dim Shared As HWND hWnd,TOOLBOX_HWND,OBJS_HWND,OP_DLG_HWND
Dim Shared bmi As bitmapv4header
Dim Shared JUMBOBJZ() As JUMBObject
Dim Shared As Integer JUMBOBJ_CNT
Dim Shared As ControlJ listbox_CTRL
Dim Shared As HMENU g_hMenuPop

'---
'INPUT BOX
Dim Shared As Integer INP_IS_OK,INP_IS_VAR,INP_ALL_VARS
Dim Shared As String INP_CONST,INP_VAR,INP_TITLE
'ASMCODE BOX
Dim Shared As Integer JASM_IS_OK
Dim Shared As String JASM_DATA
'---
Declare Sub TimerProc(ByVal hWin As HWND,ByVal uMsg As UINT,ByVal wParam As WPARAM,ByVal lParam As LPARAM)

#Include "tools.bas"


Function PROC_OPDLG(ByVal hWin As HWND, ByVal uMsg As UINT, ByVal wParam As WPARAM, ByVal lParam As LPARAM) As Integer
	Dim As Long id, nEvent

	Select Case uMsg
		Case WM_INITDIALOG
			'
		Case WM_COMMAND
			id=LoWord(wParam)
			nEvent=HiWord(wParam)
			Select Case nEvent
				Case BN_CLICKED
					Select Case id
						Case OPJUMBCOMPILE
							'compile it mann
					End Select
					'
			End Select
			'
		Case WM_CLOSE
			'EndDialog(hWin, 0)
			'
		Case WM_SIZE
			'
		Case Else
			Return FALSE
			'
	End Select
	Return TRUE

End Function


Function PROC_JUMBASM(ByVal hWin As HWND, ByVal uMsg As UINT, ByVal wParam As WPARAM, ByVal lParam As LPARAM) As Integer
	Dim As Long id, nEvent
	Static As ControlJ ED_ASMCODE

	Select Case uMsg
		Case WM_INITDIALOG
			JASM_IS_OK = 0
			ED_ASMCODE.hWnd = GetDlgItem(hWin,EDJUMBASMCODE)
		Case WM_COMMAND
			id=LoWord(wParam)
			nEvent=HiWord(wParam)
			Select Case nEvent
				Case BN_CLICKED
					Select Case id
						Case JUMBASMOK
							'OK
							JASM_IS_OK = 1
							JASM_DATA = ED_ASMCODE.Text
							If JASM_DATA = "" Then
								JASM_IS_OK = 0
							Else
								EndDialog(hWin, 0)
							EndIf
					End Select
					'
			End Select
			'
		Case WM_CLOSE
			JASM_IS_OK = 0
			EndDialog(hWin, 0)
			'
		Case WM_SIZE
			'
		Case Else
			Return FALSE
			'
	End Select
	Return TRUE

End Function
Function PROC_INPDLG(ByVal hWin As HWND, ByVal uMsg As UINT, ByVal wParam As WPARAM, ByVal lParam As LPARAM) As Integer
	Dim As Long id, nEvent
	Static As ControlJ ED_CONST_J
	Select Case uMsg
		Case WM_INITDIALOG
			'
			INP_IS_OK = 0
			ED_CONST_J.hWnd = GetDlgItem(hWin,EDINPCONST)
		Case WM_COMMAND
			id=LoWord(wParam)
			nEvent=HiWord(wParam)
			Select Case nEvent
				Case BN_CLICKED
					Select Case id
						Case INPOK
							INP_IS_OK  = 1
							INP_CONST  = ED_CONST_J.Text
							EndDialog(hWin, 0)
					End Select
					'
			End Select
			'
		Case WM_CLOSE
			INP_IS_OK = 0
			EndDialog(hWin, 0)
			'
		Case WM_SIZE
			'
		Case Else
			Return FALSE
			'
	End Select
	Return TRUE

End Function


Sub callinputbox(hWin As HWND)
	DialogBoxParam(hInstance,Cast(ZString Ptr,INPUTDLGBOX),hWin,@PROC_INPDLG,0)
End Sub
Sub callasminputbox(hWin As HWND)
	DialogBoxParam(hInstance,Cast(ZString Ptr,ASMDLGBOX),hWin,@PROC_JUMBASM,0)
	'JUMBOPDLG
End Sub
Function PROC_TB(ByVal hWin As HWND, ByVal uMsg As UINT, ByVal wParam As WPARAM, ByVal lParam As LPARAM) As Integer
	Dim As Long id, nEvent

	Select Case uMsg
		Case WM_INITDIALOG
			'
			TOOLBOX_HWND = hWin
		Case WM_COMMAND
			id=LoWord(wParam)
			nEvent=HiWord(wParam)
			If lParam Then
				' Control events
				Select Case nEvent
					Case BN_CLICKED
						Select Case id
							Case TBPAUSE
								add_object("FNC_PAUSE","F:Pause",jumb_o_FNC)
							Case TBBEGIN
								add_object("BEGIN","Begin",jumb_o_BEGIN)
								EnableWindow(GetDlgItem(hWin,TBBEGIN),FALSE)
							Case TBEND
								add_object("END","End",jumb_o_END)
							Case TBASM
								'ASM
								callasminputbox(hwin)
								If JASM_IS_OK = 1 Then
									'ok now add it
									add_object("ASM","Asm Code" ,jumb_o_ASM)
									JUMBOBJZ(JUMBOBJ_CNT).ASMCODE = JASM_DATA
								EndIf
							Case TBECHO
								'echo hmm ask for the input now
								callinputbox(hwin)
								If INP_IS_OK = 1 Then
									'ok now add it
									add_object("FNC_ECHO","F:Echo ('" & Left(INP_CONST,3) & "...')" ,jumb_o_FNC)
									JUMBOBJZ(JUMBOBJ_CNT).zdata1 = INP_CONST
								EndIf
						End Select
						'
				End Select
			Else

			EndIf
			'
		Case WM_CLOSE
			'EndDialog(hWin, 0)
			'
		Case WM_SIZE
			'
		Case Else
			Return FALSE
			'
	End Select
	Return TRUE

End Function


Function PROC_OBJS(ByVal hWin As HWND, ByVal uMsg As UINT, ByVal wParam As WPARAM, ByVal lParam As LPARAM) As Integer
	Dim As Long id, nEvent
	Static As Integer Is_link_begined,nFrom

	Select Case uMsg
		Case WM_INITDIALOG
			'
			OBJS_HWND = hWin
			listbox_CTRL.hWnd = GetDlgItem(hWin,LBOXOBJ)
		Case WM_COMMAND
			id=LoWord(wParam)
			nEvent=HiWord(wParam)
			Select Case nEvent
				Case BN_CLICKED
					Select Case id
						Case OBJECTJOIN
							If Is_link_begined = 0 Then
								Is_link_begined = 1
								If listbox_CTRL.lbSelItem <> LB_ERR Then
									nFrom = listbox_CTRL.lbgetitemdata(listbox_CTRL.lbSelItem)
								Else
									Is_link_begined = 0
								EndIf
							Else
								If listbox_CTRL.lbSelItem <> LB_ERR Then
									add_line(nFrom,listbox_CTRL.lbgetitemdata(listbox_CTRL.lbSelItem))
									Is_link_begined = 0
								Else
									'nothing
								EndIf

							EndIf

					End Select
					'
			End Select
			'
		Case WM_CLOSE
			'EndDialog(hWin, 0)
			'
		Case WM_SIZE
			'
		Case Else
			Return FALSE
			'
	End Select
	Return TRUE

End Function
'here goes the stuff of main dlg
#Include "main_dlg.bas"
Sub init_jumb_main(param As Any Ptr)

	Dim wMsg As MSG
	Dim wcls As WNDCLASS
	Dim szAppName As ZString * 30 => "JUMB"
	Dim i As Integer

	With wcls
		.style         = CS_HREDRAW Or CS_VREDRAW
		.lpfnWndProc   = @WndProc
		.cbClsExtra    = 0
		.cbWndExtra    = 0
		.hInstance     = GetModuleHandle( NULL )
		.hIcon         = LoadIcon( .hInstance, Cast(ZString Ptr,100) )
		.hCursor       = LoadCursor( NULL, IDC_ARROW )
		.hbrBackground = GetStockObject(WHITE_BRUSH )
		.lpszMenuName  = NULL
		.lpszClassName = @szAppName
	End With

	If( RegisterClass( @wcls ) = FALSE ) Then
		End
	End If

	'make a non-resizable screen 10C81000
	hWnd = CreateWindowEx(WS_EX_TOPMOST ,szAppName,"Jumb:",WS_OVERLAPPEDWINDOW And Not (WS_SIZEBOX Or WS_MAXIMIZEBOX),	CW_USEDEFAULT,CW_USEDEFAULT,CW_USEDEFAULT, CW_USEDEFAULT, 	NULL,NULL, wcls.hinstance,NULL )

	ShowWindow( hWnd, SW_NORMAL )
	UpdateWindow( hWnd )

	Randomize Timer
	Color RGB(255, 128, 0),RGB(0,0,75)
	Cls

	While 1

		If PeekMessage( @wMsg, NULL, 0,0, PM_REMOVE) Then
			'If IsDialogMessage(hWnd,@wMsg) Then
			If wmsg.message=WM_QUIT Then

				Exit While

			End If
			TranslateMessage( @wMsg )
			DispatchMessage( @wMsg )

		Else
			RedrawWindow (hWnd,0,0,RDW_INVALIDATE)
			Sleep 10
		End If
	Wend

	End

	ExitProcess(0)
End Sub
Sub init_jumb_toolbox(param As Any Ptr)
	DialogBoxParam(hInstance,Cast(ZString Ptr,JUMBTBOX),0,@PROC_TB,0)
End Sub
Sub init_jumb_objects(param As Any Ptr)
	DialogBoxParam(hInstance,Cast(ZString Ptr,OBJECTSDLG),0,@PROC_OBJS,0)
End Sub
Sub init_jumb_options(param As Any Ptr)
	DialogBoxParam(hInstance,Cast(ZString Ptr,JUMBOPDLG),0,@PROC_OPDLG,0)
End Sub

hInstance=GetModuleHandle(NULL)
CommandLine=GetCommandLine
InitCommonControls

jumb_options_h = ThreadCreate(@init_jumb_options)
jumb_toolbox_h = ThreadCreate(@init_jumb_toolbox)
jumb_obj_h     = ThreadCreate(@init_jumb_objects)
jumb_main_h    = ThreadCreate(@init_jumb_main)

g_hMenuPop = CreatePopupMenu()

AppendMenu( g_hMenuPop, MF_STRING, IDM_DELETE_OBJECT, "&Delete Object" )
AppendMenu( g_hMenuPop, MF_STRING, IDM_EDIT_OBJECT, "&Edit" )


ThreadWait(jumb_main_h)
ThreadWait(jumb_toolbox_h)
ThreadWait(jumb_obj_h)
ThreadWait(jumb_options_h)


