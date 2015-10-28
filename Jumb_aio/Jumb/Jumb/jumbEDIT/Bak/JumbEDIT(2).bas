
'BASED ON KetilO'S EXAMPLE RAEDITTEST
#Include Once "windows.bi"
#Include Once "win/commctrl.bi"
#Include Once "win/commdlg.bi"
#Include Once "win/shellapi.bi"
#Include Once "win/richedit.bi"
#Include Once "file.bi"
#Include "reglib.bas"

#Include "RAEdit.bi"
#Include "JumbE.bi"
#Include "advshll.bas"
#Include "FileIO.bas"
#Include "Misc.bas"

#Define BAS_FILE "JumbEDITJumFile"
#Define BAS_FILE_D "Jumb Source File"

Function strpslash( PATHX As String )  As String
	'' REMOVE ANY LAST \ /
	'' YOU MAY NEED THIS
	If Not Len( PATHX )   = 0 Then
		If Right( PATHX,1 )   = "\" Or Right( PATHX,1 )   = "/" Then
			Return Left( PATHX,Len( PATHX )  -1 )
		Else
			Return PATHX
		End If
	Else
		Return ""
	End If
End Function
Function getfilenameonly( PATHANDFILE As String )  As String
	Dim As Integer A
	A = InStrRev( PATHANDFILE,"\" )
	Return Mid( PATHANDFILE,A+1 )
End Function
Sub _cmd_load()
	Dim As String CL
	Dim As Integer _pos
	CL = *GetCommandLine & "   "
	If CL[0] = Asc("""") Then
		_pos = InStr(2,CL,"""")
	Else
		_pos = InStr(CL," ")
	EndIf
	CL = Trim(Trim(Mid(CL,_pos+1)),"""")
	If Len(CL) Then

		ReadTheFile(hREd,StrPtr(CL))
	EndIf

End Sub
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
Function WndProc(ByVal hWin As HWND,ByVal uMsg As UINT,ByVal wParam As WPARAM,ByVal lParam As LPARAM) As Integer
	Dim rect As RECT
	Dim buff As ZString*260
	Dim bm As Integer


	Select Case uMsg
		Case WM_CREATE
			ShowWindow(hWin,SW_MAXIMIZE)



		Case WM_INITDIALOG
			hWnd=hWin
			hREd=GetDlgItem(hWin,IDC_RAEDIT)
			_cmd_load
			SetFonts(hREd)
			SetColors(hREd)
			SetCharTab(hREd)
			SetBlocks(hREd)
			SetHighlightWords(hREd)
			SetFocus(hREd)
			'

		Case WM_COMMAND
			Select Case HiWord(wParam)
				Case BN_CLICKED,1
					Select Case LoWord(wParam)
						Case IDM_FILE_OPEN
							If WantToSave(hREd) Then
								OpenAFile(hREd)
							EndIf
							SetFocus(hREd)
						Case 10006
							'reg file ext

							Dim regBasName  As RegLib=RegLib(HKEY_CLASSES_ROOT,".jum")
							Dim regBasClass As RegLib=RegLib(HKEY_CLASSES_ROOT,BAS_FILE)
							Dim regBasIcon  As RegLib=RegLib(HKEY_CLASSES_ROOT,BAS_FILE+"\DefaultIcon")
							Dim regBasCmd   As RegLib=RegLib(HKEY_CLASSES_ROOT,BAS_FILE+"\shell\open\command")

							regBasName.createMe()
							regBasClass.createMe()
							regBasIcon.createMe()
							regBasCmd.createMe()
							regBasName.default=BAS_FILE
							regBasClass.default=BAS_FILE_D

							Dim Cmd As String=Chr(34)+strpslash(ExePath)+"\JumbEDIT.exe"+Chr(34)+" "+Chr(34)+"%1"+Chr(34)
							Dim basIco As String=Chr(34)+strpslash(ExePath)+"\JumbEDIT.exe"+Chr(34)+",1"

							regBasIcon.default=basIco
							regBasCmd.default=Cmd

						Case IDM_FILE_SAVE
							SaveAFile(hREd,@szFileName)
							SetFocus(hREd)

						Case IDM_FILE_NEW
							If WantToSave(hREd) Then
								' Clear current text
								SendMessage(hREd,WM_SETTEXT,0,Cast(LPARAM,StrPtr("")))
								' Set modified to FALSE
								SendMessage(hREd,EM_SETMODIFY,FALSE,0)
								szFileName=""
							EndIf
							SetFocus(hREd)
							'
						Case IDM_FILE_EXIT
							SendMessage(hWin,WM_CLOSE,0,0)

						Case IDM_HELP_ABOUT
							MessageBox(hWin,@abt_text,@AppName,MB_OK)

							SetFocus(hREd)

						Case IDM_COMPILENRUN
							'we need to compile ha ?!!
							If WantToSave(hREd) Then

								Dim As String _code_a_buff,_tmp_comp,_tmp_comp2
								Dim As Integer outf,inpf

								_tmp_comp = szFileName
								_tmp_comp2 = Trim(_tmp_comp,"""")

								If Len(_tmp_comp2) Then

									_code_a_buff = Space(FileLen(strpslash(ExePath) & "\jumbmain.asm"))
									inpf = FreeFile

									Open (strpslash(ExePath) & "\jumbmain.asm") For Binary As #inpf
									Get #inpf,,_code_a_buff
									Close #inpf
									' ";(**Replace0**)" = gui,console
									' ";(**Replace1**)" = win32
									' ";(**Replace2**)" = jumb asm
									' ";(**Replace3**)" = res
									'section 'Resource' data readable resource from "E:\Jumb\JumbC\f.Res"

									_tmp_comp2 = Left(_tmp_comp2,Len(_tmp_comp2) - 4)

									'compile

									Kill (_tmp_comp2 & "_0_.asm")

									advanceshell(strpslash(ExePath) & "\JumbC.exe " & _tmp_comp2 ,SW_HIDE)
									
									If Want_it_console(hWin) Then
										_code_a_buff = replace(_code_a_buff, ";(**Replace0**)" , "format PE")
									Else
										_code_a_buff = replace(_code_a_buff, ";(**Replace0**)" , "format PE GUI 4.0")
									EndIf
									'"include '"
									_code_a_buff = replace(_code_a_buff, ";(**Replace1**)" , "include '" & strpslash(ExePath) & "\fasm\INCLUDE\win32ax.inc'")
									_code_a_buff = replace(_code_a_buff, ";(**Replace2**)" , "include '" & _tmp_comp2 & "_0_.asm'")
									'_0_
									outf = FreeFile

									Kill (_tmp_comp2 & ".asm")

									Open _tmp_comp2 & ".asm" For Binary As #outf
									Put #outf,,_code_a_buff
									Close #outf
									? strpslash(ExePath) & "\fasm\fasm.exe " & _tmp_comp2 & ".asm"
									advanceshell(strpslash(ExePath) & "\fasm\fasm.exe " & _tmp_comp2 & ".asm" ,SW_HIDE)
									_tmp_comp = _tmp_comp2 & ".exe"
									ShellExecute(hWin,"Open",StrPtr(_tmp_comp2),StrPtr(""),StrPtr(""),SW_SHOWNORMAL)
									'read in the jumbmain.asm
									'replace strz
									'run fasm
									'run the exe
								EndIf
							EndIf

					End Select
					'
			End Select
			'
		Case WM_SIZE
			GetClientRect(hWin,@rect)
			MoveWindow(GetDlgItem(hWin,IDC_SBR1),0,0,0,0,TRUE)
			MoveWindow(hREd,0,0,rect.right,rect.bottom-21,TRUE)
			'
		Case WM_NOTIFY
			If Cast(NMHDR Ptr,lParam)->hwndFrom=hREd Then
				If Cast(RASELCHANGE Ptr,lParam)->seltyp=SEL_OBJECT Then
					' Bookmark clicked
					bm=SendMessage(hREd,REM_GETBOOKMARK,Cast(RASELCHANGE Ptr,lParam)->Line,0)
					If bm=1 Then
						' Collapse
						SendMessage(hREd,REM_COLLAPSE,Cast(RASELCHANGE Ptr,lParam)->Line,0)
					ElseIf bm=2 Then
						' Expand
						SendMessage(hREd,REM_EXPAND,Cast(RASELCHANGE Ptr,lParam)->Line,0)
					EndIf
				Else
					' Selection changed
					buff="Line: " & Str(Cast(RASELCHANGE Ptr,lParam)->Line+1) & " Pos: " & Str(Cast(RASELCHANGE Ptr,lParam)->chrg.cpMin-Cast(RASELCHANGE Ptr,lParam)->cpLine+1) & "	 Time: " & Time
					SendDlgItemMessage(hWin,IDC_SBR1,SB_SETTEXT,0,Cast(LPARAM,@buff))
					If Cast(RASELCHANGE Ptr,lParam)->fchanged Then
						' Update comment blocks
						'SendMessage(hREd,REM_SETCOMMENTBLOCKS,Cast(WPARAM,StrPtr("/'")),Cast(LPARAM,StrPtr("'/")))
						' Update block bookmarks
						SendMessage(hREd,REM_SETBLOCKS,0,0)
					EndIf
				EndIf
			EndIf
		Case WM_CLOSE
			If WantToSave(hREd) Then
				DestroyWindow(hWin)
			EndIf
			'
		Case WM_DESTROY
			PostQuitMessage(NULL)
			'
		Case Else
			Return DefWindowProc(hWin,uMsg,wParam,lParam)
			'
	End Select
	Return 0

End Function

Function WinMain(ByVal hInst As HINSTANCE,ByVal hPrevInst As HINSTANCE,ByVal CmdLine As ZString Ptr,ByVal CmdShow As Integer) As Integer
	Dim wc As WNDCLASSEX
	Dim msg As MSG

	' Setup and register class for dialog

	wc.cbSize=SizeOf(WNDCLASSEX)
	wc.style=CS_HREDRAW Or CS_VREDRAW
	wc.lpfnWndProc=@WndProc
	wc.cbClsExtra=0
	wc.hIcon = LoadIcon(hINSTANCE,Cast(ZString Ptr,100))
	wc.cbWndExtra=DLGWINDOWEXTRA
	wc.hInstance=hInst
	wc.hbrBackground=Cast(HBRUSH,COLOR_BTNFACE+1)
	wc.lpszMenuName=Cast(ZString Ptr,IDM_MENU)
	wc.lpszClassName=@ClassName
	'wc.hIcon=LoadIcon(NULL,IDI_APPLICATION)
	'wc.hIconSm=wc.hIcon
	wc.hCursor=LoadCursor(NULL,IDC_ARROW)
	RegisterClassEx(@wc)
	' Create and show the dialog
	CreateDialogParam(hInstance,Cast(ZString Ptr,IDD_DIALOG),NULL,@WndProc,NULL)
	ShowWindow(hWnd,SW_SHOWNORMAL)
	UpdateWindow(hWnd)
	' Message loop
	Do While GetMessage(@msg,NULL,0,0)
		TranslateMessage(@msg)
		DispatchMessage(@msg)
	Loop
	Return msg.wParam
	'v
End Function

' Program start
hInstance=GetModuleHandle(NULL)
CommandLine=GetCommandLine
'? *CommandLine

EXEFILEPATHX  = ExePath
foldername = ExePath
InitCommonControls
hLib=LoadLibrary(@szLib)
If hLib Then
	If FileLen(strpslash(ExePath) & "\jumbmain.asm") > 0 Then
		WinMain(hInstance,NULL,CommandLine,SW_SHOWDEFAULT)
		FreeLibrary(hLib)
	Else
		MessageBox(0,"jumbmain.asm not found",@AppName,MB_OK)
	EndIf
Else
	MessageBox(0,"RAEdit.dll not found",@AppName,MB_OK)
EndIf
ExitProcess(0)

End
