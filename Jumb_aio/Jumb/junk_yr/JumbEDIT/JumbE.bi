#Define IDD_DIALOG			1000
#Define IDC_RAEDIT			1001
#Define IDC_SBR1				1002
#Define IDC_TBR1 1003

#Define IDM_MENU				10000
#Define IDM_FILE_EXIT		10001
#Define IDM_FILE_OPEN		10002
#Define IDM_FILE_SAVE		10003
#Define IDM_FILE_NEW			10004
#Define IDM_HELP_ABOUT		10101
#Define IDM_COMPILENRUN    10005
#Define IDM_RUNHELP 1006
#Define IDM_RUNIT 10007

'#Define IDC_NEW 1052
'#Define IDC_OPEN 1053
'#Define IDC_SAVE 1054
'#Define IDC_COMPILE 1055
'#Define IDC_RUN 1056
'#Define IDC_GO 1057

Dim Shared As ZString*500 abt_text = "===================="+ _
"===================="+ _
!"==============\r\nHell"+ _
"o Friends thanks for"+ _
" trying out Jumb and"+ _
!" JumbEDIT.\r\nJumb is "+ _
"an easy language,eas"+ _
"y syntax hope you li"+ _
!"ke it.\r\n\r\ncontact me"+ _
!":\r\nwww.jadogg.phatco"+ _
!"de.net\r\njadogg.coder"+ _
!"@gmail.com\r\n========"+ _
"===================="+ _
"===================="+ _
!"======\r\n"

Dim Shared hInstance As HMODULE
Dim Shared CommandLine As ZString Ptr
Dim Shared CommandLine1 As string
Dim Shared hLib As HMODULE
Dim Shared hWnd As HWND
Dim Shared hREd As HWND
Dim Shared As String foldername

Const ClassName="dlgclass"
Const AppName="JumbEDIT"

Const szLib="RAEdit.dll"


Dim Shared szFileName As ZString*1260
Dim Shared szFileNamet As ZString*1260
Dim Shared As String EXEFILEPATHX
Dim Shared As String DTTEMP

' Block definitions. max 10 block definitions
Dim Shared bd(9) As RABLOCKDEF
Dim szSt(9) As ZString*64
Dim szEn(9) As ZString*64

' Comment blocks
'bd(0).lpszStart=@szSt(0)
'bd(0).lpszEnd=@szEn(0)
'szSt(0)="/'"
'szEn(0)="'/"
'bd(0).flag=BD_INCLUDELAST Or BD_COMMENTBLOCK
' Function
bd(0).lpszStart=@szSt(0)
bd(0).lpszEnd=@szEn(0)
szSt(0)="LOOP $"
szEn(0)="ENDLOOP"
bd(0).flag=BD_INCLUDELAST

bd(1).lpszStart=@szSt(1)
bd(1).lpszEnd=@szEn(1)
szSt(1)="SUB $"
szEn(1)="ENDSUB"
bd(1).flag=BD_INCLUDELAST Or BD_DIVIDERLINE
' Sub
bd(2).lpszStart=@szSt(2)
bd(2).lpszEnd=@szEn(2)
szSt(2)="WIN $"
szEn(2)="ENDWIN $"
bd(2).flag=BD_INCLUDELAST Or BD_DIVIDERLINE
