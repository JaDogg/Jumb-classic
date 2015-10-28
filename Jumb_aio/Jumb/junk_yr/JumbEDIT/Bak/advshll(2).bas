#Include Once "windows.bi"
/'
this code is based on [FBEdit src code] and [Iczelion's Win32 Assembly Tutorial part 21]
'/
'Declare Sub(cmd As String,show As Integer,ByRef iexitcode As Integer = 0)
Sub advanceshell(cmd As String,show As Integer = SW_SHOW,ByRef iexitcode As Integer = 0)

	Dim sat        As   SECURITY_ATTRIBUTES
	Dim startinfo  As   STARTUPINFO
	Dim hwritepipe As   HANDLE
	Dim hreadpipe  As   HANDLE
	Dim pinfo      As   PROCESS_INFORMATION
	Dim szAppName  As   String   = "JaDogg" ' app name
	Dim buffer     As   ZString Ptr
	Dim odata      As   String
	'   Dim Rtn        As   Dword
	'   Dim bread      As   Dword
	'   Dim sData      As   String
	'   Dim rd         As   ZString*32
	Dim bytesRead  As   dword
	Dim ExitCode   As   dword
	Dim readout    As   dword
	   '? cmd
	If cmd = "" Then Exit sub '

	sat.lpSecurityDescriptor = NULL
	sat.bInheritHandle       = TRUE
	sat.nLength              = SizeOf(SECURITY_ATTRIBUTES)

	If CreatePipe(@hreadpipe,@hwritepipe,@sat,NULL) = NULL Then

		MessageBox(NULL,StrPtr("CreatePipe failed"),StrPtr(szAppName),MB_OK Or MB_ICONERROR)

	Else
		startinfo.cb             = SizeOf(STARTUPINFO)
		GetStartupInfo(@startinfo)
		startinfo.dwFlags        = STARTF_USESHOWWINDOW + STARTF_USESTDHANDLES
		startinfo.wShowWindow    = show
		startinfo.hStdOutput     = hwritepipe
		'      startinfo.hStdInput      = hreadpipe
		startinfo.hStdError      = hwritepipe


		If CreateProcess(NULL,StrPtr(cmd),NULL,NULL,TRUE,NULL,NULL,StrPtr(foldername),@startinfo,@pinfo)=0 Then
			CloseHandle(hreadpipe)
			CloseHandle(hwritepipe)
			MessageBox(NULL,StrPtr("CreateProcess failed"),StrPtr(szAppName),MB_OK Or MB_ICONERROR)

		Else
			'allocating 1024 is not good sometimes there is unwanted chars
			'so allocating 2 is the best idea
			buffer = Allocate(2)
			CloseHandle(hwritepipe)
			While TRUE
				RtlZeroMemory(buffer,2)
				readout = ReadFile(hreadpipe,buffer,1,@bytesRead,NULL)
				If readout = NULL Then Exit While
				odata += (*buffer)
			Wend
			CloseHandle(hreadpipe)
			GetExitCodeProcess(pinfo.hProcess,@ExitCode)
		EndIf

	EndIf
	iexitcode = ExitCode
	'Return odata
End Sub

'Sub advanceshell_nowait(cmd As String,show As Integer = SW_SHOW)
'
'
'	If cmd = "" Then Exit sub '
'   ShellExecute(hWnd,"Open",StrPtr(cmd),"","",show)
'
'End Sub
