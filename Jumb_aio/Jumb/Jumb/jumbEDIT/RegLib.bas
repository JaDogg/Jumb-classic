#Include Once "windows.bi"
#Include Once "win/winreg.bi"

''{ RegLib FBEdit Code Block
#Define REG_KEY_NOT_EXIST "REG_KEY_NOT_EXIST"
#Define REG_KEY_INVALID "REG_KEY_INVALID"

Type RegLib
	Declare Constructor(root As HKEY,key As String, create As Boolean=FALSE)
	Declare Destructor()
	Declare Function createMe() As Boolean
	Declare Function deleteMe() As Boolean
	Declare Function getValue(v As String) As String
	Declare Function setValue(v As String, s As String) As Boolean
	Declare Property default() As String
	Declare Property default(s As String)
	exists As Boolean
	Private:
	hroot  As HKEY
	rkey   As HKEY
	key    As String
End Type

Constructor RegLib(root As HKEY,key As String, create As Boolean=FALSE)
this.hroot=root
this.key=key
If RegOpenKeyEx(root, StrPtr(key), 0, KEY_ALL_ACCESS, @this.rkey) <> ERROR_SUCCESS Then
	If create Then
		createMe()
	Else
		exists=FALSE
	EndIf
Else
	exists=TRUE
EndIf
End Constructor

Destructor RegLib()
RegCloseKey(rkey)
End Destructor

Function RegLib.createMe() As Boolean
	If RegCreateKeyEx(hroot,StrPtr(key),0,0,REG_OPTION_NON_VOLATILE,KEY_ALL_ACCESS,NULL,@rkey,0)=ERROR_SUCCESS Then
		exists=TRUE
	Else
		exists=FALSE
	EndIf
	Return exists
End Function

Function RegLib.deleteMe() As Boolean
	If RegDeleteKey(hroot,StrPtr(key)) = ERROR_SUCCESS Then
		exists=FALSE
		Return TRUE
	Else
		exists=TRUE
		Return FALSE
	End If
End Function

Function RegLib.getValue(v As String) As String
	If exists=FALSE Then Return REG_KEY_NOT_EXIST
	Dim out_ As ZString * 1024
	Dim l As DWORD = 512
	Dim t As DWORD
	If RegQueryValueEx(rkey, StrPtr(v), NULL, @t, @out_, @l) <> ERROR_SUCCESS Or (t <> REG_SZ And t <> REG_EXPAND_SZ) Then
		Return REG_KEY_INVALID
	EndIf
	out_[512 - 1] = 0
	Return out_
End Function

Function RegLib.setValue(v As String, s As String) As Boolean
	If exists=FALSE Then Return FALSE
	Return RegSetValueEx(rkey,StrPtr(v),0,REG_SZ,StrPtr(s),Len(s)+1)= ERROR_SUCCESS
End Function

Property RegLib.default() As String
Return getValue("")
End Property

Property RegLib.default(s As String)
setValue("",s)
End Property

''}
'
'/' Small test, File Association
'Dim regAssocName as RegLib=RegLib(HKEY_CLASSES_ROOT,".bas",true)
'regAssocName.default="FreeBasicFile"
'
'Dim regClass as RegLib=RegLib(HKEY_CLASSES_ROOT,"FreeBasicFile",true)
'regClass.default="FreeBasic Source"
'
'Dim regIcon as RegLib=RegLib(HKEY_CLASSES_ROOT,"FreeBasicFile\DefaultIcon",true)
'regIcon.default="user32.dll,5"
'
'Dim regCommand as RegLib=RegLib(HKEY_CLASSES_ROOT,"FreeBasicFile\shell\open\command",true)
'regCommand.default=Chr(34)+"notepad"+Chr(34)+" "+Chr(34)+"%1"+Chr(34)
'
'sleep
''/