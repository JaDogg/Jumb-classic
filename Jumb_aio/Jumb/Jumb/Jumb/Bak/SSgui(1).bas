#Include Once "windows.bi"
#define WIN_INCLUDE_ALL
#Include Once "DIR.BI"
#Include Once "FILE.BI"
'this is known as Simple Stupid GUI
'i was trying to getrid of the confuse
'and make something oop like ;- )
Type ControlJ
	Tag As String
	hWnd As HWND
	Declare Property Text(  )  As String
	Declare Property Text( new_text As String )
	'	Declare Property SelText(  )  As String
	'	Declare Property SelText(  new_seltext As String )
	'	Declare Property SelStart(  )  As Integer
	'	Declare Property SelStart(  new_selstart As Integer )
	'	Declare Property SelLenth(  )  As Integer
	'	Declare Property SelLenth(  new_sellenth As Integer )
	Declare Property Enabled(  )  As Boolean
	Declare Property Enabled( is_enabled As Boolean )
	Declare Function lbAdd( new_itm As String )  As Integer
	Declare Function lbSelItem(  )  As Integer
	Declare Function lbItemText( itm_index As Integer )  As String
	Declare Sub lbfind(a As String)
	Declare Property Checked(  )  As Integer
	Declare Property Checked( is_checked As Integer )
	Declare Sub Focus(  )
	Declare Sub seticon( iconum As Integer )
	Declare Sub controlj.lbsetitemdata(itm_index As Integer,idata As integer)
	Declare Function controlj.lbgetitemdata(itm_index As Integer)As Integer
	
End Type
Property ControlJ.text(  )  As String
Dim lenofreturn As Integer
lenofreturn = GetWindowTextLength( hWnd )
If lenofreturn Then
	Dim buff As ZString Ptr
	buff = Allocate( lenofreturn + 3 )
	lenofreturn = GetWindowText( hWnd,buff,lenofreturn + 2 )
	Return *buff
Else

	Return ""
End If
End Property
Property ControlJ.Text(  new_text As String )
Dim Tmp As ZString Ptr
Tmp = StrPtr( new_text )
SetWindowText( hWnd,Tmp )
End Property
Property ControlJ.Enabled(  )  As Boolean
Return IsWindowEnabled( hWnd )
End Property
Property ControlJ.Enabled( is_enabled As Boolean )
EnableWindow( hWnd,is_enabled )
End Property
Function controlj.lbadd( new_itm As String )  As Integer
	If new_itm = "" Then Exit Function
	Return SendMessage ( hWnd,LB_ADDSTRING,Cast( WPARAM,0 )  ,Cast( LPARAM,StrPtr( new_itm )   )   )
End Function
Function controlj.lbselitem(  )  As Integer
	Return SendMessage( hWnd,LB_GETCURSEL,0,0  )
End Function
Function controlj.lbitemtext( itm_index As Integer ) As String
	Dim Ztext As ZString Ptr
	Dim nlen As Integer
	'	lb_GETLBTEXTLEN
	nlen = SendMessage( hWnd,LB_GETTEXTLEN,Cast( WPARAM,itm_index ) ,0 )
	Ztext = Allocate( nlen + 1 )
	nlen = SendMessage( hWnd,LB_GETTEXT,Cast( WPARAM,itm_index ) ,Cast( LPARAM,Ztext )  )
	If Not nlen = LB_ERR Then
		Return *Ztext
	Else
		Return ""
	EndIf
End Function
Sub controlj.lbsetitemdata(itm_index As Integer,idata As integer)
	SendMessage ( hWnd,LB_SETITEMDATA,Cast( WPARAM,itm_index )  ,Cast( LPARAM,idata ) )
End Sub
function controlj.lbgetitemdata(itm_index As Integer)As integer
	Return SendMessage ( hWnd,LB_GETITEMDATA,Cast( WPARAM,itm_index )  ,Cast( LPARAM,0) )
End Function
Sub controlj.lbfind(a As String)
	Dim b As Integer
	b = SendMessage(hWnd,LB_SELECTSTRING,-1,Cast(wparam,StrPtr(a)))
	'   If Not a = LB_ERR Then
	'
	'   EndIf
End Sub
Property ControlJ.Checked(  ) As Integer
Dim nState As Integer
nState = SendMessage( hWnd,BM_GETSTATE,0,0 )

If nState = BST_CHECKED Then
	Return -1
Else
	Return 0
EndIf

End Property
Property ControlJ.Checked( is_checked As Integer )
If is_checked Then
	SendMessage( hWnd,BM_SETSTATE,BST_CHECKED	 ,0 )
Else
	SendMessage( hWnd,BM_SETSTATE,BST_UNCHECKED ,0 )
EndIf
End Property
Sub controlj.focus(  )
	SetFocus( hWnd )
End Sub
Sub controlj.seticon( iconum As Integer )
	Dim As HICON HICO
	Dim MYhINSTANCE As HANDLE = GetModuleHandle( NULL )
	HICO = LoadIcon( MYhINSTANCE, Cast( ZString Ptr,iconum )   )
	SetClassLong( hWnd,GCL_HICON,Cast( Dword,HICO )   )
End Sub