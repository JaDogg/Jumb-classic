#Include "crt.bi"
Extern "c++"

Type Stack

	Private:
	As Any Ptr Ptr data_
	As Integer length
	As Integer top

	Declare Sub Grow( )
	
	Public:
	Declare Constructor( ByVal size As Integer = 256 )
	Declare Destructor( )

	Declare Sub Push( ByVal d As Any Ptr )
	Declare Function Pop( ) As Any Ptr

	Declare Function GetAt( ByVal index As Integer ) As Any Ptr
	Declare Sub SetAt( ByVal index As Integer, ByVal d As Any Ptr )

	Declare Sub Enumerate( cb As Sub( ByVal As Any Ptr ) )

	Declare Function Size( ) As Integer

End Type

Sub Stack.Grow( )
	Dim As Any Ptr Ptr temp = New Any Ptr[length * 2]
	memcpy( temp, data_, length * SizeOf( Any Ptr ) )
	Delete[] data_
	data_ = temp
End Sub

''-----

Constructor Stack( ByVal size_ As Integer )
data_ = New Any Ptr[size_]
length = size_
top = 0
End Constructor

Destructor Stack( )
Delete[] data_
End Destructor

''-----

Sub Stack.Push( ByVal d As Any Ptr )
	If top >= length Then Grow( )

	data_[top] = d

	top += 1
End Sub

Function Stack.Pop( ) As Any Ptr
	If top > 0 Then
		top -= 1
		Return data_[top]
	Else
		Return NULL
	End If
End Function

''-----

Function Stack.GetAt( ByVal index As Integer ) As Any Ptr
	If ( index >= 0 ) And ( index < top ) Then _
	Return data_[index] _
Else _
Return NULL
End Function

Sub Stack.SetAt( ByVal index As Integer, ByVal d As Any Ptr )
	If ( index >= 0 ) And ( index < top ) Then
		data_[index] = d
	End If
End Sub

''-----

Sub Stack.Enumerate( cb As Sub( ByVal As Any Ptr ) )
	For i As Integer = 0 To top - 1
		cb( data_[i] )
	Next
End Sub

''-----

Function Stack.Size( ) As Integer
	Return top
End Function


End Extern
