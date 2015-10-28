Dim As UByte Ptr DATAX
Open "aa.bin" For Binary As #1
DATAX = Allocate(Lof(1)+1)
Get #1,,*DATAX,Lof(1)
Open "out.bas" For Output As #2
? #2,"Dim Shared As UByte CryptStub(" & Lof(1)-1 & ") = {" ;
For I As Integer = 0 To Lof(1)-1
	If I = Lof(1)-1 Then
		? #2, "&H" & Hex(DATAX[I]) & "}" 
	Else
		If (I Mod 10) = 0 Then
			? #2, "&H" & Hex(DATAX[I]) & ", _"
		Else
			? #2, "&H" & Hex(DATAX[I]) & ","; 
		EndIf
		
	EndIf
	
Next

Close #1
SLEEP