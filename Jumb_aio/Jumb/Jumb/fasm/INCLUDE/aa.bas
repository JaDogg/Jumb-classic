Dim As String sdat,indat,tdat
Open "WIN32AX.INC" For Binary As #1
sdat = Space(Lof(1))
Get #1,,sdat
Close #1
Dim As Integer i,f,e

Do
	i = InStr(sdat,"include ")
	If i = 0 Then GoTo hell
	f = i+8
	e = f
	Do Until(sdat[e] = Asc("'"))
		e+=1
	Loop
 
	
	Open Mid(sdat,f+1,e-f) For Binary As #2
	indat = Space(Lof(2))
	Get #2,,indat
	Close #2
	
	tdat = Mid(sdat,1,i-1) & indat & Mid(sdat,e+2)
	sdat = tdat
	'Sleep
Loop

hell:

Open "out.asm" For Binary As #3
Put #3,,sdat
Close #3
'sleep
