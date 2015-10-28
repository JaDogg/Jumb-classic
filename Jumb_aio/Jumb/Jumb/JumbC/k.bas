Dim Shared As ZString*20 key_words(30) = {"DEFSTR","DEFNUM","ASMCODE","ENDASM","CLS","PAUSE","SUB","ENDSUB","EXITSUB","SUBRETURN","LETINT","IF","TERMINATE","LOOP","ENDLOOP","LCASE","UCASE","TITLE","OKMSG","ECHO","ECHOINT","ECHONEWLINE","TRIM","LTRIM","RTRIM","ASM","KILL","ECHOSTREAX","CONSTZSTR"}
Open "o" For Output As #1
For i As Integer = 0 To 30
	? #1,key_words(i)
Next
sleep