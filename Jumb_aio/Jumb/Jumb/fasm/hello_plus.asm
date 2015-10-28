
format PE

section "JumbCode"  data code readable writable executable

include 'E:\Jumb\Jumb\fasm\INCLUDE\win32ax.inc'


invoke ExitProcess,0

data import

 library kernel32,'KERNEL32.DLL',\
	 user32,'USER32.DLL'
	 
	import_kernel32
	import_user32

end data



;(**Replace3**)