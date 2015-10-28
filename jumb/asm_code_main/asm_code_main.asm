
format PE ;GUI 4.0

section "JumbCode"  data code readable writable executable

;change this to where you extract fasm
include 'E:\PrgL\fasm\INCLUDE\win32ax.inc'



;init
invoke _jumb_init

;generated jumb code file is included or just copy the code here
include 'E:\Jumb\JumbC\HELLO.asm'

;terminate
invoke _term



data import

 library kernel32,'KERNEL32.DLL',\
	 user32,'USER32.DLL',\
	 jumbdll,'jumb.dll'
import_kernel32
import_user32


 import jumbdll,\
	_jumb_init,'_jumb_init',\
	_cls,'_cls',\
	_sleep,'_sleep',\
	_term,'_term',\
	_print_str_eax,'_print_str_eax',\
	_print_int_eax,'_print_int_eax',\
	_print_newline,'_print_newline',\	
	_jumb_ucase,'_jumb_ucase',\
	_jumb_trim,'_jumb_trim',\
	_jumb_title,'_jumb_title',\
	_jumb_ltrim,'_jumb_ltrim',\
	_jumb_rtrim,'_jumb_rtrim',\
	_jumb_lcase,'_jumb_lcase'

end data

;resource file
section 'Resource' data readable resource from "E:\Jumb\JumbC\Hello.Res"