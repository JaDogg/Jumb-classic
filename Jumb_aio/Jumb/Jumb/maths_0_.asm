jmp _jumb_defint_skp1
A1 dd ?
_jumb_defint_skp1:
jmp _JUMB_SKP_TMP_STR1
_JUMB_TMP_STR1 db "SIMPLE PLUS",0
_JUMB_SKP_TMP_STR1:
MOV EAX,_JUMB_TMP_STR1
invoke _jumb_title

MOV EAX,500
PUSH 255
invoke _jumb_mul
PUSH 456
invoke _jumb_add
PUSH 14
invoke _jumb_add
PUSH 45
invoke _jumb_min
PUSH 456
invoke _jumb_dev
MOV [A1],EAX
MOV EAX,[A1]
invoke _print_int_eax

invoke _print_newline
invoke _sleep
