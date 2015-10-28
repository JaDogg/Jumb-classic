jmp _jumb_defint_skp1
A1 dd ?
A2 dd ?
A3 dd ?
A4 dd ?
_jumb_defint_skp1:
jmp _JUMB_SKP_TMP_STR1
_JUMB_TMP_STR1 db "SIMPLE PLUS",0
_JUMB_SKP_TMP_STR1:
MOV EAX,_JUMB_TMP_STR1
invoke _jumb_title

MOV EAX,500
MOV [A1],EAX
MOV EAX,1000
MOV [A2],EAX
MOV EAX,[A1]
PUSH 50
invoke _jumb_add
PUSH [A2]
invoke _jumb_add
MOV [A3],EAX
MOV EAX,[A3]
MOV [A4],EAX
jmp _JUMB_SKP_TMP_STR2
_JUMB_TMP_STR2 db "A1 => ",0
_JUMB_SKP_TMP_STR2:
MOV EAX,_JUMB_TMP_STR2
invoke _print_str_eax

MOV EAX,[A1]
invoke _print_int_eax

invoke _print_newline
jmp _JUMB_SKP_TMP_STR3
_JUMB_TMP_STR3 db "A2 => ",0
_JUMB_SKP_TMP_STR3:
MOV EAX,_JUMB_TMP_STR3
invoke _print_str_eax

MOV EAX,[A2]
invoke _print_int_eax

invoke _print_newline
jmp _JUMB_SKP_TMP_STR4
_JUMB_TMP_STR4 db "A3 = A1 + 50 + A2 => ",0
_JUMB_SKP_TMP_STR4:
MOV EAX,_JUMB_TMP_STR4
invoke _print_str_eax

MOV EAX,[A3]
invoke _print_int_eax

invoke _print_newline
jmp _JUMB_SKP_TMP_STR5
_JUMB_TMP_STR5 db "A4 = A3 => ",0
_JUMB_SKP_TMP_STR5:
MOV EAX,_JUMB_TMP_STR5
invoke _print_str_eax

MOV EAX,[A4]
invoke _print_int_eax

invoke _print_newline
jmp _JUMB_SKP_TMP_STR6
_JUMB_TMP_STR6 db "A4 = A4 + 50 => ",0
_JUMB_SKP_TMP_STR6:
MOV EAX,_JUMB_TMP_STR6
invoke _print_str_eax

MOV EAX,[A4]
PUSH 50
invoke _jumb_add
MOV [A4],EAX
MOV EAX,[A4]
invoke _print_int_eax

invoke _print_newline
jmp _JUMB_SKP_TMP_STR7
_JUMB_TMP_STR7 db "DO YOU LIKE PLUS +++",0
_JUMB_SKP_TMP_STR7:
MOV EAX,_JUMB_TMP_STR7
invoke _print_str_eax

invoke _sleep
