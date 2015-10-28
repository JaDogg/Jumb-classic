jmp _jumb_defint_skp1
A1 dd ?
_jumb_defint_skp1:
jmp _JUMB_SKP_TMP_STR1
_JUMB_TMP_STR1 db "loop TEST => Eat THE Loop",0
_JUMB_SKP_TMP_STR1:
MOV EAX,_JUMB_TMP_STR1
invoke _jumb_title

mov eax,1
mov [A1],eax
_jumb_loop1_begin:
MOV EAX,[A1]
PUSH 5
invoke _jumb_add
MOV [A1],EAX
MOV EAX,[A1]
invoke _print_int_eax

jmp _JUMB_SKP_TMP_STR2
_JUMB_TMP_STR2 db " ",0
_JUMB_SKP_TMP_STR2:
MOV EAX,_JUMB_TMP_STR2
invoke _print_str_eax

cmp [A1],100
jge _jumb_loop1_end
inc [A1]
jmp _jumb_loop1_begin
_jumb_loop1_end:
invoke _sleep
