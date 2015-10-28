jmp _jumb_defint_skp1
TMPINT dd ?
TMPINT2 dd ?
TMPINT3 dd ?
_jumb_defint_skp1:
XOR EAX,EAX  ; (comments here will be included in asm output)
jmp _JUMB_SKP_TMP_STR1
_JUMB_TMP_STR1 db "    H3110 W0rlD   ",0
_JUMB_SKP_TMP_STR1:
MOV EAX,_JUMB_TMP_STR1
invoke _jumb_ucase
invoke _jumb_lcase
invoke _jumb_trim
invoke _jumb_title

jmp _JUMB_SKP_TMP_STR2
_JUMB_TMP_STR2 db "Jumb HELLO WORLD DEMO",0
_JUMB_SKP_TMP_STR2:
MOV EAX,_JUMB_TMP_STR2
invoke _print_str_eax

invoke _print_newline
mov eax,0
mov [TMPINT],eax
_jumb_loop1_begin:
invoke _print_newline
mov eax,[TMPINT]
mov [TMPINT2],eax
_jumb_loop2_begin:
mov eax,[TMPINT2]
mov [TMPINT3],eax
_jumb_loop3_begin:
MOV EAX,[TMPINT3]
invoke _print_int_eax

jmp _JUMB_SKP_TMP_STR3
_JUMB_TMP_STR3 db " ",0
_JUMB_SKP_TMP_STR3:
MOV EAX,_JUMB_TMP_STR3
invoke _print_str_eax

cmp [TMPINT3],7
jge _jumb_loop3_end
inc [TMPINT3]
jmp _jumb_loop3_begin
_jumb_loop3_end:
cmp [TMPINT2],7
jge _jumb_loop2_end
inc [TMPINT2]
jmp _jumb_loop2_begin
_jumb_loop2_end:
cmp [TMPINT],7
jge _jumb_loop1_end
inc [TMPINT]
jmp _jumb_loop1_begin
_jumb_loop1_end:
invoke _print_newline
invoke _print_newline
jmp _JUMB_SKP_TMP_STR4
_JUMB_TMP_STR4 db "Press any key to exit",0
_JUMB_SKP_TMP_STR4:
MOV EAX,_JUMB_TMP_STR4
invoke _print_str_eax

invoke _sleep
invoke _term
