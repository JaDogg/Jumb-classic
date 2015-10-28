
; Extended Win32 programming headers (ASCII)


; Win32 programming headers (ASCII)


; Macroinstructions for defining data structures

macro struct name
 { fields@struct equ name
   match child parent, name \{ fields@struct equ child,fields@\#parent \}
   sub@struct equ
   struc db [val] \{ \common define field@struct .,db,<val>
			     fields@struct equ fields@struct,field@struct \}
   struc dw [val] \{ \common define field@struct .,dw,<val>
			     fields@struct equ fields@struct,field@struct \}
   struc du [val] \{ \common define field@struct .,du,<val>
			     fields@struct equ fields@struct,field@struct \}
   struc dd [val] \{ \common define field@struct .,dd,<val>
			     fields@struct equ fields@struct,field@struct \}
   struc dp [val] \{ \common define field@struct .,dp,<val>
			     fields@struct equ fields@struct,field@struct \}
   struc dq [val] \{ \common define field@struct .,dq,<val>
			     fields@struct equ fields@struct,field@struct \}
   struc dt [val] \{ \common define field@struct .,dt,<val>
			     fields@struct equ fields@struct,field@struct \}
   struc rb count \{ define field@struct .,db,count dup (?)
		     fields@struct equ fields@struct,field@struct \}
   struc rw count \{ define field@struct .,dw,count dup (?)
		     fields@struct equ fields@struct,field@struct \}
   struc rd count \{ define field@struct .,dd,count dup (?)
		     fields@struct equ fields@struct,field@struct \}
   struc rp count \{ define field@struct .,dp,count dup (?)
		     fields@struct equ fields@struct,field@struct \}
   struc rq count \{ define field@struct .,dq,count dup (?)
		     fields@struct equ fields@struct,field@struct \}
   struc rt count \{ define field@struct .,dt,count dup (?)
		     fields@struct equ fields@struct,field@struct \}
   macro db [val] \{ \common \local anonymous
		     define field@struct anonymous,db,<val>
		     fields@struct equ fields@struct,field@struct \}
   macro dw [val] \{ \common \local anonymous
		     define field@struct anonymous,dw,<val>
		     fields@struct equ fields@struct,field@struct \}
   macro du [val] \{ \common \local anonymous
		     define field@struct anonymous,du,<val>
		     fields@struct equ fields@struct,field@struct \}
   macro dd [val] \{ \common \local anonymous
		     define field@struct anonymous,dd,<val>
		     fields@struct equ fields@struct,field@struct \}
   macro dp [val] \{ \common \local anonymous
		     define field@struct anonymous,dp,<val>
		     fields@struct equ fields@struct,field@struct \}
   macro dq [val] \{ \common \local anonymous
		     define field@struct anonymous,dq,<val>
		     fields@struct equ fields@struct,field@struct \}
   macro dt [val] \{ \common \local anonymous
		     define field@struct anonymous,dt,<val>
		     fields@struct equ fields@struct,field@struct \}
   macro rb count \{ \local anonymous
		     define field@struct anonymous,db,count dup (?)
		     fields@struct equ fields@struct,field@struct \}
   macro rw count \{ \local anonymous
		     define field@struct anonymous,dw,count dup (?)
		     fields@struct equ fields@struct,field@struct \}
   macro rd count \{ \local anonymous
		     define field@struct anonymous,dd,count dup (?)
		     fields@struct equ fields@struct,field@struct \}
   macro rp count \{ \local anonymous
		     define field@struct anonymous,dp,count dup (?)
		     fields@struct equ fields@struct,field@struct \}
   macro rq count \{ \local anonymous
		     define field@struct anonymous,dq,count dup (?)
		     fields@struct equ fields@struct,field@struct \}
   macro rt count \{ \local anonymous
		     define field@struct anonymous,dt,count dup (?)
		     fields@struct equ fields@struct,field@struct \}
   macro union \{ fields@struct equ fields@struct,,union,<
		  sub@struct equ union \}
   macro struct \{ fields@struct equ fields@struct,,substruct,<
		  sub@struct equ substruct \}
   virtual at 0 }

macro ends
 { match , sub@struct \{ restruc db,dw,du,dd,dp,dq,dt
			 restruc rb,rw,rd,rp,rq,rt
			 purge db,dw,du,dd,dp,dq,dt
			 purge rb,rw,rd,rp,rq,rt
			 purge union,struct
			 match name=,fields,fields@struct \\{ fields@struct equ
							      make@struct name,fields
							      define fields@\\#name fields \\}
			 end virtual \}
   match any, sub@struct \{ fields@struct equ fields@struct> \}
   restore sub@struct }

macro make@struct name,[field,type,def]
 { common
    if $
     display 'Error: definition of ',`name,' contains illegal instructions.',0Dh,0Ah
     err
    end if
    local define
    define equ name
   forward
    local sub
    match , field \{ make@substruct type,name,sub def
		     define equ define,.,sub, \}
    match any, field \{ define equ define,.#field,type,<def> \}
   common
    match fields, define \{ define@struct fields \} }

macro define@struct name,[field,type,def]
 { common
    local list
    list equ
   forward
    if ~ field eq .
     name#field type def
     sizeof.#name#field = $ - name#field
    else
     label name#.#type
     rb sizeof.#type
    end if
    local value
    match any, list \{ list equ list, \}
    list equ list <value>
   common
    sizeof.#name = $
    restruc name
    match values, list \{
    struc name value \\{
    match any, fields@struct \\\{ fields@struct equ fields@struct,.,name,<values> \\\}
    match , fields@struct \\\{ label .
   forward
     match , value \\\\{ field type def \\\\}
     match any, value \\\\{ field type value
			    if ~ field eq .
			     rb sizeof.#name#field - ($-field)
			    end if \\\\}
   common \\\} \\}
    macro name value \\{
   forward
     match , value \\\{ type def \\\}
     match any, value \\\{ \\\local ..field
			   ..field = $
			   type value
			   if ~ field eq .
			    rb sizeof.#name#field - ($-..field)
			   end if \\\}
   common \\} \} }
macro enable@substruct
 { macro make@substruct substruct,parent,name,[field,type,def]
    \{ \common
	\local define
	define equ parent,name
       \forward
	\local sub
	match , field \\{ match any, type \\\{ enable@substruct
					       make@substruct type,name,sub def
					       purge make@substruct
					       define equ define,.,sub, \\\} \\}
	match any, field \\{ define equ define,.\#field,type,<def> \\}
       \common
	match fields, define \\{ define@\#substruct fields \\} \} }

enable@substruct

macro define@union parent,name,[field,type,def]
 { common
    virtual at parent#.#name
   forward
    if ~ field eq .
     virtual at parent#.#name
      parent#field type def
      sizeof.#parent#field = $ - parent#field
     end virtual
     if sizeof.#parent#field > $ - parent#.#name
      rb sizeof.#parent#field - ($ - parent#.#name)
     end if
    else
     virtual at parent#.#name
      parent type def
     end virtual
     label name#.#type at parent#.#name
     if sizeof.#type > $ - parent#.#name
      rb sizeof.#type - ($ - parent#.#name)
     end if
    end if
   common
    sizeof.#name = $ - parent#.#name
    end virtual
    struc name [value] \{ \common
    label .\#name
    last@union equ
   forward
    match any, last@union \\{ virtual at .\#name
			       field type def
			      end virtual \\}
    match , last@union \\{ match , value \\\{ field type def \\\}
			   match any, value \\\{ field type value \\\} \\}
    last@union equ field
   common rb sizeof.#name - ($ - .\#name) \} }

macro define@substruct parent,name,[field,type,def]
 { common
    virtual at parent#.#name
   forward
    if ~ field eq .
     parent#field type def
     sizeof.#parent#field = $ - parent#field
    else
     label name#.#type
     rb sizeof.#type
    end if
   common
    sizeof.#name = $ - parent#.#name
    end virtual
    struc name value \{
    label .\#name
   forward
     match , value \\{ field type def \\}
     match any, value \\{ field type value
			  if ~ field eq .
			   rb sizeof.#parent#field - ($-field)
			  end if \\}
   common \} }


; Macroinstructions for defining and calling procedures

macro stdcall proc,[arg]		; directly call STDCALL procedure
 { common
    if ~ arg eq
   reverse
    pushd arg
   common
    end if
    call proc }

macro invoke proc,[arg] 		; indirectly call STDCALL procedure
 { common
    if ~ arg eq
   reverse
     pushd arg
   common
    end if
    call [proc] }

macro ccall proc,[arg]			; directly call CDECL procedure
 { common
    size@ccall = 0
    if ~ arg eq
   reverse
    pushd arg
    size@ccall = size@ccall+4
   common
    end if
    call proc
    if size@ccall
    add esp,size@ccall
    end if }

macro cinvoke proc,[arg]		; indirectly call CDECL procedure
 { common
    size@ccall = 0
    if ~ arg eq
   reverse
    pushd arg
    size@ccall = size@ccall+4
   common
    end if
    call [proc]
    if size@ccall
    add esp,size@ccall
    end if }

macro proc [args]			; define procedure
 { common
    match name params, args>
    \{ define@proc name,<params \} }

prologue@proc equ prologuedef

macro prologuedef procname,flag,parmbytes,localbytes,reglist
 { local loc
   loc = (localbytes+3) and (not 3)
   parmbase@proc equ ebp+8
   localbase@proc equ ebp-loc
   if parmbytes | localbytes
    push ebp
    mov ebp,esp
    if localbytes
     sub esp,loc
    end if
   end if
   irps reg, reglist \{ push reg \} }

epilogue@proc equ epiloguedef

macro epiloguedef procname,flag,parmbytes,localbytes,reglist
 { irps reg, reglist \{ reverse pop reg \}
   if parmbytes | localbytes
    leave
   end if
   if flag and 10000b
    retn
   else
    retn parmbytes
   end if }

close@proc equ

macro define@proc name,statement
 { local params,flag,regs,parmbytes,localbytes,current
   if used name
   name:
   match =stdcall args, statement \{ params equ args
				     flag = 11b \}
   match =stdcall, statement \{ params equ
				flag = 11b \}
   match =c args, statement \{ params equ args
			       flag = 10001b \}
   match =c, statement \{ params equ
			  flag = 10001b \}
   match =params, params \{ params equ statement
			    flag = 0 \}
   match =uses reglist=,args, params \{ regs equ reglist
					params equ args \}
   match =regs =uses reglist, regs params \{ regs equ reglist
					     params equ \}
   match =regs, regs \{ regs equ \}
   match prologue:reglist, prologue@proc:<regs> \{ prologue name,flag,parmbytes,localbytes,reglist \}
   virtual at parmbase@proc
   match =,args, params \{ defargs@proc args \}
   match =args@proc args, args@proc params \{ defargs@proc args \}
   parmbytes = $-(parmbase@proc)
   end virtual
   name # % = parmbytes/4
   all@vars equ
   current = 0
   macro locals
   \{ virtual at localbase@proc+current
      macro label def \\{ match . type,def> \\\{ deflocal@proc .,label,<type \\\} \\}
      struc db [val] \\{ \common deflocal@proc .,db,val \\}
      struc du [val] \\{ \common deflocal@proc .,du,val \\}
      struc dw [val] \\{ \common deflocal@proc .,dw,val \\}
      struc dp [val] \\{ \common deflocal@proc .,dp,val \\}
      struc dd [val] \\{ \common deflocal@proc .,dd,val \\}
      struc dt [val] \\{ \common deflocal@proc .,dt,val \\}
      struc dq [val] \\{ \common deflocal@proc .,dq,val \\}
      struc rb cnt \\{ deflocal@proc .,rb cnt, \\}
      struc rw cnt \\{ deflocal@proc .,rw cnt, \\}
      struc rp cnt \\{ deflocal@proc .,rp cnt, \\}
      struc rd cnt \\{ deflocal@proc .,rd cnt, \\}
      struc rt cnt \\{ deflocal@proc .,rt cnt, \\}
      struc rq cnt \\{ deflocal@proc .,rq cnt, \\} \}
   macro endl
   \{ purge label
      restruc db,du,dw,dp,dd,dt,dq
      restruc rb,rw,rp,rd,rt,rq
      current = $-(localbase@proc)
      end virtual \}
   macro ret operand
   \{ match any, operand \\{ retn operand \\}
      match , operand \\{ match epilogue:reglist, epilogue@proc:<regs> \\\{ epilogue name,flag,parmbytes,localbytes,reglist \\\} \\} \}
   macro finish@proc
   \{ localbytes = current
      match close:reglist, close@proc:<regs> \\{ close name,flag,parmbytes,localbytes,reglist \\}
      end if \} }

macro defargs@proc [arg]
 { common
    if ~ arg eq
   forward
     local ..arg,current@arg
     match argname:type, arg
      \{ current@arg equ argname
	 label ..arg type
	 argname equ ..arg
	 if dqword eq type
	   dd ?,?,?,?
	 else if tbyte eq type
	   dd ?,?,?
	 else if qword eq type | pword eq type
	   dd ?,?
	 else
	   dd ?
	 end if \}
     match =current@arg,current@arg
      \{ current@arg equ arg
	 arg equ ..arg
	 ..arg dd ? \}
   common
     args@proc equ current@arg
   forward
     restore current@arg
   common
    end if }

macro deflocal@proc name,def,[val] { name def val }

macro deflocal@proc name,def,[val]
 { common
    match vars, all@vars \{ all@vars equ all@vars, \}
    all@vars equ all@vars name
   forward
    local ..var,..tmp
    ..var def val
    match =?, val \{ ..tmp equ \}
    match any =?, val \{ ..tmp equ \}
    match any (=?), val \{ ..tmp equ \}
    match =label, def \{ ..tmp equ \}
    match tmp : value, ..tmp : val
     \{ tmp: end virtual
	initlocal@proc ..var,def value
	virtual at tmp\}
   common
    match first rest, ..var, \{ name equ first \} }

struc label type { label . type }

macro initlocal@proc name,def
 { virtual at name
    def
    size@initlocal = $ - name
   end virtual
   position@initlocal = 0
   while size@initlocal > position@initlocal
    virtual at name
     def
     if size@initlocal - position@initlocal < 2
      current@initlocal = 1
      load byte@initlocal byte from name+position@initlocal
     else if size@initlocal - position@initlocal < 4
      current@initlocal = 2
      load word@initlocal word from name+position@initlocal
     else
      current@initlocal = 4
      load dword@initlocal dword from name+position@initlocal
     end if
    end virtual
    if current@initlocal = 1
     mov byte [name+position@initlocal],byte@initlocal
    else if current@initlocal = 2
     mov word [name+position@initlocal],word@initlocal
    else
     mov dword [name+position@initlocal],dword@initlocal
    end if
    position@initlocal = position@initlocal + current@initlocal
   end while }

macro endp
 { purge ret,locals,endl
   finish@proc
   purge finish@proc
   restore regs@proc
   match all,args@proc \{ restore all \}
   restore args@proc
   match all,all@vars \{ restore all \} }

macro local [var]
 { common
    locals
   forward done@local equ
    match varname[count]:vartype, var
    \{ match =BYTE, vartype \\{ varname rb count
				restore done@local \\}
       match =WORD, vartype \\{ varname rw count
				restore done@local \\}
       match =DWORD, vartype \\{ varname rd count
				 restore done@local \\}
       match =PWORD, vartype \\{ varname rp count
				 restore done@local \\}
       match =QWORD, vartype \\{ varname rq count
				 restore done@local \\}
       match =TBYTE, vartype \\{ varname rt count
				 restore done@local \\}
       match =DQWORD, vartype \\{ label varname dqword
				  rq count+count
				  restore done@local \\}
       match , done@local \\{ virtual
			       varname vartype
			      end virtual
			      rb count*sizeof.\#vartype
			      restore done@local \\} \}
    match :varname:vartype, done@local:var
    \{ match =BYTE, vartype \\{ varname db ?
				restore done@local \\}
       match =WORD, vartype \\{ varname dw ?
				restore done@local \\}
       match =DWORD, vartype \\{ varname dd ?
				 restore done@local \\}
       match =PWORD, vartype \\{ varname dp ?
				 restore done@local \\}
       match =QWORD, vartype \\{ varname dq ?
				 restore done@local \\}
       match =TBYTE, vartype \\{ varname dt ?
				 restore done@local \\}
       match =DQWORD, vartype \\{ label varname dqword
				  dq ?,?
				  restore done@local \\}
       match , done@local \\{ varname vartype
			      restore done@local \\} \}
    match ,done@local
    \{ var
       restore done@local \}
   common
    endl }


; Macroinstructions for interfacing the COM (Component Object Model) classes

macro cominvk object,proc,[arg]
 { common
    if ~ arg eq
   reverse
     pushd arg
   common
    end if
    mov eax,[object]
    push eax
    mov eax,[eax]
    call [eax+object#.#proc] }

macro comcall handle,object,proc,[arg]
 { common
    if ~ arg eq
   reverse
     pushd arg
   common
    end if
    if handle eqtype eax | handle eqtype 0
     push handle
     local ..handle
     label ..handle at handle
     mov eax,[..handle]
    else
     mov eax,handle
     push eax
     mov eax,[eax]
    end if
    call [eax+object#.#proc] }

macro interface name,[proc]
 { common
    struc name \{
    match any, fields@struct \\{ fields@struct equ fields@struct,.,name, \\}
    match , fields@struct \\{ . dd ?
    virtual at 0
   forward
    .#proc dd ?
   common
    end virtual \\} \}
    virtual at 0
   forward
     name#.#proc dd ?
   common
    end virtual }



; Macroinstructions for making import section

macro library [name,string]
 { forward
    local _label
    if defined name#.redundant
     if ~ name#.redundant
      dd RVA name#.lookup,0,0,RVA _label,RVA name#.address
     end if
    end if
    name#.referred = 1
   common
    dd 0,0,0,0,0
   forward
    if defined name#.redundant
     if ~ name#.redundant
      _label db string,0
	     rb RVA $ and 1
     end if
    end if }

macro import name,[label,string]
 { common
    if defined name#.referred
     name#.lookup:
   forward
     if used label
      if string eqtype ''
       local _label
       dd RVA _label
      else
       dd 80000000h + string
      end if
     end if
   common
     if $ > name#.lookup
      name#.redundant = 0
      dd 0
     else
      name#.redundant = 1
     end if
     name#.address:
   forward
     if used label
      if string eqtype ''
       label dd RVA _label
      else
       label dd 80000000h + string
      end if
     end if
   common
     if ~ name#.redundant
      dd 0
     end if
   forward
     if used label & string eqtype ''
     _label dw 0
	    db string,0
	    rb RVA $ and 1
     end if
   common
    end if }

macro api [name] {}


; Macroinstruction for making export section

macro export dllname,[label,string]
 { common
    local module,addresses,names,ordinal,count
    count = 0
   forward
    count = count+1
   common
    dd 0,0,0,RVA module,1
    dd count,count,RVA addresses,RVA names,RVA ordinal
    addresses:
   forward
    dd RVA label
   common
    names:
   forward
    local name
    dd RVA name
   common
    ordinal: count = 0
   forward
    dw count
    count = count+1
   common
    module db dllname,0
   forward
    name db string,0
   common
    local x,y,z,str1,str2,v1,v2
    x = count shr 1
    while x > 0
     y = x
     while y < count
      z = y
      while z-x >= 0
       load v1 dword from names+z*4
       str1=($-RVA $)+v1
       load v2 dword from names+(z-x)*4
       str2=($-RVA $)+v2
       while v1 > 0
	load v1 from str1+%-1
	load v2 from str2+%-1
	if v1 <> v2
	 break
	end if
       end while
       if v1<v2
	load v1 dword from names+z*4
	load v2 dword from names+(z-x)*4
	store dword v1 at names+(z-x)*4
	store dword v2 at names+z*4
	load v1 word from ordinal+z*2
	load v2 word from ordinal+(z-x)*2
	store word v1 at ordinal+(z-x)*2
	store word v2 at ordinal+z*2
       else
	break
       end if
       z = z-x
      end while
      y = y+1
     end while
     x = x shr 1
    end while }


; Macroinstructions for making resource section

macro directory [type,label]
 { common
    local max,count
    count = 0
    max = 0
   forward
    count = count + 1
    if type > max
     max = type
    end if
   common
    root@resource dd 0,%t,0,count shl 16
    repeat max
   forward
    if % = type
     dd type,80000000h+label-root@resource
    end if
   common
    end repeat }

macro resource dir,[id,lang,label]
 { common
    dir:
    local min,max,count,current
   forward
    min = id
    max = id
   common
    count = 0
   forward
    count = count + 1
    if id < min
     min = id
    else if id > max
     max = id
    end if
   common
    dd 0,%t,0,count shl 16
    repeat max-min+1
     current = $
   forward
     if min+%-1 = id
      if current = $
       dd id,80000000h+label#.directory-root@resource
      end if
     end if
   common
    end repeat
    repeat max-min+1
     current = $
   forward
     if min+%-1 = id
      if current = $
       label#.directory dd 0,%t,0,10000h,lang,label-root@resource
       count = 1
      else
       dd lang,label-root@resource
       count = count + 1
      end if
     end if
     label#.resid = id
   common
     local x,y,z,v1,v2
     if count > 1
      store word count at current+0Eh
      x = count shr 1
      while x > 0
       y = x
       while y < count
	z = y
	while z-x >= 0
	 load v1 dword from current+10h+z*8
	 load v2 dword from current+10h+(z-x)*8
	 if v1<v2
	  store dword v1 at current+10h+(z-x)*8
	  store dword v2 at current+10h+z*8
	  load v1 dword from current+10h+z*8+4
	  load v2 dword from current+10h+(z-x)*8+4
	  store dword v1 at current+10h+(z-x)*8+4
	  store dword v2 at current+10h+z*8+4
	 else
	  break
	 end if
	 z = z-x
	end while
	y = y+1
       end while
       x = x shr 1
      end while
     end if
    end repeat }

macro bitmap label,bitmap_file
 { local data,size
   label dd RVA data,size,0,0
   data file bitmap_file:0Eh
   size = $ - data
   align 4 }

macro icon group,[label,icon_file]
 { common local count
    count = 0
   forward local data,size,position
    label dd RVA data,size,0,0
    virtual at 0
     file icon_file:6,16
     load size dword from 8
     load position dword from 12
    end virtual
    data file icon_file:position,size
    count = count+1
   common local header
    align 4
    group dd RVA header,6+count*14,0,0
    header dw 0,1,count
   forward
    file icon_file:6,12
    dw label#.resid
   common
    align 4 }

macro cursor group,[label,cursor_file]
 { common local count
    count = 0
   forward local data,width,height,size,position
    label dd RVA data,size+4,0,0
    virtual at 0
     file cursor_file:6,16
     load width byte from 0
     load height byte from 1
     load size dword from 8
     load position dword from 12
    end virtual
    data file cursor_file:10,4
	 file cursor_file:position,size
    count = count+1
   common local header
    align 4
    group dd RVA header,6+count*14,0,0
    header dw 0,2,count
   forward
    dw width,height,1,0
    dd size+4
    dw label#.resid
   common
    align 4 }

macro menu label
 { local data,size
   label dd RVA data,size,0,0
   data dw 1,4,0,0
   menu_size equ size = $ - data
   menu_level = 1 }

macro menuitem string,id,resinfo,status,type
 { dd MFT_STRING or type+0,status+0,id
   dw resinfo+0
   du string,0
   align 4
   if ~ resinfo eq
    if resinfo and MFR_END
     menu_level = menu_level - 1
    end if
    if resinfo and MFR_POPUP
     menu_level = menu_level + 1
     dd 0
    end if
   end if
   if menu_level = 0
    menu_size
   end if }

macro menuseparator resinfo
 { dd MFT_SEPARATOR,0,0
   dw resinfo+0,0
   if ~ resinfo eq
    if resinfo and MFR_END
     menu_level = menu_level - 1
    end if
   end if
   if menu_level = 0
    menu_size
   end if }

macro dialog label,title,x,y,cx,cy,style,exstyle,menu,fontname,fontsize
 { local data,size,items
   label dd RVA data,size,0,0
   data dd style or DS_SETFONT,exstyle +0
   dw items,x,y,cx,cy
   if menu+0 <> 0
    dw 0FFFFh
   end if
   du menu+0,0,title,0
   if fontname eq
    du 8,'MS Sans Serif',0
   else
    du fontsize+0,fontname,0
   end if
   align 4
   dialog_size equ size = $ - data
   dialog_items equ items = dialog_items_counter
   dialog_items_counter = 0 }

macro dialogitem class,title,id,x,y,cx,cy,style,exstyle
 { dd style or WS_CHILD,exstyle +0
   dw x,y,cx,cy,id
   if class eq 'BUTTON'
    dw 0FFFFh,80h
   else if class eq 'EDIT'
    dw 0FFFFh,81h
   else if class eq 'STATIC'
    dw 0FFFFh,82h
   else if class eq 'LISTBOX'
    dw 0FFFFh,83h
   else if class eq 'SCROLLBAR'
    dw 0FFFFh,84h
   else if class eq 'COMBOBOX'
    dw 0FFFFh,85h
   else
    du class,0
   end if
   if title eqtype 0
    dw 0FFFFh,title
   else
    du title,0
   end if
   dw 0
   align 4
   dialog_items_counter = dialog_items_counter + 1 }

macro enddialog
 { dialog_items
   dialog_size }

macro accelerator label,[fvirt,key,cmd]
 { common
    local data,size
    label dd RVA data,size,0,0
    data:
    accel_count = 0
   forward
    accel_count = accel_count + 1
   common
    size = accel_count * 8
   forward
    accel_count = accel_count - 1
    if accel_count = 0
     dw fvirt or 80h,key
    else
     dw fvirt,key
    end if
    dd cmd }

macro versioninfo label,fileos,filetype,filesubtype,lang,cp,[name,value]
 { common
    local data,size,vivalue,visize
    label dd RVA data,size,0,0
    data dw size,visize,0
    du 'VS_VERSION_INFO',0,0
    vivalue dd 0FEEF04BDh,00010000h
    local version,count,shift,char,filever,productver
    filever = 0
    productver = 0
   forward
    if name eq 'FileVersion' | name eq 'ProductVersion'
     virtual at 0
      db value
      count = $
      version = 0
      shift = 16
      repeat count
       load char from %-1
       if char='.'
	if shift mod 32
	 shift = shift-16
	else
	 shift = shift+32+16
	end if
       else
	version = (version and not (0FFFFh shl shift)) or ((version shr shift and 0FFFFh)*10+char-'0') shl shift
       end if
      end repeat
     end virtual
     if name eq 'FileVersion'
      filever = version
     else if name eq 'ProductVersion'
      productver = version
     end if
    end if
   common
    dq filever,productver
    dd 0,0,fileos,filetype+0,filesubtype+0,0,0
    visize = $ - vivalue
    local sfi_data,sfi_size
    sfi_data dd sfi_size
    du 1,'StringFileInfo',0
    local str_data,str_size
    str_data dd str_size
    du 1,'040904E4',0
   forward
    local vs_data,vs_size,value_data,value_size
    align 4
    vs_data dw vs_size,value_size/2
    du 1,name,0
    align 4
    value_data du value,0
    value_size = $ - value_data
    vs_size = $ - vs_data
   common
    align 4
    str_size = $ - str_data
    sfi_size = $ - sfi_data
    local vfi_data,vfi_size,var_data,var_size
    vfi_data dd vfi_size
    du 1,'VarFileInfo',0,0
    var_data dw var_size,4
    du 0,'Translation',0,0
    dw lang,cp+0
    var_size = $ - var_data
    vfi_size = $ - vfi_data
    size = $ - data }

macro resdata label
{ local data,size
  label dd RVA data,size,0,0
  data = $
  ressize equ size = $ - data }

macro endres
{ ressize
  align 4 }


struc TCHAR [val] { common match any, val \{ . db val \}
                           match , val \{ . db ? \} }
sizeof.TCHAR = 1


; KERNEL32.DLL structures and constants

struct SYSTEM_INFO
  wProcessorArchitecture      dw ?
  wReserved		      dw ?
  dwPageSize		      dd ?
  lpMinimumApplicationAddress dd ?
  lpMaximumApplicationAddress dd ?
  dwActiveProcessorMask       dd ?
  dwNumberOfProcessors	      dd ?
  dwProcessorType	      dd ?
  dwAllocationGranularity     dd ?
  wProcessorLevel	      dw ?
  wProcessorRevision	      dw ?
ends

struct OSVERSIONINFO
  dwOSVersionInfoSize dd ?
  dwMajorVersion      dd ?
  dwMinorVersion      dd ?
  dwBuildNumber       dd ?
  dwPlatformId	      dd ?
  szCSDVersion	      TCHAR 128 dup (?)
ends

struct OSVERSIONINFOA
  dwOSVersionInfoSize dd ?
  dwMajorVersion      dd ?
  dwMinorVersion      dd ?
  dwBuildNumber       dd ?
  dwPlatformId	      dd ?
  szCSDVersion	      db 128 dup (?)
ends

struct OSVERSIONINFOW
  dwOSVersionInfoSize dd ?
  dwMajorVersion      dd ?
  dwMinorVersion      dd ?
  dwBuildNumber       dd ?
  dwPlatformId	      dd ?
  szCSDVersion	      du 128 dup (?)
ends

struct MEMORYSTATUS
  dwiLength	  dd ?
  dwMemoryLoad	  dd ?
  dwTotalPhys	  dd ?
  dwAvailPhys	  dd ?
  dwTotalPageFile dd ?
  dwAvailPageFile dd ?
  dwTotalVirtual  dd ?
  dwAvailVirtual  dd ?
ends

struct STARTUPINFO
  cb		  dd ?
  lpReserved	  dd ?
  lpDesktop	  dd ?
  lpTitle	  dd ?
  dwX		  dd ?
  dwY		  dd ?
  dwXSize	  dd ?
  dwYSize	  dd ?
  dwXCountChars   dd ?
  dwYCountChars   dd ?
  dwFillAttribute dd ?
  dwFlags	  dd ?
  wShowWindow	  dw ?
  cbReserved2	  dw ?
  lpReserved2	  dd ?
  hStdInput	  dd ?
  hStdOutput	  dd ?
  hStdError	  dd ?
ends

struct PROCESS_INFORMATION
  hProcess    dd ?
  hThread     dd ?
  dwProcessId dd ?
  dwThreadId  dd ?
ends

struct FILETIME
  dwLowDateTime  dd ?
  dwHighDateTime dd ?
ends

struct SYSTEMTIME
  wYear 	dw ?
  wMonth	dw ?
  wDayOfWeek	dw ?
  wDay		dw ?
  wHour 	dw ?
  wMinute	dw ?
  wSecond	dw ?
  wMilliseconds dw ?
ends

struct BY_HANDLE_FILE_INFORMATION
  dwFileAttributes     dd ?
  ftCreationTime       FILETIME
  ftLastAccessTime     FILETIME
  ftLastWriteTime      FILETIME
  dwVolumeSerialNumber dd ?
  nFileSizeHigh        dd ?
  nFileSizeLow	       dd ?
  nNumberOfLinks       dd ?
  nFileIndexHigh       dd ?
  nFileIndexLow        dd ?
ends

struct WIN32_FIND_DATA
  dwFileAttributes   dd ?
  ftCreationTime     FILETIME
  ftLastAccessTime   FILETIME
  ftLastWriteTime    FILETIME
  nFileSizeHigh      dd ?
  nFileSizeLow	     dd ?
  dwReserved0	     dd ?
  dwReserved1	     dd ?
  cFileName	     TCHAR MAX_PATH dup (?)
  cAlternateFileName TCHAR 14 dup (?)
ends

struct WIN32_FIND_DATAA
  dwFileAttributes   dd ?
  ftCreationTime     FILETIME
  ftLastAccessTime   FILETIME
  ftLastWriteTime    FILETIME
  nFileSizeHigh      dd ?
  nFileSizeLow	     dd ?
  dwReserved0	     dd ?
  dwReserved1	     dd ?
  cFileName	     db MAX_PATH dup (?)
  cAlternateFileName db 14 dup (?)
ends

struct WIN32_FIND_DATAW
  dwFileAttributes   dd ?
  ftCreationTime     FILETIME
  ftLastAccessTime   FILETIME
  ftLastWriteTime    FILETIME
  nFileSizeHigh      dd ?
  nFileSizeLow	     dd ?
  dwReserved0	     dd ?
  dwReserved1	     dd ?
  cFileName	     du MAX_PATH dup (?)
  cAlternateFileName du 14 dup (?)
ends

; General constants

NULL  = 0
TRUE  = 1
FALSE = 0

; Maximum path length in characters

MAX_PATH = 260

; Access rights

DELETE_RIGHT		  = 00010000h
READ_CONTROL		  = 00020000h
WRITE_DAC		  = 00040000h
WRITE_OWNER		  = 00080000h
SYNCHRONIZE		  = 00100000h
STANDARD_RIGHTS_READ	  = READ_CONTROL
STANDARD_RIGHTS_WRITE	  = READ_CONTROL
STANDARD_RIGHTS_EXECUTE   = READ_CONTROL
STANDARD_RIGHTS_REQUIRED  = 000F0000h
STANDARD_RIGHTS_ALL	  = 001F0000h
SPECIFIC_RIGHTS_ALL	  = 0000FFFFh
ACCESS_SYSTEM_SECURITY	  = 01000000h
MAXIMUM_ALLOWED 	  = 02000000h
GENERIC_READ		  = 80000000h
GENERIC_WRITE		  = 40000000h
GENERIC_EXECUTE 	  = 20000000h
GENERIC_ALL		  = 10000000h
PROCESS_TERMINATE	  = 00000001h
PROCESS_CREATE_THREAD	  = 00000002h
PROCESS_VM_OPERATION	  = 00000008h
PROCESS_VM_READ 	  = 00000010h
PROCESS_VM_WRITE	  = 00000020h
PROCESS_DUP_HANDLE	  = 00000040h
PROCESS_CREATE_PROCESS	  = 00000080h
PROCESS_SET_QUOTA	  = 00000100h
PROCESS_SET_INFORMATION   = 00000200h
PROCESS_QUERY_INFORMATION = 00000400h
PROCESS_ALL_ACCESS	  = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or 0FFFh
FILE_SHARE_READ 	  = 00000001h
FILE_SHARE_WRITE	  = 00000002h
FILE_SHARE_DELETE	  = 00000004h

; CreateFile actions

CREATE_NEW	  = 1
CREATE_ALWAYS	  = 2
OPEN_EXISTING	  = 3
OPEN_ALWAYS	  = 4
TRUNCATE_EXISTING = 5

; OpenFile modes

OF_READ 	    = 0000h
OF_WRITE	    = 0001h
OF_READWRITE	    = 0002h
OF_SHARE_COMPAT     = 0000h
OF_SHARE_EXCLUSIVE  = 0010h
OF_SHARE_DENY_WRITE = 0020h
OF_SHARE_DENY_READ  = 0030h
OF_SHARE_DENY_NONE  = 0040h
OF_PARSE	    = 0100h
OF_DELETE	    = 0200h
OF_VERIFY	    = 0400h
OF_CANCEL	    = 0800h
OF_CREATE	    = 1000h
OF_PROMPT	    = 2000h
OF_EXIST	    = 4000h
OF_REOPEN	    = 8000h

; SetFilePointer methods

FILE_BEGIN   = 0
FILE_CURRENT = 1
FILE_END     = 2

; File attributes

FILE_ATTRIBUTE_READONLY   = 001h
FILE_ATTRIBUTE_HIDDEN	  = 002h
FILE_ATTRIBUTE_SYSTEM	  = 004h
FILE_ATTRIBUTE_DIRECTORY  = 010h
FILE_ATTRIBUTE_ARCHIVE	  = 020h
FILE_ATTRIBUTE_NORMAL	  = 080h
FILE_ATTRIBUTE_TEMPORARY  = 100h
FILE_ATTRIBUTE_COMPRESSED = 800h

; File flags

FILE_FLAG_WRITE_THROUGH    = 80000000h
FILE_FLAG_OVERLAPPED	   = 40000000h
FILE_FLAG_NO_BUFFERING	   = 20000000h
FILE_FLAG_RANDOM_ACCESS    = 10000000h
FILE_FLAG_SEQUENTIAL_SCAN  = 08000000h
FILE_FLAG_DELETE_ON_CLOSE  = 04000000h
FILE_FLAG_BACKUP_SEMANTICS = 02000000h
FILE_FLAG_POSIX_SEMANTICS  = 01000000h

; Notify filters

FILE_NOTIFY_CHANGE_FILE_NAME  = 001h
FILE_NOTIFY_CHANGE_DIR_NAME   = 002h
FILE_NOTIFY_CHANGE_ATTRIBUTES = 004h
FILE_NOTIFY_CHANGE_SIZE       = 008h
FILE_NOTIFY_CHANGE_LAST_WRITE = 010h
FILE_NOTIFY_CHANGE_SECURITY   = 100h

; File types

FILE_TYPE_UNKNOWN = 0
FILE_TYPE_DISK	  = 1
FILE_TYPE_CHAR	  = 2
FILE_TYPE_PIPE	  = 3
FILE_TYPE_REMOTE  = 8000h

; LockFileEx flags

LOCKFILE_FAIL_IMMEDIATELY = 1
LOCKFILE_EXCLUSIVE_LOCK   = 2

; MoveFileEx flags

MOVEFILE_REPLACE_EXISTING   = 1
MOVEFILE_COPY_ALLOWED	    = 2
MOVEFILE_DELAY_UNTIL_REBOOT = 4
MOVEFILE_WRITE_THROUGH	    = 8

; FindFirstFileEx flags

FIND_FIRST_EX_CASE_SENSITIVE = 1

; Device handles

INVALID_HANDLE_VALUE = -1
STD_INPUT_HANDLE     = -10
STD_OUTPUT_HANDLE    = -11
STD_ERROR_HANDLE     = -12

; DuplicateHandle options

DUPLICATE_CLOSE_SOURCE = 1
DUPLICATE_SAME_ACCESS  = 2

; File mapping acccess rights

SECTION_QUERY	    = 01h
SECTION_MAP_WRITE   = 02h
SECTION_MAP_READ    = 04h
SECTION_MAP_EXECUTE = 08h
SECTION_EXTEND_SIZE = 10h
SECTION_ALL_ACCESS  = STANDARD_RIGHTS_REQUIRED or SECTION_QUERY or SECTION_MAP_WRITE or SECTION_MAP_READ or SECTION_MAP_EXECUTE or SECTION_EXTEND_SIZE
FILE_MAP_COPY	    = SECTION_QUERY
FILE_MAP_WRITE	    = SECTION_MAP_WRITE
FILE_MAP_READ	    = SECTION_MAP_READ
FILE_MAP_ALL_ACCESS = SECTION_ALL_ACCESS

; File system flags

FILE_CASE_SENSITIVE_SEARCH = 0001h
FILE_CASE_PRESERVED_NAMES  = 0002h
FILE_UNICODE_ON_DISK	   = 0004h
FILE_PERSISTENT_ACLS	   = 0008h
FILE_FILE_COMPRESSION	   = 0010h
FILE_VOLUME_IS_COMPRESSED  = 8000h
FS_CASE_IS_PRESERVED	   = FILE_CASE_PRESERVED_NAMES
FS_CASE_SENSITIVE	   = FILE_CASE_SENSITIVE_SEARCH
FS_UNICODE_STORED_ON_DISK  = FILE_UNICODE_ON_DISK
FS_PERSISTENT_ACLS	   = FILE_PERSISTENT_ACLS

; Drive types

DRIVE_UNKNOWN	  = 0
DRIVE_NO_ROOT_DIR = 1
DRIVE_REMOVABLE   = 2
DRIVE_FIXED	  = 3
DRIVE_REMOTE	  = 4
DRIVE_CDROM	  = 5
DRIVE_RAMDISK	  = 6

; Pipe modes

PIPE_ACCESS_INBOUND	 = 1
PIPE_ACCESS_OUTBOUND	 = 2
PIPE_ACCESS_DUPLEX	 = 3
PIPE_CLIENT_END 	 = 0
PIPE_SERVER_END 	 = 1
PIPE_WAIT		 = 0
PIPE_NOWAIT		 = 1
PIPE_READMODE_BYTE	 = 0
PIPE_READMODE_MESSAGE	 = 2
PIPE_TYPE_BYTE		 = 0
PIPE_TYPE_MESSAGE	 = 4
PIPE_UNLIMITED_INSTANCES = 255

; Global memory flags

GMEM_FIXED	       = 0000h
GMEM_MOVEABLE	       = 0002h
GMEM_NOCOMPACT	       = 0010h
GMEM_NODISCARD	       = 0020h
GMEM_ZEROINIT	       = 0040h
GMEM_MODIFY	       = 0080h
GMEM_DISCARDABLE       = 0100h
GMEM_NOT_BANKED        = 1000h
GMEM_SHARE	       = 2000h
GMEM_DDESHARE	       = 2000h
GMEM_NOTIFY	       = 4000h
GMEM_LOWER	       = GMEM_NOT_BANKED
GMEM_VALID_FLAGS       = 7F72h
GMEM_INVALID_HANDLE    = 8000h
GMEM_DISCARDED	       = 4000h
GMEM_LOCKCOUNT	       = 0FFh
GHND		       = GMEM_MOVEABLE + GMEM_ZEROINIT
GPTR		       = GMEM_FIXED + GMEM_ZEROINIT

; Local memory flags

LMEM_FIXED	       = 0000h
LMEM_MOVEABLE	       = 0002h
LMEM_NOCOMPACT	       = 0010h
LMEM_NODISCARD	       = 0020h
LMEM_ZEROINIT	       = 0040h
LMEM_MODIFY	       = 0080h
LMEM_DISCARDABLE       = 0F00h
LMEM_VALID_FLAGS       = 0F72h
LMEM_INVALID_HANDLE    = 8000h
LHND		       = LMEM_MOVEABLE + LMEM_ZEROINIT
LPTR		       = LMEM_FIXED + LMEM_ZEROINIT
LMEM_DISCARDED	       = 4000h
LMEM_LOCKCOUNT	       = 00FFh

; Page access flags

PAGE_NOACCESS	       = 001h
PAGE_READONLY	       = 002h
PAGE_READWRITE	       = 004h
PAGE_WRITECOPY	       = 008h
PAGE_EXECUTE	       = 010h
PAGE_EXECUTE_READ      = 020h
PAGE_EXECUTE_READWRITE = 040h
PAGE_EXECUTE_WRITECOPY = 080h
PAGE_GUARD	       = 100h
PAGE_NOCACHE	       = 200h

; Memory allocation flags

MEM_COMMIT	       = 001000h
MEM_RESERVE	       = 002000h
MEM_DECOMMIT	       = 004000h
MEM_RELEASE	       = 008000h
MEM_FREE	       = 010000h
MEM_PRIVATE	       = 020000h
MEM_MAPPED	       = 040000h
MEM_RESET	       = 080000h
MEM_TOP_DOWN	       = 100000h

; Heap allocation flags

HEAP_NO_SERIALIZE	 = 1
HEAP_GENERATE_EXCEPTIONS = 4
HEAP_ZERO_MEMORY	 = 8

; Platform identifiers

VER_PLATFORM_WIN32s	   = 0
VER_PLATFORM_WIN32_WINDOWS = 1
VER_PLATFORM_WIN32_NT	   = 2

; GetBinaryType return values

SCS_32BIT_BINARY = 0
SCS_DOS_BINARY	 = 1
SCS_WOW_BINARY	 = 2
SCS_PIF_BINARY	 = 3
SCS_POSIX_BINARY = 4
SCS_OS216_BINARY = 5

; CreateProcess flags

DEBUG_PROCESS		 = 001h
DEBUG_ONLY_THIS_PROCESS  = 002h
CREATE_SUSPENDED	 = 004h
DETACHED_PROCESS	 = 008h
CREATE_NEW_CONSOLE	 = 010h
NORMAL_PRIORITY_CLASS	 = 020h
IDLE_PRIORITY_CLASS	 = 040h
HIGH_PRIORITY_CLASS	 = 080h
REALTIME_PRIORITY_CLASS  = 100h
CREATE_NEW_PROCESS_GROUP = 200h
CREATE_SEPARATE_WOW_VDM  = 800h

; Thread priority values

THREAD_BASE_PRIORITY_MIN      = -2
THREAD_BASE_PRIORITY_MAX      = 2
THREAD_BASE_PRIORITY_LOWRT    = 15
THREAD_BASE_PRIORITY_IDLE     = -15
THREAD_PRIORITY_LOWEST	      = THREAD_BASE_PRIORITY_MIN
THREAD_PRIORITY_BELOW_NORMAL  = THREAD_PRIORITY_LOWEST + 1
THREAD_PRIORITY_NORMAL	      = 0
THREAD_PRIORITY_HIGHEST       = THREAD_BASE_PRIORITY_MAX
THREAD_PRIORITY_ABOVE_NORMAL  = THREAD_PRIORITY_HIGHEST - 1
THREAD_PRIORITY_ERROR_RETURN  = 7FFFFFFFh
THREAD_PRIORITY_TIME_CRITICAL = THREAD_BASE_PRIORITY_LOWRT
THREAD_PRIORITY_IDLE	      = THREAD_BASE_PRIORITY_IDLE

; Startup flags

STARTF_USESHOWWINDOW	= 001h
STARTF_USESIZE		= 002h
STARTF_USEPOSITION	= 004h
STARTF_USECOUNTCHARS	= 008h
STARTF_USEFILLATTRIBUTE = 010h
STARTF_RUNFULLSCREEN	= 020h
STARTF_FORCEONFEEDBACK	= 040h
STARTF_FORCEOFFFEEDBACK = 080h
STARTF_USESTDHANDLES	= 100h

; Shutdown flags

SHUTDOWN_NORETRY = 1h

; LoadLibraryEx flags

DONT_RESOLVE_DLL_REFERENCES   = 1
LOAD_LIBRARY_AS_DATAFILE      = 2
LOAD_WITH_ALTERED_SEARCH_PATH = 8

; DLL entry-point calls

DLL_PROCESS_DETACH = 0
DLL_PROCESS_ATTACH = 1
DLL_THREAD_ATTACH  = 2
DLL_THREAD_DETACH  = 3

; Status codes

STATUS_WAIT_0			= 000000000h
STATUS_ABANDONED_WAIT_0 	= 000000080h
STATUS_USER_APC 		= 0000000C0h
STATUS_TIMEOUT			= 000000102h
STATUS_PENDING			= 000000103h
STATUS_DATATYPE_MISALIGNMENT	= 080000002h
STATUS_BREAKPOINT		= 080000003h
STATUS_SINGLE_STEP		= 080000004h
STATUS_ACCESS_VIOLATION 	= 0C0000005h
STATUS_IN_PAGE_ERROR		= 0C0000006h
STATUS_NO_MEMORY		= 0C0000017h
STATUS_ILLEGAL_INSTRUCTION	= 0C000001Dh
STATUS_NONCONTINUABLE_EXCEPTION = 0C0000025h
STATUS_INVALID_DISPOSITION	= 0C0000026h
STATUS_ARRAY_BOUNDS_EXCEEDED	= 0C000008Ch
STATUS_FLOAT_DENORMAL_OPERAND	= 0C000008Dh
STATUS_FLOAT_DIVIDE_BY_ZERO	= 0C000008Eh
STATUS_FLOAT_INEXACT_RESULT	= 0C000008Fh
STATUS_FLOAT_INVALID_OPERATION	= 0C0000090h
STATUS_FLOAT_OVERFLOW		= 0C0000091h
STATUS_FLOAT_STACK_CHECK	= 0C0000092h
STATUS_FLOAT_UNDERFLOW		= 0C0000093h
STATUS_INTEGER_DIVIDE_BY_ZERO	= 0C0000094h
STATUS_INTEGER_OVERFLOW 	= 0C0000095h
STATUS_PRIVILEGED_INSTRUCTION	= 0C0000096h
STATUS_STACK_OVERFLOW		= 0C00000FDh
STATUS_CONTROL_C_EXIT		= 0C000013Ah
WAIT_FAILED			= -1
WAIT_OBJECT_0			= STATUS_WAIT_0
WAIT_ABANDONED			= STATUS_ABANDONED_WAIT_0
WAIT_ABANDONED_0		= STATUS_ABANDONED_WAIT_0
WAIT_TIMEOUT			= STATUS_TIMEOUT
WAIT_IO_COMPLETION		= STATUS_USER_APC
STILL_ACTIVE			= STATUS_PENDING

; Exception codes

EXCEPTION_CONTINUABLE		= 0
EXCEPTION_NONCONTINUABLE	= 1
EXCEPTION_ACCESS_VIOLATION	= STATUS_ACCESS_VIOLATION
EXCEPTION_DATATYPE_MISALIGNMENT = STATUS_DATATYPE_MISALIGNMENT
EXCEPTION_BREAKPOINT		= STATUS_BREAKPOINT
EXCEPTION_SINGLE_STEP		= STATUS_SINGLE_STEP
EXCEPTION_ARRAY_BOUNDS_EXCEEDED = STATUS_ARRAY_BOUNDS_EXCEEDED
EXCEPTION_FLT_DENORMAL_OPERAND	= STATUS_FLOAT_DENORMAL_OPERAND
EXCEPTION_FLT_DIVIDE_BY_ZERO	= STATUS_FLOAT_DIVIDE_BY_ZERO
EXCEPTION_FLT_INEXACT_RESULT	= STATUS_FLOAT_INEXACT_RESULT
EXCEPTION_FLT_INVALID_OPERATION = STATUS_FLOAT_INVALID_OPERATION
EXCEPTION_FLT_OVERFLOW		= STATUS_FLOAT_OVERFLOW
EXCEPTION_FLT_STACK_CHECK	= STATUS_FLOAT_STACK_CHECK
EXCEPTION_FLT_UNDERFLOW 	= STATUS_FLOAT_UNDERFLOW
EXCEPTION_INT_DIVIDE_BY_ZERO	= STATUS_INTEGER_DIVIDE_BY_ZERO
EXCEPTION_INT_OVERFLOW		= STATUS_INTEGER_OVERFLOW
EXCEPTION_ILLEGAL_INSTRUCTION	= STATUS_ILLEGAL_INSTRUCTION
EXCEPTION_PRIV_INSTRUCTION	= STATUS_PRIVILEGED_INSTRUCTION
EXCEPTION_IN_PAGE_ERROR 	= STATUS_IN_PAGE_ERROR

; Registry options

REG_OPTION_RESERVED	       = 0
REG_OPTION_NON_VOLATILE        = 0
REG_OPTION_VOLATILE	       = 1
REG_OPTION_CREATE_LINK	       = 2
REG_OPTION_BACKUP_RESTORE      = 4
REG_CREATED_NEW_KEY	       = 1
REG_OPENED_EXISTING_KEY        = 2
REG_WHOLE_HIVE_VOLATILE        = 1
REG_REFRESH_HIVE	       = 2
REG_NOTIFY_CHANGE_NAME	       = 1
REG_NOTIFY_CHANGE_ATTRIBUTES   = 2
REG_NOTIFY_CHANGE_LAST_SET     = 4
REG_NOTIFY_CHANGE_SECURITY     = 8
REG_LEGAL_CHANGE_FILTER        = REG_NOTIFY_CHANGE_NAME or REG_NOTIFY_CHANGE_ATTRIBUTES or REG_NOTIFY_CHANGE_LAST_SET or REG_NOTIFY_CHANGE_SECURITY
REG_LEGAL_OPTION	       = REG_OPTION_RESERVED or REG_OPTION_NON_VOLATILE or REG_OPTION_VOLATILE or REG_OPTION_CREATE_LINK or REG_OPTION_BACKUP_RESTORE
REG_NONE		       = 0
REG_SZ			       = 1
REG_EXPAND_SZ		       = 2
REG_BINARY		       = 3
REG_DWORD		       = 4
REG_DWORD_LITTLE_ENDIAN        = 4
REG_DWORD_BIG_ENDIAN	       = 5
REG_LINK		       = 6
REG_MULTI_SZ		       = 7
REG_RESOURCE_LIST	       = 8
REG_FULL_RESOURCE_DESCRIPTOR   = 9
REG_RESOURCE_REQUIREMENTS_LIST = 10

; Registry access modes

KEY_QUERY_VALUE 	       = 1
KEY_SET_VALUE		       = 2
KEY_CREATE_SUB_KEY	       = 4
KEY_ENUMERATE_SUB_KEYS	       = 8
KEY_NOTIFY		       = 10h
KEY_CREATE_LINK 	       = 20h
KEY_READ		       = STANDARD_RIGHTS_READ or KEY_QUERY_VALUE or KEY_ENUMERATE_SUB_KEYS or KEY_NOTIFY and not SYNCHRONIZE
KEY_WRITE		       = STANDARD_RIGHTS_WRITE or KEY_SET_VALUE or KEY_CREATE_SUB_KEY and not SYNCHRONIZE
KEY_EXECUTE		       = KEY_READ
KEY_ALL_ACCESS		       = STANDARD_RIGHTS_ALL or KEY_QUERY_VALUE or KEY_SET_VALUE or KEY_CREATE_SUB_KEY or KEY_ENUMERATE_SUB_KEYS or KEY_NOTIFY or KEY_CREATE_LINK and not SYNCHRONIZE

; Predefined registry keys

HKEY_CLASSES_ROOT     = 80000000h
HKEY_CURRENT_USER     = 80000001h
HKEY_LOCAL_MACHINE    = 80000002h
HKEY_USERS	      = 80000003h
HKEY_PERFORMANCE_DATA = 80000004h
HKEY_CURRENT_CONFIG   = 80000005h
HKEY_DYN_DATA	      = 80000006h

; FormatMessage flags

FORMAT_MESSAGE_ALLOCATE_BUFFER = 0100h
FORMAT_MESSAGE_IGNORE_INSERTS  = 0200h
FORMAT_MESSAGE_FROM_STRING     = 0400h
FORMAT_MESSAGE_FROM_HMODULE    = 0800h
FORMAT_MESSAGE_FROM_SYSTEM     = 1000h
FORMAT_MESSAGE_ARGUMENT_ARRAY  = 2000h
FORMAT_MESSAGE_MAX_WIDTH_MASK  = 00FFh

; Language identifiers

LANG_NEUTRAL		     = 00h
LANG_BULGARIAN		     = 02h
LANG_CHINESE		     = 04h
LANG_CROATIAN		     = 1Ah
LANG_CZECH		     = 05h
LANG_DANISH		     = 06h
LANG_DUTCH		     = 13h
LANG_ENGLISH		     = 09h
LANG_FINNISH		     = 0Bh
LANG_FRENCH		     = 0Ch
LANG_GERMAN		     = 07h
LANG_GREEK		     = 08h
LANG_HUNGARIAN		     = 0Eh
LANG_ICELANDIC		     = 0Fh
LANG_ITALIAN		     = 10h
LANG_JAPANESE		     = 11h
LANG_KOREAN		     = 12h
LANG_NORWEGIAN		     = 14h
LANG_POLISH		     = 15h
LANG_PORTUGUESE 	     = 16h
LANG_ROMANIAN		     = 18h
LANG_RUSSIAN		     = 19h
LANG_SLOVAK		     = 1Bh
LANG_SLOVENIAN		     = 24h
LANG_SPANISH		     = 0Ah
LANG_SWEDISH		     = 1Dh
LANG_THAI		     = 1Eh
LANG_TURKISH		     = 1Fh

; Sublanguage identifiers

SUBLANG_NEUTRAL 	     = 00h shl 10
SUBLANG_DEFAULT 	     = 01h shl 10
SUBLANG_SYS_DEFAULT	     = 02h shl 10
SUBLANG_CHINESE_TRADITIONAL  = 01h shl 10
SUBLANG_CHINESE_SIMPLIFIED   = 02h shl 10
SUBLANG_CHINESE_HONGKONG     = 03h shl 10
SUBLANG_CHINESE_SINGAPORE    = 04h shl 10
SUBLANG_DUTCH		     = 01h shl 10
SUBLANG_DUTCH_BELGIAN	     = 02h shl 10
SUBLANG_ENGLISH_US	     = 01h shl 10
SUBLANG_ENGLISH_UK	     = 02h shl 10
SUBLANG_ENGLISH_AUS	     = 03h shl 10
SUBLANG_ENGLISH_CAN	     = 04h shl 10
SUBLANG_ENGLISH_NZ	     = 05h shl 10
SUBLANG_ENGLISH_EIRE	     = 06h shl 10
SUBLANG_FRENCH		     = 01h shl 10
SUBLANG_FRENCH_BELGIAN	     = 02h shl 10
SUBLANG_FRENCH_CANADIAN      = 03h shl 10
SUBLANG_FRENCH_SWISS	     = 04h shl 10
SUBLANG_GERMAN		     = 01h shl 10
SUBLANG_GERMAN_SWISS	     = 02h shl 10
SUBLANG_GERMAN_AUSTRIAN      = 03h shl 10
SUBLANG_ITALIAN 	     = 01h shl 10
SUBLANG_ITALIAN_SWISS	     = 02h shl 10
SUBLANG_NORWEGIAN_BOKMAL     = 01h shl 10
SUBLANG_NORWEGIAN_NYNORSK    = 02h shl 10
SUBLANG_PORTUGUESE	     = 02h shl 10
SUBLANG_PORTUGUESE_BRAZILIAN = 01h shl 10
SUBLANG_SPANISH 	     = 01h shl 10
SUBLANG_SPANISH_MEXICAN      = 02h shl 10
SUBLANG_SPANISH_MODERN	     = 03h shl 10

; Sorting identifiers

SORT_DEFAULT		     = 0 shl 16
SORT_JAPANESE_XJIS	     = 0 shl 16
SORT_JAPANESE_UNICODE	     = 1 shl 16
SORT_CHINESE_BIG5	     = 0 shl 16
SORT_CHINESE_PRCP	     = 0 shl 16
SORT_CHINESE_UNICODE	     = 1 shl 16
SORT_CHINESE_PRC	     = 2 shl 16
SORT_CHINESE_BOPOMOFO	     = 3 shl 16
SORT_KOREAN_KSC 	     = 0 shl 16
SORT_KOREAN_UNICODE	     = 1 shl 16
SORT_GERMAN_PHONE_BOOK	     = 1 shl 16
SORT_HUNGARIAN_DEFAULT	     = 0 shl 16
SORT_HUNGARIAN_TECHNICAL     = 1 shl 16

; Code pages

CP_ACP	      = 0	    ; default to ANSI code page
CP_OEMCP      = 1	    ; default to OEM code page
CP_MACCP      = 2	    ; default to MAC code page
CP_THREAD_ACP = 3	    ; current thread's ANSI code page
CP_SYMBOL     = 42	    ; SYMBOL translations
CP_UTF7       = 65000	    ; UTF-7 translation
CP_UTF8       = 65001	    ; UTF-8 translation

; Resource types

RT_CURSOR	= 1
RT_BITMAP	= 2
RT_ICON 	= 3
RT_MENU 	= 4
RT_DIALOG	= 5
RT_STRING	= 6
RT_FONTDIR	= 7
RT_FONT 	= 8
RT_ACCELERATOR	= 9
RT_RCDATA	= 10
RT_MESSAGETABLE = 11
RT_GROUP_CURSOR = 12
RT_GROUP_ICON	= 14
RT_VERSION	= 16
RT_DLGINCLUDE	= 17
RT_PLUGPLAY	= 19
RT_VXD		= 20
RT_ANICURSOR	= 21
RT_ANIICON	= 22
RT_HTML 	= 23
RT_MANIFEST	= 24

; Clipboard formats

CF_TEXT 	   = 001h
CF_BITMAP	   = 002h
CF_METAFILEPICT    = 003h
CF_SYLK 	   = 004h
CF_DIF		   = 005h
CF_TIFF 	   = 006h
CF_OEMTEXT	   = 007h
CF_DIB		   = 008h
CF_PALETTE	   = 009h
CF_PENDATA	   = 00Ah
CF_RIFF 	   = 00Bh
CF_WAVE 	   = 00Ch
CF_UNICODETEXT	   = 00Dh
CF_ENHMETAFILE	   = 00Eh
CF_HDROP	   = 00Fh
CF_LOCALE	   = 010h
CF_OWNERDISPLAY    = 080h
CF_DSPTEXT	   = 081h
CF_DSPBITMAP	   = 082h
CF_DSPMETAFILEPICT = 083h
CF_DSPENHMETAFILE  = 08Eh
CF_PRIVATEFIRST    = 200h
CF_PRIVATELAST	   = 2FFh
CF_GDIOBJFIRST	   = 300h
CF_GDIOBJLAST	   = 3FFh

; OS types for version info

VOS_UNKNOWN	  = 00000000h
VOS_DOS 	  = 00010000h
VOS_OS216	  = 00020000h
VOS_OS232	  = 00030000h
VOS_NT		  = 00040000h
VOS__BASE	  = 00000000h
VOS__WINDOWS16	  = 00000001h
VOS__PM16	  = 00000002h
VOS__PM32	  = 00000003h
VOS__WINDOWS32	  = 00000004h
VOS_DOS_WINDOWS16 = 00010001h
VOS_DOS_WINDOWS32 = 00010004h
VOS_OS216_PM16	  = 00020002h
VOS_OS232_PM32	  = 00030003h
VOS_NT_WINDOWS32  = 00040004h

; File types for version info

VFT_UNKNOWN    = 00000000h
VFT_APP        = 00000001h
VFT_DLL        = 00000002h
VFT_DRV        = 00000003h
VFT_FONT       = 00000004h
VFT_VXD        = 00000005h
VFT_STATIC_LIB = 00000007h

; File subtypes for version info

VFT2_UNKNOWN		   = 00000000h
VFT2_DRV_PRINTER	   = 00000001h
VFT2_DRV_KEYBOARD	   = 00000002h
VFT2_DRV_LANGUAGE	   = 00000003h
VFT2_DRV_DISPLAY	   = 00000004h
VFT2_DRV_MOUSE		   = 00000005h
VFT2_DRV_NETWORK	   = 00000006h
VFT2_DRV_SYSTEM 	   = 00000007h
VFT2_DRV_INSTALLABLE	   = 00000008h
VFT2_DRV_SOUND		   = 00000009h
VFT2_DRV_COMM		   = 0000000Ah
VFT2_DRV_INPUTMETHOD	   = 0000000Bh
VFT2_DRV_VERSIONED_PRINTER = 0000000Ch
VFT2_FONT_RASTER	   = 00000001h
VFT2_FONT_VECTOR	   = 00000002h
VFT2_FONT_TRUETYPE	   = 00000003h

; Console control signals

CTRL_C_EVENT	    = 0
CTRL_BREAK_EVENT    = 1
CTRL_CLOSE_EVENT    = 2
CTRL_LOGOFF_EVENT   = 5
CTRL_SHUTDOWN_EVENT = 6

; Standard file handles

STD_INPUT_HANDLE       = 0FFFFFFF6h
STD_OUTPUT_HANDLE      = 0FFFFFFF5h
STD_ERROR_HANDLE       = 0FFFFFFF4h


; USER32.DLL structures and constants

struct POINT
  x dd ?
  y dd ?
ends

struct RECT
  left	 dd ?
  top	 dd ?
  right  dd ?
  bottom dd ?
ends

struct WNDCLASS
  style 	dd ?
  lpfnWndProc	dd ?
  cbClsExtra	dd ?
  cbWndExtra	dd ?
  hInstance	dd ?
  hIcon 	dd ?
  hCursor	dd ?
  hbrBackground dd ?
  lpszMenuName	dd ?
  lpszClassName dd ?
ends

struct WNDCLASSEX
  cbSize	dd ?
  style 	dd ?
  lpfnWndProc	dd ?
  cbClsExtra	dd ?
  cbWndExtra	dd ?
  hInstance	dd ?
  hIcon 	dd ?
  hCursor	dd ?
  hbrBackground dd ?
  lpszMenuName	dd ?
  lpszClassName dd ?
  hIconSm	dd ?
ends

struct CREATESTRUCT
  lpCreateParams dd ?
  hInstance	 dd ?
  hMenu 	 dd ?
  hwndParent	 dd ?
  cy		 dd ?
  cx		 dd ?
  y		 dd ?
  x		 dd ?
  style 	 dd ?
  lpszName	 dd ?
  lpszClass	 dd ?
  dwExStyle	 dd ?
ends

struct CLIENTCREATESTRUCT
  hWindowMenu  dd ?
  idFirstChild dd ?
ends

struct MDICREATESTRUCT
  szClass dd ?
  szTitle dd ?
  hOwner  dd ?
  x	  dd ?
  y	  dd ?
  cx	  dd ?
  cy	  dd ?
  style   dd ?
  lParam  dd ?
ends

struct SCROLLINFO
  cbSize    dd ?
  fMask     dd ?
  nMin	    dd ?
  nMax	    dd ?
  nPage     dd ?
  nPos	    dd ?
  nTrackPos dd ?
ends

struct MSG
  hwnd	  dd ?
  message dd ?
  wParam  dd ?
  lParam  dd ?
  time	  dd ?
  pt	  POINT
ends

struct MINMAXINFO
  ptReserved	 POINT
  ptMaxSize	 POINT
  ptMaxPosition  POINT
  ptMinTrackSize POINT
  ptMaxTrackSize POINT
ends

struct WINDOWPLACEMENT
  length	   dd ?
  flags 	   dd ?
  showCmd	   dd ?
  ptMinPosition    POINT
  ptMaxPosition    POINT
  rcNormalPosition RECT
ends

struct WINDOWPOS
  hwnd		  dd ?
  hwndInsertAfter dd ?
  x		  dd ?
  y		  dd ?
  cx		  dd ?
  cy		  dd ?
  flags 	  dd ?
ends

struct NMHDR
  hwndFrom dd ?
  idFrom   dd ?
  code	   dd ?
ends

struct COPYDATASTRUCT
  dwData dd ?
  cbData dd ?
  lpData dd ?
ends

struct ACCEL
  fVirt dw ?
  key	dw ?
  cmd	dw ?
ends

struct PAINTSTRUCT
  hdc	      dd ?
  fErase      dd ?
  rcPaint     RECT
  fRestore    dd ?
  fIncUpdate  dd ?
  rgbReserved db 32 dup (?)
ends

struct DRAWTEXTPARAMS
  cbSize	dd ?
  iTabLength	dd ?
  iLeftMargin	dd ?
  iRightMargin	dd ?
  uiLengthDrawn dd ?
ends

struct DRAWITEMSTRUCT
  CtlType    dd ?
  CtlID      dd ?
  itemID     dd ?
  itemAction dd ?
  itemState  dd ?
  hwndItem   dd ?
  hDC	     dd ?
  rcItem     RECT
  itemData   dd ?
ends

struct MENUITEMINFO
  cbSize	dd ?
  fMask 	dd ?
  fType 	dd ?
  fState	dd ?
  wID		dd ?
  hSubMenu	dd ?
  hbmpChecked	dd ?
  hbmpUnchecked dd ?
  dwItemData	dd ?
  dwTypeData	dd ?
  cch		dd ?
ends

struct MEASUREITEMSTRUCT
  CtlType    dd ?
  CtlID      dd ?
  itemID     dd ?
  itemWidth  dd ?
  itemHeight dd ?
  itemData   dd ?
ends

struct MSGBOXPARAMS
  cbSize	     dd ?
  hwndOwner	     dd ?
  hInstance	     dd ?
  lpszText	     dd ?
  lpszCaption	     dd ?
  dwStyle	     dd ?
  lpszIcon	     dd ?
  dwContextHelpId    dd ?
  lpfnMsgBoxCallback dd ?
  dwLanguageId	     dd ?
ends

; MessageBox type flags

MB_OK			= 000000h
MB_OKCANCEL		= 000001h
MB_ABORTRETRYIGNORE	= 000002h
MB_YESNOCANCEL		= 000003h
MB_YESNO		= 000004h
MB_RETRYCANCEL		= 000005h
MB_ICONHAND		= 000010h
MB_ICONQUESTION 	= 000020h
MB_ICONEXCLAMATION	= 000030h
MB_ICONASTERISK 	= 000040h
MB_USERICON		= 000080h
MB_ICONWARNING		= MB_ICONEXCLAMATION
MB_ICONERROR		= MB_ICONHAND
MB_ICONINFORMATION	= MB_ICONASTERISK
MB_ICONSTOP		= MB_ICONHAND
MB_DEFBUTTON1		= 000000h
MB_DEFBUTTON2		= 000100h
MB_DEFBUTTON3		= 000200h
MB_DEFBUTTON4		= 000300h
MB_APPLMODAL		= 000000h
MB_SYSTEMMODAL		= 001000h
MB_TASKMODAL		= 002000h
MB_HELP 		= 004000h
MB_NOFOCUS		= 008000h
MB_SETFOREGROUND	= 010000h
MB_DEFAULT_DESKTOP_ONLY = 020000h
MB_TOPMOST		= 040000h
MB_RIGHT		= 080000h
MB_RTLREADING		= 100000h
MB_SERVICE_NOTIFICATION = 200000h

; Conventional dialog box and message box command IDs

IDOK	 = 1
IDCANCEL = 2
IDABORT  = 3
IDRETRY  = 4
IDIGNORE = 5
IDYES	 = 6
IDNO	 = 7
IDCLOSE  = 8
IDHELP	 = 9

; Class styles

CS_VREDRAW	   = 00001h
CS_HREDRAW	   = 00002h
CS_KEYCVTWINDOW    = 00004h
CS_DBLCLKS	   = 00008h
CS_OWNDC	   = 00020h
CS_CLASSDC	   = 00040h
CS_PARENTDC	   = 00080h
CS_NOKEYCVT	   = 00100h
CS_SAVEBITS	   = 00800h
CS_NOCLOSE	   = 00200h
CS_BYTEALIGNCLIENT = 01000h
CS_BYTEALIGNWINDOW = 02000h
CS_PUBLICCLASS	   = 04000h
CS_GLOBALCLASS	   = CS_PUBLICCLASS
CS_IME		   = 10000h

; Windows styles

WS_OVERLAPPED	= 000000000h
WS_ICONICPOPUP	= 0C0000000h
WS_POPUP	= 080000000h
WS_CHILD	= 040000000h
WS_MINIMIZE	= 020000000h
WS_VISIBLE	= 010000000h
WS_DISABLED	= 008000000h
WS_CLIPSIBLINGS = 004000000h
WS_CLIPCHILDREN = 002000000h
WS_MAXIMIZE	= 001000000h
WS_CAPTION	= 000C00000h
WS_BORDER	= 000800000h
WS_DLGFRAME	= 000400000h
WS_VSCROLL	= 000200000h
WS_HSCROLL	= 000100000h
WS_SYSMENU	= 000080000h
WS_THICKFRAME	= 000040000h
WS_HREDRAW	= 000020000h
WS_VREDRAW	= 000010000h
WS_GROUP	= 000020000h
WS_TABSTOP	= 000010000h
WS_MINIMIZEBOX	= 000020000h
WS_MAXIMIZEBOX	= 000010000h

; Common Window Styles

WS_OVERLAPPEDWINDOW = WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or WS_THICKFRAME or WS_MINIMIZEBOX or WS_MAXIMIZEBOX
WS_POPUPWINDOW	    = WS_POPUP or WS_BORDER or WS_SYSMENU
WS_CHILDWINDOW	    = WS_CHILD
WS_TILEDWINDOW	    = WS_OVERLAPPEDWINDOW
WS_TILED	    = WS_OVERLAPPED
WS_ICONIC	    = WS_MINIMIZE
WS_SIZEBOX	    = WS_THICKFRAME

; Extended Window Styles

WS_EX_DLGMODALFRAME    = 00001h
WS_EX_DRAGOBJECT       = 00002h
WS_EX_NOPARENTNOTIFY   = 00004h
WS_EX_TOPMOST	       = 00008h
WS_EX_ACCEPTFILES      = 00010h
WS_EX_TRANSPARENT      = 00020h
WS_EX_MDICHILD	       = 00040h
WS_EX_TOOLWINDOW       = 00080h
WS_EX_WINDOWEDGE       = 00100h
WS_EX_CLIENTEDGE       = 00200h
WS_EX_CONTEXTHELP      = 00400h
WS_EX_RIGHT	       = 01000h
WS_EX_LEFT	       = 00000h
WS_EX_RTLREADING       = 02000h
WS_EX_LTRREADING       = 00000h
WS_EX_LEFTSCROLLBAR    = 04000h
WS_EX_RIGHTSCROLLBAR   = 00000h
WS_EX_CONTROLPARENT    = 10000h
WS_EX_STATICEDGE       = 20000h
WS_EX_APPWINDOW        = 40000h
WS_EX_LAYERED	       = 80000h
WS_EX_OVERLAPPEDWINDOW = WS_EX_WINDOWEDGE or WS_EX_CLIENTEDGE
WS_EX_PALETTEWINDOW    = WS_EX_WINDOWEDGE or WS_EX_TOOLWINDOW or WS_EX_TOPMOST

; MDI client style bits

MDIS_ALLCHILDSTYLES = 1

; Special CreateWindow position value

CW_USEDEFAULT = 8000h

; Predefined window handle

HWND_DESKTOP   = 0

; ShowWindow commands

SW_HIDE 	   = 0
SW_SHOWNORMAL	   = 1
SW_NORMAL	   = 1
SW_SHOWMINIMIZED   = 2
SW_SHOWMAXIMIZED   = 3
SW_MAXIMIZE	   = 3
SW_SHOWNOACTIVATE  = 4
SW_SHOW 	   = 5
SW_MINIMIZE	   = 6
SW_SHOWMINNOACTIVE = 7
SW_SHOWNA	   = 8
SW_RESTORE	   = 9
SW_SHOWDEFAULT	   = 10

; SetWindowPos flags

SWP_NOSIZE	   = 0001h
SWP_NOMOVE	   = 0002h
SWP_NOZORDER	   = 0004h
SWP_NOREDRAW	   = 0008h
SWP_NOACTIVATE	   = 0010h
SWP_DRAWFRAME	   = 0020h
SWP_SHOWWINDOW	   = 0040h
SWP_HIDEWINDOW	   = 0080h
SWP_NOCOPYBITS	   = 0100h
SWP_NOREPOSITION   = 0200h
SWP_DEFERERASE	   = 2000h
SWP_ASYNCWINDOWPOS = 4000h

; SetWindowPos special handle values

HWND_TOP       = 0
HWND_BOTTOM    = 1
HWND_TOPMOST   = -1
HWND_NOTOPMOST = -2

; GetWindow flags

GW_HWNDFIRST = 0
GW_HWNDLAST  = 1
GW_HWNDNEXT  = 2
GW_HWNDPREV  = 3
GW_OWNER     = 4
GW_CHILD     = 5

; RedrawWindow flags

RDW_INVALIDATE	    = 0001h
RDW_INTERNALPAINT   = 0002h
RDW_ERASE	    = 0004h
RDW_VALIDATE	    = 0008h
RDW_NOINTERNALPAINT = 0010h
RDW_NOERASE	    = 0020h
RDW_NOCHILDREN	    = 0040h
RDW_ALLCHILDREN     = 0080h
RDW_UPDATENOW	    = 0100h
RDW_ERASENOW	    = 0200h
RDW_FRAME	    = 0400h
RDW_NOFRAME	    = 0800h

; PeekMessage Options

PM_NOREMOVE = 0000h
PM_REMOVE   = 0001h
PM_NOYIELD  = 0002h

; Window state messages

WM_STATE		  = 0000h
WM_NULL 		  = 0000h
WM_CREATE		  = 0001h
WM_DESTROY		  = 0002h
WM_MOVE 		  = 0003h
WM_SIZE 		  = 0005h
WM_ACTIVATE		  = 0006h
WM_SETFOCUS		  = 0007h
WM_KILLFOCUS		  = 0008h
WM_ENABLE		  = 000Ah
WM_SETREDRAW		  = 000Bh
WM_SETTEXT		  = 000Ch
WM_GETTEXT		  = 000Dh
WM_GETTEXTLENGTH	  = 000Eh
WM_PAINT		  = 000Fh
WM_CLOSE		  = 0010h
WM_QUERYENDSESSION	  = 0011h
WM_QUIT 		  = 0012h
WM_QUERYOPEN		  = 0013h
WM_ERASEBKGND		  = 0014h
WM_SYSCOLORCHANGE	  = 0015h
WM_ENDSESSION		  = 0016h
WM_SYSTEMERROR		  = 0017h
WM_SHOWWINDOW		  = 0018h
WM_CTLCOLOR		  = 0019h
WM_WININICHANGE 	  = 001Ah
WM_DEVMODECHANGE	  = 001Bh
WM_ACTIVATEAPP		  = 001Ch
WM_FONTCHANGE		  = 001Dh
WM_TIMECHANGE		  = 001Eh
WM_CANCELMODE		  = 001Fh
WM_SETCURSOR		  = 0020h
WM_MOUSEACTIVATE	  = 0021h
WM_CHILDACTIVATE	  = 0022h
WM_QUEUESYNC		  = 0023h
WM_GETMINMAXINFO	  = 0024h
WM_PAINTICON		  = 0026h
WM_ICONERASEBKGND	  = 0027h
WM_NEXTDLGCTL		  = 0028h
WM_SPOOLERSTATUS	  = 002Ah
WM_DRAWITEM		  = 002Bh
WM_MEASUREITEM		  = 002Ch
WM_DELETEITEM		  = 002Dh
WM_VKEYTOITEM		  = 002Eh
WM_CHARTOITEM		  = 002Fh
WM_SETFONT		  = 0030h
WM_GETFONT		  = 0031h
WM_SETHOTKEY		  = 0032h
WM_QUERYDRAGICON	  = 0037h
WM_COMPAREITEM		  = 0039h
WM_COMPACTING		  = 0041h
WM_COMMNOTIFY		  = 0044h
WM_WINDOWPOSCHANGING	  = 0046h
WM_WINDOWPOSCHANGED	  = 0047h
WM_POWER		  = 0048h
WM_COPYDATA		  = 004Ah
WM_CANCELJOURNAL	  = 004Bh
WM_NOTIFY		  = 004Eh
WM_INPUTLANGCHANGEREQUEST = 0050h
WM_INPUTLANGCHANGE	  = 0051h
WM_TCARD		  = 0052h
WM_HELP 		  = 0053h
WM_USERCHANGED		  = 0054h
WM_NOTIFYFORMAT 	  = 0055h
WM_CONTEXTMENU		  = 007Bh
WM_STYLECHANGING	  = 007Ch
WM_STYLECHANGED 	  = 007Dh
WM_DISPLAYCHANGE	  = 007Eh
WM_GETICON		  = 007Fh
WM_SETICON		  = 0080h
WM_NCCREATE		  = 0081h
WM_NCDESTROY		  = 0082h
WM_NCCALCSIZE		  = 0083h
WM_NCHITTEST		  = 0084h
WM_NCPAINT		  = 0085h
WM_NCACTIVATE		  = 0086h
WM_GETDLGCODE		  = 0087h
WM_NCMOUSEMOVE		  = 00A0h
WM_NCLBUTTONDOWN	  = 00A1h
WM_NCLBUTTONUP		  = 00A2h
WM_NCLBUTTONDBLCLK	  = 00A3h
WM_NCRBUTTONDOWN	  = 00A4h
WM_NCRBUTTONUP		  = 00A5h
WM_NCRBUTTONDBLCLK	  = 00A6h
WM_NCMBUTTONDOWN	  = 00A7h
WM_NCMBUTTONUP		  = 00A8h
WM_NCMBUTTONDBLCLK	  = 00A9h
WM_KEYFIRST		  = 0100h
WM_KEYDOWN		  = 0100h
WM_KEYUP		  = 0101h
WM_CHAR 		  = 0102h
WM_DEADCHAR		  = 0103h
WM_SYSKEYDOWN		  = 0104h
WM_SYSKEYUP		  = 0105h
WM_SYSCHAR		  = 0106h
WM_SYSDEADCHAR		  = 0107h
WM_KEYLAST		  = 0108h
WM_INITDIALOG		  = 0110h
WM_COMMAND		  = 0111h
WM_SYSCOMMAND		  = 0112h
WM_TIMER		  = 0113h
WM_HSCROLL		  = 0114h
WM_VSCROLL		  = 0115h
WM_INITMENU		  = 0116h
WM_INITMENUPOPUP	  = 0117h
WM_MENUSELECT		  = 011Fh
WM_MENUCHAR		  = 0120h
WM_ENTERIDLE		  = 0121h
WM_MENURBUTTONUP	  = 0122h
WM_MENUDRAG		  = 0123h
WM_MENUGETOBJECT	  = 0124h
WM_UNINITMENUPOPUP	  = 0125h
WM_MENUCOMMAND		  = 0126h
WM_CTLCOLORMSGBOX	  = 0132h
WM_CTLCOLOREDIT 	  = 0133h
WM_CTLCOLORLISTBOX	  = 0134h
WM_CTLCOLORBTN		  = 0135h
WM_CTLCOLORDLG		  = 0136h
WM_CTLCOLORSCROLLBAR	  = 0137h
WM_CTLCOLORSTATIC	  = 0138h
WM_MOUSEFIRST		  = 0200h
WM_MOUSEMOVE		  = 0200h
WM_LBUTTONDOWN		  = 0201h
WM_LBUTTONUP		  = 0202h
WM_LBUTTONDBLCLK	  = 0203h
WM_RBUTTONDOWN		  = 0204h
WM_RBUTTONUP		  = 0205h
WM_RBUTTONDBLCLK	  = 0206h
WM_MBUTTONDOWN		  = 0207h
WM_MBUTTONUP		  = 0208h
WM_MBUTTONDBLCLK	  = 0209h
WM_MOUSEWHEEL		  = 020Ah
WM_MOUSELAST		  = 020Ah
WM_PARENTNOTIFY 	  = 0210h
WM_ENTERMENULOOP	  = 0211h
WM_EXITMENULOOP 	  = 0212h
WM_NEXTMENU		  = 0213h
WM_SIZING		  = 0214h
WM_CAPTURECHANGED	  = 0215h
WM_MOVING		  = 0216h
WM_POWERBROADCAST	  = 0218h
WM_DEVICECHANGE 	  = 0219h
WM_MDICREATE		  = 0220h
WM_MDIDESTROY		  = 0221h
WM_MDIACTIVATE		  = 0222h
WM_MDIRESTORE		  = 0223h
WM_MDINEXT		  = 0224h
WM_MDIMAXIMIZE		  = 0225h
WM_MDITILE		  = 0226h
WM_MDICASCADE		  = 0227h
WM_MDIICONARRANGE	  = 0228h
WM_MDIGETACTIVE 	  = 0229h
WM_MDISETMENU		  = 0230h
WM_ENTERSIZEMOVE	  = 0231h
WM_EXITSIZEMOVE 	  = 0232h
WM_DROPFILES		  = 0233h
WM_MDIREFRESHMENU	  = 0234h
WM_IME_SETCONTEXT	  = 0281h
WM_IME_NOTIFY		  = 0282h
WM_IME_CONTROL		  = 0283h
WM_IME_COMPOSITIONFULL	  = 0284h
WM_IME_SELECT		  = 0285h
WM_IME_CHAR		  = 0286h
WM_IME_KEYDOWN		  = 0290h
WM_IME_KEYUP		  = 0291h
WM_MOUSEHOVER		  = 02A1h
WM_MOUSELEAVE		  = 02A3h
WM_CUT			  = 0300h
WM_COPY 		  = 0301h
WM_PASTE		  = 0302h
WM_CLEAR		  = 0303h
WM_UNDO 		  = 0304h
WM_RENDERFORMAT 	  = 0305h
WM_RENDERALLFORMATS	  = 0306h
WM_DESTROYCLIPBOARD	  = 0307h
WM_DRAWCLIPBOARD	  = 0308h
WM_PAINTCLIPBOARD	  = 0309h
WM_VSCROLLCLIPBOARD	  = 030Ah
WM_SIZECLIPBOARD	  = 030Bh
WM_ASKCBFORMATNAME	  = 030Ch
WM_CHANGECBCHAIN	  = 030Dh
WM_HSCROLLCLIPBOARD	  = 030Eh
WM_QUERYNEWPALETTE	  = 030Fh
WM_PALETTEISCHANGING	  = 0310h
WM_PALETTECHANGED	  = 0311h
WM_HOTKEY		  = 0312h
WM_PRINT		  = 0317h
WM_PRINTCLIENT		  = 0318h
WM_HANDHELDFIRST	  = 0358h
WM_HANDHELDLAST 	  = 035Fh
WM_AFXFIRST		  = 0360h
WM_AFXLAST		  = 037Fh
WM_PENWINFIRST		  = 0380h
WM_PENWINLAST		  = 038Fh
WM_COALESCE_FIRST	  = 0390h
WM_COALESCE_LAST	  = 039Fh
WM_USER 		  = 0400h

; WM_SIZE commands

SIZE_RESTORED  = 0
SIZE_MINIMIZED = 1
SIZE_MAXIMIZED = 2
SIZE_MAXSHOW   = 3
SIZE_MAXHIDE   = 4

; WM_ACTIVATE states

WA_INACTIVE    = 0
WA_ACTIVE      = 1
WA_CLICKACTIVE = 2

; WM_SHOWWINDOW identifiers

SW_PARENTCLOSING = 1
SW_OTHERZOOM	 = 2
SW_PARENTOPENING = 3
SW_OTHERUNZOOM	 = 4

; WM_MOUSEACTIVATE return codes

MA_ACTIVATE	    = 1
MA_ACTIVATEANDEAT   = 2
MA_NOACTIVATE	    = 3
MA_NOACTIVATEANDEAT = 4

; WM_MDITILE flags

MDITILE_VERTICAL     = 0
MDITILE_HORIZONTAL   = 1
MDITILE_SKIPDISABLED = 2

; WM_NOTIFY codes

NM_OUTOFMEMORY = -1
NM_CLICK       = -2
NM_DBLCLICK    = -3
NM_RETURN      = -4
NM_RCLICK      = -5
NM_RDBLCLK     = -6
NM_SETFOCUS    = -7
NM_KILLFOCUS   = -8

; WM_SETICON types

ICON_SMALL = 0
ICON_BIG   = 1

; WM_HOTKEY commands

HOTKEYF_SHIFT	= 01h
HOTKEYF_CONTROL = 02h
HOTKEYF_ALT	= 04h
HOTKEYF_EXT	= 08h

; Keystroke flags

KF_EXTENDED = 0100h
KF_DLGMODE  = 0800h
KF_MENUMODE = 1000h
KF_ALTDOWN  = 2000h
KF_REPEAT   = 4000h
KF_UP	    = 8000h

; Key state masks for mouse messages

MK_LBUTTON = 01h
MK_RBUTTON = 02h
MK_SHIFT   = 04h
MK_CONTROL = 08h
MK_MBUTTON = 10h

; WM_SIZING codes

WMSZ_LEFT	 = 1
WMSZ_RIGHT	 = 2
WMSZ_TOP	 = 3
WMSZ_TOPLEFT	 = 4
WMSZ_TOPRIGHT	 = 5
WMSZ_BOTTOM	 = 6
WMSZ_BOTTOMLEFT  = 7
WMSZ_BOTTOMRIGHT = 8

; WM_HOTKEY modifiers

MOD_ALT     = 1
MOD_CONTROL = 2
MOD_SHIFT   = 4
MOD_WIN     = 8

; WM_PRINT flags

PRF_CHECKVISIBLE = 01h
PRF_NONCLIENT	 = 02h
PRF_CLIENT	 = 04h
PRF_ERASEBKGND	 = 08h
PRF_CHILDREN	 = 10h
PRF_OWNED	 = 20h

; Virtual key codes

VK_LBUTTON   = 001h
VK_CANCEL    = 003h
VK_RBUTTON   = 002h
VK_MBUTTON   = 004h
VK_BACK      = 008h
VK_TAB	     = 009h
VK_CLEAR     = 00Ch
VK_RETURN    = 00Dh
VK_SHIFT     = 010h
VK_CONTROL   = 011h
VK_MENU      = 012h
VK_PAUSE     = 013h
VK_CAPITAL   = 014h
VK_ESCAPE    = 01Bh
VK_SPACE     = 020h
VK_PRIOR     = 021h
VK_PGUP      = 021h
VK_PGDN      = 022h
VK_NEXT      = 022h
VK_END	     = 023h
VK_HOME      = 024h
VK_LEFT      = 025h
VK_UP	     = 026h
VK_RIGHT     = 027h
VK_DOWN      = 028h
VK_SELECT    = 029h
VK_PRINT     = 02Ah
VK_EXECUTE   = 02Bh
VK_SNAPSHOT  = 02Ch
VK_INSERT    = 02Dh
VK_DELETE    = 02Eh
VK_HELP      = 02Fh
VK_LWIN      = 05Bh
VK_RWIN      = 05Ch
VK_APPS      = 05Dh
VK_NUMPAD0   = 060h
VK_NUMPAD1   = 061h
VK_NUMPAD2   = 062h
VK_NUMPAD3   = 063h
VK_NUMPAD4   = 064h
VK_NUMPAD5   = 065h
VK_NUMPAD6   = 066h
VK_NUMPAD7   = 067h
VK_NUMPAD8   = 068h
VK_NUMPAD9   = 069h
VK_MULTIPLY  = 06Ah
VK_ADD	     = 06Bh
VK_SEPARATOR = 06Ch
VK_SUBTRACT  = 06Dh
VK_DECIMAL   = 06Eh
VK_DIVIDE    = 06Fh
VK_F1	     = 070h
VK_F2	     = 071h
VK_F3	     = 072h
VK_F4	     = 073h
VK_F5	     = 074h
VK_F6	     = 075h
VK_F7	     = 076h
VK_F8	     = 077h
VK_F9	     = 078h
VK_F10	     = 079h
VK_F11	     = 07Ah
VK_F12	     = 07Bh
VK_F13	     = 07Ch
VK_F14	     = 07Dh
VK_F15	     = 07Eh
VK_F16	     = 07Fh
VK_F17	     = 080h
VK_F18	     = 081h
VK_F19	     = 082h
VK_F20	     = 083h
VK_F21	     = 084h
VK_F22	     = 085h
VK_F23	     = 086h
VK_F24	     = 087h
VK_NUMLOCK   = 090h
VK_SCROLL    = 091h
VK_LSHIFT    = 0A0h
VK_RSHIFT    = 0A1h
VK_LCONTROL  = 0A2h
VK_RCONTROL  = 0A3h
VK_LMENU     = 0A4h
VK_RMENU     = 0A5h
VK_ATTN      = 0F6h
VK_CRSEL     = 0F7h
VK_EXSEL     = 0F8h
VK_EREOF     = 0F9h
VK_PLAY      = 0FAh
VK_ZOOM      = 0FBh
VK_NONAME    = 0FCh
VK_PA1	     = 0FDh
VK_OEM_CLEAR = 0FEh

; Accelerator flags

FVIRTKEY  = 01h
FNOINVERT = 02h
FSHIFT	  = 04h
FCONTROL  = 08h
FALT	  = 10h

; GetClassLong offsets

GCL_MENUNAME	  = -8
GCL_HBRBACKGROUND = -10
GCL_HCURSOR	  = -12
GCL_HICON	  = -14
GCL_HMODULE	  = -16
GCL_CBWNDEXTRA	  = -18
GCL_CBCLSEXTRA	  = -20
GCL_WNDPROC	  = -24
GCL_STYLE	  = -26
GCW_ATOM	  = -32
GCL_HICONSM	  = -34

; WNDCLASS parameters

DLGWINDOWEXTRA = 30

; GetWindowLong offsets

GWL_WNDPROC	  = -4
GWL_HINSTANCE	  = -6
GWL_HWNDPARENT	  = -8
GWL_STYLE	  = -16
GWL_EXSTYLE	  = -20
GWL_USERDATA	  = -21
GWL_ID		  = -12
DWL_MSGRESULT	  = 0
DWL_DLGPROC	  = 4
DWL_USER	  = 8

; GetSystemMetrics codes

SM_CXSCREEN	     = 0
SM_CYSCREEN	     = 1
SM_CXVSCROLL	     = 2
SM_CYHSCROLL	     = 3
SM_CYCAPTION	     = 4
SM_CXBORDER	     = 5
SM_CYBORDER	     = 6
SM_CXDLGFRAME	     = 7
SM_CYDLGFRAME	     = 8
SM_CYVTHUMB	     = 9
SM_CXHTHUMB	     = 10
SM_CXICON	     = 11
SM_CYICON	     = 12
SM_CXCURSOR	     = 13
SM_CYCURSOR	     = 14
SM_CYMENU	     = 15
SM_CXFULLSCREEN      = 16
SM_CYFULLSCREEN      = 17
SM_CYKANJIWINDOW     = 18
SM_MOUSEPRESENT      = 19
SM_CYVSCROLL	     = 20
SM_CXHSCROLL	     = 21
SM_DEBUG	     = 22
SM_SWAPBUTTON	     = 23
SM_RESERVED1	     = 24
SM_RESERVED2	     = 25
SM_RESERVED3	     = 26
SM_RESERVED4	     = 27
SM_CXMIN	     = 28
SM_CYMIN	     = 29
SM_CXSIZE	     = 30
SM_CYSIZE	     = 31
SM_CXFRAME	     = 32
SM_CYFRAME	     = 33
SM_CXMINTRACK	     = 34
SM_CYMINTRACK	     = 35
SM_CXDOUBLECLK	     = 36
SM_CYDOUBLECLK	     = 37
SM_CXICONSPACING     = 38
SM_CYICONSPACING     = 39
SM_MENUDROPALIGNMENT = 40
SM_PENWINDOWS	     = 41
SM_DBCSENABLED	     = 42
SM_CMOUSEBUTTONS     = 43
SM_CXFIXEDFRAME      = SM_CXDLGFRAME
SM_CYFIXEDFRAME      = SM_CYDLGFRAME
SM_CXSIZEFRAME	     = SM_CXFRAME
SM_CYSIZEFRAME	     = SM_CYFRAME
SM_SECURE	     = 44
SM_CXEDGE	     = 45
SM_CYEDGE	     = 46
SM_CXMINSPACING      = 47
SM_CYMINSPACING      = 48
SM_CXSMICON	     = 49
SM_CYSMICON	     = 50
SM_CYSMCAPTION	     = 51
SM_CXSMSIZE	     = 52
SM_CYSMSIZE	     = 53
SM_CXMENUSIZE	     = 54
SM_CYMENUSIZE	     = 55
SM_ARRANGE	     = 56
SM_CXMINIMIZED	     = 57
SM_CYMINIMIZED	     = 58
SM_CXMAXTRACK	     = 59
SM_CYMAXTRACK	     = 60
SM_CXMAXIMIZED	     = 61
SM_CYMAXIMIZED	     = 62
SM_NETWORK	     = 63
SM_CLEANBOOT	     = 67
SM_CXDRAG	     = 68
SM_CYDRAG	     = 69
SM_SHOWSOUNDS	     = 70
SM_CXMENUCHECK	     = 71
SM_CYMENUCHECK	     = 72
SM_SLOWMACHINE	     = 73
SM_MIDEASTENABLED    = 74
SM_MOUSEWHEELPRESENT = 75
SM_CMETRICS	     = 76

; Predefined cursor identifiers

IDC_ARROW	= 32512
IDC_IBEAM	= 32513
IDC_WAIT	= 32514
IDC_CROSS	= 32515
IDC_UPARROW	= 32516
IDC_SIZE	= 32640
IDC_ICON	= 32641
IDC_SIZENWSE	= 32642
IDC_SIZENESW	= 32643
IDC_SIZEWE	= 32644
IDC_SIZENS	= 32645
IDC_NO		= 32648
IDC_HAND	= 32649
IDC_APPSTARTING = 32650
IDC_HELP	= 32651

; Predefined icon identifiers

IDI_APPLICATION = 32512
IDI_HAND	= 32513
IDI_QUESTION	= 32514
IDI_EXCLAMATION = 32515
IDI_ASTERISK	= 32516
IDI_WINLOGO	= 32517

; System colors

COLOR_SCROLLBAR 	      = 0
COLOR_BACKGROUND	      = 1
COLOR_ACTIVECAPTION	      = 2
COLOR_INACTIVECAPTION	      = 3
COLOR_MENU		      = 4
COLOR_WINDOW		      = 5
COLOR_WINDOWFRAME	      = 6
COLOR_MENUTEXT		      = 7
COLOR_WINDOWTEXT	      = 8
COLOR_CAPTIONTEXT	      = 9
COLOR_ACTIVEBORDER	      = 10
COLOR_INACTIVEBORDER	      = 11
COLOR_APPWORKSPACE	      = 12
COLOR_HIGHLIGHT 	      = 13
COLOR_HIGHLIGHTTEXT	      = 14
COLOR_BTNFACE		      = 15
COLOR_BTNSHADOW 	      = 16
COLOR_GRAYTEXT		      = 17
COLOR_BTNTEXT		      = 18
COLOR_INACTIVECAPTIONTEXT     = 19
COLOR_BTNHIGHLIGHT	      = 20
COLOR_3DDKSHADOW	      = 21
COLOR_3DLIGHT		      = 22
COLOR_INFOTEXT		      = 23
COLOR_INFOBK		      = 24
COLOR_HOTLIGHT		      = 26
COLOR_GRADIENTACTIVECAPTION   = 27
COLOR_GRADIENTINACTIVECAPTION = 28

; Button messages

BM_GETCHECK = 00F0h
BM_SETCHECK = 00F1h
BM_GETSTATE = 00F2h
BM_SETSTATE = 00F3h
BM_SETSTYLE = 00F4h
BM_CLICK    = 00F5h
BM_GETIMAGE = 00F6h
BM_SETIMAGE = 00F7h

; Button notifications

BN_CLICKED	 = 0
BN_PAINT	 = 1
BN_HILITE	 = 2
BN_UNHILITE	 = 3
BN_DISABLE	 = 4
BN_DOUBLECLICKED = 5
BN_SETFOCUS	 = 6
BN_KILLFOCUS	 = 7
BN_PUSHED	 = BN_HILITE
BN_UNPUSHED	 = BN_UNHILITE
BN_DBLCLK	 = BN_DOUBLECLICKED

; Button styles

BS_PUSHBUTTON	   = 0000h
BS_DEFPUSHBUTTON   = 0001h
BS_CHECKBOX	   = 0002h
BS_AUTOCHECKBOX    = 0003h
BS_RADIOBUTTON	   = 0004h
BS_3STATE	   = 0005h
BS_AUTO3STATE	   = 0006h
BS_GROUPBOX	   = 0007h
BS_USERBUTTON	   = 0008h
BS_AUTORADIOBUTTON = 0009h
BS_OWNERDRAW	   = 000Bh
BS_TEXT 	   = 0000h
BS_LEFTTEXT	   = 0020h
BS_RIGHTBUTTON	   = BS_LEFTTEXT
BS_ICON 	   = 0040h
BS_BITMAP	   = 0080h
BS_LEFT 	   = 0100h
BS_RIGHT	   = 0200h
BS_CENTER	   = 0300h
BS_TOP		   = 0400h
BS_BOTTOM	   = 0800h
BS_VCENTER	   = 0C00h
BS_PUSHLIKE	   = 1000h
BS_MULTILINE	   = 2000h
BS_NOTIFY	   = 4000h
BS_FLAT 	   = 8000h

; Button states

BST_UNCHECKED	  = 0
BST_CHECKED	  = 1
BST_INDETERMINATE = 2
BST_PUSHED	  = 4
BST_FOCUS	  = 8

; List box messages

LB_ADDSTRING	       = 0180h
LB_INSERTSTRING        = 0181h
LB_DELETESTRING        = 0182h
LB_SELITEMRANGEEX      = 0183h
LB_RESETCONTENT        = 0184h
LB_SETSEL	       = 0185h
LB_SETCURSEL	       = 0186h
LB_GETSEL	       = 0187h
LB_GETCURSEL	       = 0188h
LB_GETTEXT	       = 0189h
LB_GETTEXTLEN	       = 018Ah
LB_GETCOUNT	       = 018Bh
LB_SELECTSTRING        = 018Ch
LB_DIR		       = 018Dh
LB_GETTOPINDEX	       = 018Eh
LB_FINDSTRING	       = 018Fh
LB_GETSELCOUNT	       = 0190h
LB_GETSELITEMS	       = 0191h
LB_SETTABSTOPS	       = 0192h
LB_GETHORIZONTALEXTENT = 0193h
LB_SETHORIZONTALEXTENT = 0194h
LB_SETCOLUMNWIDTH      = 0195h
LB_ADDFILE	       = 0196h
LB_SETTOPINDEX	       = 0197h
LB_GETITEMRECT	       = 0198h
LB_GETITEMDATA	       = 0199h
LB_SETITEMDATA	       = 019Ah
LB_SELITEMRANGE        = 019Bh
LB_SETANCHORINDEX      = 019Ch
LB_GETANCHORINDEX      = 019Dh
LB_SETCARETINDEX       = 019Eh
LB_GETCARETINDEX       = 019Fh
LB_SETITEMHEIGHT       = 01A0h
LB_GETITEMHEIGHT       = 01A1h
LB_FINDSTRINGEXACT     = 01A2h
LB_SETLOCALE	       = 01A5h
LB_GETLOCALE	       = 01A6h
LB_SETCOUNT	       = 01A7h
LB_INITSTORAGE	       = 01A8h
LB_ITEMFROMPOINT       = 01A9h

; List box notifications

LBN_ERRSPACE  = -2
LBN_SELCHANGE = 1
LBN_DBLCLK    = 2
LBN_SELCANCEL = 3
LBN_SETFOCUS  = 4
LBN_KILLFOCUS = 5

; List box styles

LBS_NOTIFY	      = 0001h
LBS_SORT	      = 0002h
LBS_NOREDRAW	      = 0004h
LBS_MULTIPLESEL       = 0008h
LBS_OWNERDRAWFIXED    = 0010h
LBS_OWNERDRAWVARIABLE = 0020h
LBS_HASSTRINGS	      = 0040h
LBS_USETABSTOPS       = 0080h
LBS_NOINTEGRALHEIGHT  = 0100h
LBS_MULTICOLUMN       = 0200h
LBS_WANTKEYBOARDINPUT = 0400h
LBS_EXTENDEDSEL       = 0800h
LBS_DISABLENOSCROLL   = 1000h
LBS_NODATA	      = 2000h
LBS_NOSEL	      = 4000h
LBS_STANDARD	      = LBS_NOTIFY or LBS_SORT or WS_VSCROLL or WS_BORDER

; List box return values

LB_OKAY     = 0
LB_ERR	    = -1
LB_ERRSPACE = -2

; Combo box messages

CB_GETEDITSEL		 = 0140h
CB_LIMITTEXT		 = 0141h
CB_SETEDITSEL		 = 0142h
CB_ADDSTRING		 = 0143h
CB_DELETESTRING 	 = 0144h
CB_DIR			 = 0145h
CB_GETCOUNT		 = 0146h
CB_GETCURSEL		 = 0147h
CB_GETLBTEXT		 = 0148h
CB_GETLBTEXTLEN 	 = 0149h
CB_INSERTSTRING 	 = 014Ah
CB_RESETCONTENT 	 = 014Bh
CB_FINDSTRING		 = 014Ch
CB_SELECTSTRING 	 = 014Dh
CB_SETCURSEL		 = 014Eh
CB_SHOWDROPDOWN 	 = 014Fh
CB_GETITEMDATA		 = 0150h
CB_SETITEMDATA		 = 0151h
CB_GETDROPPEDCONTROLRECT = 0152h
CB_SETITEMHEIGHT	 = 0153h
CB_GETITEMHEIGHT	 = 0154h
CB_SETEXTENDEDUI	 = 0155h
CB_GETEXTENDEDUI	 = 0156h
CB_GETDROPPEDSTATE	 = 0157h
CB_FINDSTRINGEXACT	 = 0158h
CB_SETLOCALE		 = 0159h
CB_GETLOCALE		 = 015Ah
CB_GETTOPINDEX		 = 015Bh
CB_SETTOPINDEX		 = 015Ch
CB_GETHORIZONTALEXTENT	 = 015Dh
CB_SETHORIZONTALEXTENT	 = 015Eh
CB_GETDROPPEDWIDTH	 = 015Fh
CB_SETDROPPEDWIDTH	 = 0160h
CB_INITSTORAGE		 = 0161h

; Combo box notifications

CBN_ERRSPACE	 = -1
CBN_SELCHANGE	 = 1
CBN_DBLCLK	 = 2
CBN_SETFOCUS	 = 3
CBN_KILLFOCUS	 = 4
CBN_EDITCHANGE	 = 5
CBN_EDITUPDATE	 = 6
CBN_DROPDOWN	 = 7
CBN_CLOSEUP	 = 8
CBN_SELENDOK	 = 9
CBN_SELENDCANCEL = 10

; Combo box styles

CBS_SIMPLE	      = 0001h
CBS_DROPDOWN	      = 0002h
CBS_DROPDOWNLIST      = 0003h
CBS_OWNERDRAWFIXED    = 0010h
CBS_OWNERDRAWVARIABLE = 0020h
CBS_AUTOHSCROLL       = 0040h
CBS_OEMCONVERT	      = 0080h
CBS_SORT	      = 0100h
CBS_HASSTRINGS	      = 0200h
CBS_NOINTEGRALHEIGHT  = 0400h
CBS_DISABLENOSCROLL   = 0800h
CBS_UPPERCASE	      = 2000h
CBS_LOWERCASE	      = 4000h

; Combo box return values

CB_OKAY     = 0
CB_ERR	    = -1
CB_ERRSPACE = -2

; Edit control messages

EM_GETSEL	       = 00B0h
EM_SETSEL	       = 00B1h
EM_GETRECT	       = 00B2h
EM_SETRECT	       = 00B3h
EM_SETRECTNP	       = 00B4h
EM_SCROLL	       = 00B5h
EM_LINESCROLL	       = 00B6h
EM_SCROLLCARET	       = 00B7h
EM_GETMODIFY	       = 00B8h
EM_SETMODIFY	       = 00B9h
EM_GETLINECOUNT        = 00BAh
EM_LINEINDEX	       = 00BBh
EM_SETHANDLE	       = 00BCh
EM_GETHANDLE	       = 00BDh
EM_GETTHUMB	       = 00BEh
EM_LINELENGTH	       = 00C1h
EM_REPLACESEL	       = 00C2h
EM_GETLINE	       = 00C4h
EM_LIMITTEXT	       = 00C5h
EM_CANUNDO	       = 00C6h
EM_UNDO 	       = 00C7h
EM_FMTLINES	       = 00C8h
EM_LINEFROMCHAR        = 00C9h
EM_SETTABSTOPS	       = 00CBh
EM_SETPASSWORDCHAR     = 00CCh
EM_EMPTYUNDOBUFFER     = 00CDh
EM_GETFIRSTVISIBLELINE = 00CEh
EM_SETREADONLY	       = 00CFh
EM_SETWORDBREAKPROC    = 00D0h
EM_GETWORDBREAKPROC    = 00D1h
EM_GETPASSWORDCHAR     = 00D2h
EM_SETMARGINS	       = 00D3h
EM_GETMARGINS	       = 00D4h
EM_SETLIMITTEXT        = EM_LIMITTEXT
EM_GETLIMITTEXT        = 00D5h
EM_POSFROMCHAR	       = 00D6h
EM_CHARFROMPOS	       = 00D7h

; Edit control EM_SETMARGIN parameters

EC_LEFTMARGIN  = 1
EC_RIGHTMARGIN = 2
EC_USEFONTINFO = 0FFFFh

; Edit control notifications

EN_SETFOCUS  = 0100h
EN_KILLFOCUS = 0200h
EN_CHANGE    = 0300h
EN_UPDATE    = 0400h
EN_ERRSPACE  = 0500h
EN_MAXTEXT   = 0501h
EN_HSCROLL   = 0601h
EN_VSCROLL   = 0602h

; Edit control styles

ES_LEFT        = 0000h
ES_CENTER      = 0001h
ES_RIGHT       = 0002h
ES_MULTILINE   = 0004h
ES_UPPERCASE   = 0008h
ES_LOWERCASE   = 0010h
ES_PASSWORD    = 0020h
ES_AUTOVSCROLL = 0040h
ES_AUTOHSCROLL = 0080h
ES_NOHIDESEL   = 0100h
ES_OEMCONVERT  = 0400h
ES_READONLY    = 0800h
ES_WANTRETURN  = 1000h
ES_NUMBER      = 2000h

; Static window messages

STM_SETICON  = 0170h
STM_GETICON  = 0171h
STM_SETIMAGE = 0172h
STM_GETIMAGE = 0173h

; Static window notifications

STN_CLICKED = 0
STN_DBLCLK  = 1
STN_ENABLE  = 2
STN_DISABLE = 3

; Static window styles

SS_LEFT 	  = 0000h
SS_CENTER	  = 0001h
SS_RIGHT	  = 0002h
SS_ICON 	  = 0003h
SS_BLACKRECT	  = 0004h
SS_GRAYRECT	  = 0005h
SS_WHITERECT	  = 0006h
SS_BLACKFRAME	  = 0007h
SS_GRAYFRAME	  = 0008h
SS_WHITEFRAME	  = 0009h
SS_USERITEM	  = 000Ah
SS_SIMPLE	  = 000Bh
SS_LEFTNOWORDWRAP = 000Ch
SS_BITMAP	  = 000Eh
SS_OWNERDRAW	  = 000Dh
SS_ENHMETAFILE	  = 000Fh
SS_ETCHEDHORZ	  = 0010h
SS_ETCHEDVERT	  = 0011h
SS_ETCHEDFRAME	  = 0012h
SS_TYPEMASK	  = 001Fh
SS_NOPREFIX	  = 0080h
SS_NOTIFY	  = 0100h
SS_CENTERIMAGE	  = 0200h
SS_RIGHTJUST	  = 0400h
SS_REALSIZEIMAGE  = 0800h
SS_SUNKEN	  = 1000h

; Scroll bar constants

SB_HORZ 	 = 0
SB_VERT 	 = 1
SB_CTL		 = 2
SB_BOTH 	 = 3

; Scroll bar messages

SBM_SETPOS	   = 00E0h
SBM_GETPOS	   = 00E1h
SBM_SETRANGE	   = 00E2h
SBM_SETRANGEREDRAW = 00E6h
SBM_GETRANGE	   = 00E3h
SBM_ENABLE_ARROWS  = 00E4h
SBM_SETSCROLLINFO  = 00E9h
SBM_GETSCROLLINFO  = 00EAh

; Scroll bar commands

SB_LINEUP	 = 0
SB_LINELEFT	 = 0
SB_LINEDOWN	 = 1
SB_LINERIGHT	 = 1
SB_PAGEUP	 = 2
SB_PAGELEFT	 = 2
SB_PAGEDOWN	 = 3
SB_PAGERIGHT	 = 3
SB_THUMBPOSITION = 4
SB_THUMBTRACK	 = 5
SB_TOP		 = 6
SB_LEFT 	 = 6
SB_BOTTOM	 = 7
SB_RIGHT	 = 7
SB_ENDSCROLL	 = 8

; Scroll bar styles

SBS_HORZ		    = 0000h
SBS_VERT		    = 0001h
SBS_TOPALIGN		    = 0002h
SBS_LEFTALIGN		    = 0002h
SBS_BOTTOMALIGN 	    = 0004h
SBS_RIGHTALIGN		    = 0004h
SBS_SIZEBOXTOPLEFTALIGN     = 0002h
SBS_SIZEBOXBOTTOMRIGHTALIGN = 0004h
SBS_SIZEBOX		    = 0008h
SBS_SIZEGRIP		    = 0010h

; Scroll bar info flags

SIF_RANGE	    = 0001h
SIF_PAGE	    = 0002h
SIF_POS 	    = 0004h
SIF_DISABLENOSCROLL = 0008h
SIF_TRACKPOS	    = 0010h
SIF_ALL 	    = SIF_RANGE or SIF_PAGE or SIF_POS or SIF_TRACKPOS

; Dialog styles

DS_ABSALIGN	 = 0001h
DS_SYSMODAL	 = 0002h
DS_3DLOOK	 = 0004h
DS_FIXEDSYS	 = 0008h
DS_NOFAILCREATE  = 0010h
DS_LOCALEDIT	 = 0020h
DS_SETFONT	 = 0040h
DS_MODALFRAME	 = 0080h
DS_NOIDLEMSG	 = 0100h
DS_SETFOREGROUND = 0200h
DS_CONTROL	 = 0400h
DS_CENTER	 = 0800h
DS_CENTERMOUSE	 = 1000h
DS_CONTEXTHELP	 = 2000h

; Dialog codes

DLGC_WANTARROWS      = 0001h
DLGC_WANTTAB	     = 0002h
DLGC_WANTALLKEYS     = 0004h
DLGC_WANTMESSAGE     = 0004h
DLGC_HASSETSEL	     = 0008h
DLGC_DEFPUSHBUTTON   = 0010h
DLGC_UNDEFPUSHBUTTON = 0020h
DLGC_RADIOBUTTON     = 0040h
DLGC_WANTCHARS	     = 0080h
DLGC_STATIC	     = 0100h
DLGC_BUTTON	     = 2000h

; Menu flags

MF_INSERT	   = 0000h
MF_CHANGE	   = 0080h
MF_APPEND	   = 0100h
MF_DELETE	   = 0200h
MF_REMOVE	   = 1000h
MF_BYCOMMAND	   = 0000h
MF_BYPOSITION	   = 0400h
MF_SEPARATOR	   = 0800h
MF_UNCHECKED	   = 0000h
MF_ENABLED	   = 0000h
MF_GRAYED	   = 0001h
MF_DISABLED	   = 0002h
MF_CHECKED	   = 0008h
MF_USECHECKBITMAPS = 0200h
MF_STRING	   = 0000h
MF_BITMAP	   = 0004h
MF_OWNERDRAW	   = 0100h
MF_POPUP	   = 0010h
MF_MENUBARBREAK    = 0020h
MF_MENUBREAK	   = 0040h
MF_UNHILITE	   = 0000h
MF_HILITE	   = 0080h
MF_DEFAULT	   = 1000h
MF_SYSMENU	   = 2000h
MF_HELP 	   = 4000h
MF_RIGHTJUSTIFY    = 4000h
MF_MOUSESELECT	   = 8000h
MF_END		   = 0080h
MFT_STRING	   = MF_STRING
MFT_BITMAP	   = MF_BITMAP
MFT_MENUBARBREAK   = MF_MENUBARBREAK
MFT_MENUBREAK	   = MF_MENUBREAK
MFT_OWNERDRAW	   = MF_OWNERDRAW
MFT_RADIOCHECK	   = 0200h
MFT_SEPARATOR	   = MF_SEPARATOR
MFT_RIGHTORDER	   = 2000h
MFT_RIGHTJUSTIFY   = MF_RIGHTJUSTIFY
MFS_GRAYED	   = 0003h
MFS_DISABLED	   = MFS_GRAYED
MFS_CHECKED	   = MF_CHECKED
MFS_HILITE	   = MF_HILITE
MFS_ENABLED	   = MF_ENABLED
MFS_UNCHECKED	   = MF_UNCHECKED
MFS_UNHILITE	   = MF_UNHILITE
MFS_DEFAULT	   = MF_DEFAULT
MFR_POPUP	   = 0001h
MFR_END 	   = MF_END

; System menu command values

SC_SIZE 	= 61440
SC_MOVE 	= 61456
SC_MINIMIZE	= 61472
SC_MAXIMIZE	= 61488
SC_NEXTWINDOW	= 61504
SC_PREVWINDOW	= 61520
SC_CLOSE	= 61536
SC_VSCROLL	= 61552
SC_HSCROLL	= 61568
SC_MOUSEMENU	= 61584
SC_KEYMENU	= 61696
SC_ARRANGE	= 61712
SC_RESTORE	= 61728
SC_TASKLIST	= 61744
SC_SCREENSAVE	= 61760
SC_HOTKEY	= 61776
SC_DEFAULT	= 61792
SC_MONITORPOWER = 61808
SC_CONTEXTHELP	= 61824
SC_SEPARATOR	= 61455

; Border types

BDR_RAISEDOUTER = 01h
BDR_SUNKENOUTER = 02h
BDR_RAISEDINNER = 04h
BDR_SUNKENINNER = 08h
BDR_OUTER	= 03h
BDR_INNER	= 0Ch
BDR_RAISED	= 05h
BDR_SUNKEN	= 0Ah
EDGE_RAISED	= BDR_RAISEDOUTER or BDR_RAISEDINNER
EDGE_SUNKEN	= BDR_SUNKENOUTER or BDR_SUNKENINNER
EDGE_ETCHED	= BDR_SUNKENOUTER or BDR_RAISEDINNER
EDGE_BUMP	= BDR_RAISEDOUTER or BDR_SUNKENINNER

; Border flags

BF_LEFT 		   = 0001h
BF_TOP			   = 0002h
BF_RIGHT		   = 0004h
BF_BOTTOM		   = 0008h
BF_TOPLEFT		   = BF_TOP or BF_LEFT
BF_TOPRIGHT		   = BF_TOP or BF_RIGHT
BF_BOTTOMLEFT		   = BF_BOTTOM or BF_LEFT
BF_BOTTOMRIGHT		   = BF_BOTTOM or BF_RIGHT
BF_RECT 		   = BF_LEFT or BF_TOP or BF_RIGHT or BF_BOTTOM
BF_DIAGONAL		   = 0010h
BF_DIAGONAL_ENDTOPRIGHT    = BF_DIAGONAL or BF_TOP or BF_RIGHT
BF_DIAGONAL_ENDTOPLEFT	   = BF_DIAGONAL or BF_TOP or BF_LEFT
BF_DIAGONAL_ENDBOTTOMLEFT  = BF_DIAGONAL or BF_BOTTOM or BF_LEFT
BF_DIAGONAL_ENDBOTTOMRIGHT = BF_DIAGONAL or BF_BOTTOM or BF_RIGHT
BF_MIDDLE		   = 0800h
BF_SOFT 		   = 1000h
BF_ADJUST		   = 2000h
BF_FLAT 		   = 4000h
BF_MONO 		   = 8000h

; Frame control types

DFC_CAPTION   = 1
DFC_MENU      = 2
DFC_SCROLL    = 3
DFC_BUTTON    = 4
DFC_POPUPMENU = 5

; Frame control states

DFCS_CAPTIONCLOSE	 = 0000h
DFCS_CAPTIONMIN 	 = 0001h
DFCS_CAPTIONMAX 	 = 0002h
DFCS_CAPTIONRESTORE	 = 0003h
DFCS_CAPTIONHELP	 = 0004h
DFCS_MENUARROW		 = 0000h
DFCS_MENUCHECK		 = 0001h
DFCS_MENUBULLET 	 = 0002h
DFCS_MENUARROWRIGHT	 = 0004h
DFCS_SCROLLUP		 = 0000h
DFCS_SCROLLDOWN 	 = 0001h
DFCS_SCROLLLEFT 	 = 0002h
DFCS_SCROLLRIGHT	 = 0003h
DFCS_SCROLLCOMBOBOX	 = 0005h
DFCS_SCROLLSIZEGRIP	 = 0008h
DFCS_SCROLLSIZEGRIPRIGHT = 0010h
DFCS_BUTTONCHECK	 = 0000h
DFCS_BUTTONRADIOIMAGE	 = 0001h
DFCS_BUTTONRADIOMASK	 = 0002h
DFCS_BUTTONRADIO	 = 0004h
DFCS_BUTTON3STATE	 = 0008h
DFCS_BUTTONPUSH 	 = 0010h
DFCS_INACTIVE		 = 0100h
DFCS_PUSHED		 = 0200h
DFCS_CHECKED		 = 0400h
DFCS_TRANSPARENT	 = 0800h
DFCS_HOT		 = 1000h
DFCS_ADJUSTRECT 	 = 2000h
DFCS_FLAT		 = 4000h
DFCS_MONO		 = 8000h

; DrawCaption flags

DC_ACTIVE   = 01h
DC_SMALLCAP = 02h
DC_ICON     = 04h
DC_TEXT     = 08h
DC_INBUTTON = 10h

; DrawIconEx options

DI_MASK        = 1
DI_IMAGE       = 2
DI_NORMAL      = 3
DI_COMPAT      = 4
DI_DEFAULTSIZE = 8

; DrawText parameters

DT_TOP		   = 00000h
DT_LEFT 	   = 00000h
DT_CENTER	   = 00001h
DT_RIGHT	   = 00002h
DT_VCENTER	   = 00004h
DT_BOTTOM	   = 00008h
DT_WORDBREAK	   = 00010h
DT_SINGLELINE	   = 00020h
DT_EXPANDTABS	   = 00040h
DT_TABSTOP	   = 00080h
DT_NOCLIP	   = 00100h
DT_EXTERNALLEADING = 00200h
DT_CALCRECT	   = 00400h
DT_NOPREFIX	   = 00800h
DT_INTERNAL	   = 01000h
DT_EDITCONTROL	   = 02000h
DT_PATH_ELLIPSIS   = 04000h
DT_END_ELLIPSIS    = 08000h
DT_MODIFYSTRING    = 10000h
DT_RTLREADING	   = 20000h
DT_WORD_ELLIPSIS   = 40000h

; GetDCEx flags

DCX_WINDOW	     = 000001h
DCX_CACHE	     = 000002h
DCX_NORESETATTRS     = 000004h
DCX_CLIPCHILDREN     = 000008h
DCX_CLIPSIBLINGS     = 000010h
DCX_PARENTCLIP	     = 000020h
DCX_EXCLUDERGN	     = 000040h
DCX_INTERSECTRGN     = 000080h
DCX_EXCLUDEUPDATE    = 000100h
DCX_INTERSECTUPDATE  = 000200h
DCX_LOCKWINDOWUPDATE = 000400h
DCX_VALIDATE	     = 200000h

; SetWindowsHook codes

WH_MSGFILTER	   = -1
WH_JOURNALRECORD   = 0
WH_JOURNALPLAYBACK = 1
WH_KEYBOARD	   = 2
WH_GETMESSAGE	   = 3
WH_CALLWNDPROC	   = 4
WH_CBT		   = 5
WH_SYSMSGFILTER    = 6
WH_MOUSE	   = 7
WH_HARDWARE	   = 8
WH_DEBUG	   = 9
WH_SHELL	   = 10
WH_FOREGROUNDIDLE  = 11
WH_CALLWNDPROCRET  = 12
WH_KEYBOARD_LL	   = 13
WH_MOUSE_LL	   = 14

; Hook codes

HC_ACTION      = 0
HC_GETNEXT     = 1
HC_SKIP        = 2
HC_NOREMOVE    = 3
HC_SYSMODALON  = 4
HC_SYSMODALOFF = 5

; CBT hook codes

HCBT_MOVESIZE	  = 0
HCBT_MINMAX	  = 1
HCBT_QS 	  = 2
HCBT_CREATEWND	  = 3
HCBT_DESTROYWND   = 4
HCBT_ACTIVATE	  = 5
HCBT_CLICKSKIPPED = 6
HCBT_KEYSKIPPED   = 7
HCBT_SYSCOMMAND   = 8
HCBT_SETFOCUS	  = 9

; ExitWindowsEx flags

EWX_LOGOFF   = 0
EWX_SHUTDOWN = 1
EWX_REBOOT   = 2
EWX_FORCE    = 4
EWX_POWEROFF = 8

; WinHelp commands

HELP_CONTEXT	  = 001h
HELP_QUIT	  = 002h
HELP_INDEX	  = 003h
HELP_CONTENTS	  = 003h
HELP_HELPONHELP   = 004h
HELP_SETINDEX	  = 005h
HELP_SETCONTENTS  = 005h
HELP_CONTEXTPOPUP = 008h
HELP_FORCEFILE	  = 009h
HELP_CONTEXTMENU  = 00Ah
HELP_FINDER	  = 00Bh
HELP_WM_HELP	  = 00Ch
HELP_SETPOPUP_POS = 00Dh
HELP_KEY	  = 101h
HELP_COMMAND	  = 102h
HELP_PARTIALKEY   = 105h
HELP_MULTIKEY	  = 201h
HELP_SETWINPOS	  = 203h

; keybd_event flags

KEYEVENTF_EXTENDEDKEY = 1h
KEYEVENTF_KEYUP       = 2h

; mouse_event flags

MOUSEEVENTF_MOVE       = 0001h
MOUSEEVENTF_LEFTDOWN   = 0002h
MOUSEEVENTF_LEFTUP     = 0004h
MOUSEEVENTF_RIGHTDOWN  = 0008h
MOUSEEVENTF_RIGHTUP    = 0010h
MOUSEEVENTF_MIDDLEDOWN = 0020h
MOUSEEVENTF_MIDDLEUP   = 0040h
MOUSEEVENTF_WHEEL      = 0800h
MOUSEEVENTF_ABSOLUTE   = 8000h

; TrackPopupMenu flags

TPM_LEFTBUTTON	    = 0000h
TPM_RIGHTBUTTON     = 0002h
TPM_LEFTALIGN	    = 0000h
TPM_CENTERALIGN     = 0004h
TPM_RIGHTALIGN	    = 0008h
TPM_TOPALIGN	    = 0000h
TPM_VCENTERALIGN    = 0010h
TPM_BOTTOMALIGN     = 0020h
TPM_HORIZONTAL	    = 0000h
TPM_VERTICAL	    = 0040h
TPM_NONOTIFY	    = 0080h
TPM_RETURNCMD	    = 0100h
TPM_RECURSE	    = 0001h
TPM_HORPOSANIMATION = 0400h
TPM_HORNEGANIMATION = 0800h
TPM_VERPOSANIMATION = 1000h
TPM_VERNEGANIMATION = 2000h
TPM_NOANIMATION     = 4000h
TPM_LAYOUTRTL	    = 8000h

; Menu item info mask values

MIIM_STATE	= 001h
MIIM_ID 	= 002h
MIIM_SUBMENU	= 004h
MIIM_CHECKMARKS = 008h
MIIM_TYPE	= 010h
MIIM_DATA	= 020h
MIIM_STRING	= 040h
MIIM_BITMAP	= 080h
MIIM_FTYPE	= 100h

; DRAWITEMSTRUCT control types

ODT_MENU     = 1
ODT_LISTBOX  = 2
ODT_COMBOBOX = 3
ODT_BUTTON   = 4
ODT_STATIC   = 5

; DRAWITEMSTRUCT actions

ODA_DRAWENTIRE = 1
ODA_SELECT     = 2
ODA_FOCUS      = 4

; DRAWITEMSTRUCT states

ODS_SELECTED	 = 0001h
ODS_GRAYED	 = 0002h
ODS_DISABLED	 = 0004h
ODS_CHECKED	 = 0008h
ODS_FOCUS	 = 0010h
ODS_DEFAULT	 = 0020h
ODS_COMBOBOXEDIT = 1000h
ODS_HOTLIGHT	 = 0040h
ODS_INACTIVE	 = 0080h

; WINDOWPLACEMENT flags

WPF_SETMINPOSITION     = 1
WPF_RESTORETOMAXIMIZED = 2

; Layered window attributes

LWA_COLORKEY = 1
LWA_ALPHA    = 2

; UpdateLayeredWindow flags

ULW_COLORKEY = 1
ULW_ALPHA    = 2
ULW_OPAQUE   = 4

; SystemParametersInfo parameters

SPI_GETACCESSTIMEOUT	  = 60
SPI_GETANIMATION	  = 72
SPI_GETBEEP		  = 1
SPI_GETBORDER		  = 5
SPI_GETDEFAULTINPUTLANG   = 89
SPI_GETDRAGFULLWINDOWS	  = 38
SPI_GETFASTTASKSWITCH	  = 35
SPI_GETFILTERKEYS	  = 50
SPI_GETFONTSMOOTHING	  = 74
SPI_GETGRIDGRANULARITY	  = 18
SPI_GETHIGHCONTRAST	  = 66
SPI_GETICONMETRICS	  = 45
SPI_GETICONTITLELOGFONT   = 31
SPI_GETICONTITLEWRAP	  = 25
SPI_GETKEYBOARDDELAY	  = 22
SPI_GETKEYBOARDPREF	  = 68
SPI_GETKEYBOARDSPEED	  = 10
SPI_GETLOWPOWERACTIVE	  = 83
SPI_GETLOWPOWERTIMEOUT	  = 79
SPI_GETMENUDROPALIGNMENT  = 27
SPI_GETMINIMIZEDMETRICS   = 43
SPI_GETMOUSE		  = 3
SPI_GETMOUSEKEYS	  = 54
SPI_GETMOUSETRAILS	  = 94
SPI_GETNONCLIENTMETRICS   = 41
SPI_GETPOWEROFFACTIVE	  = 84
SPI_GETPOWEROFFTIMEOUT	  = 80
SPI_GETSCREENREADER	  = 70
SPI_GETSCREENSAVEACTIVE   = 16
SPI_GETSCREENSAVETIMEOUT  = 14
SPI_GETSERIALKEYS	  = 62
SPI_GETSHOWSOUNDS	  = 56
SPI_GETSOUNDSENTRY	  = 64
SPI_GETSTICKYKEYS	  = 58
SPI_GETTOGGLEKEYS	  = 52
SPI_GETWINDOWSEXTENSION   = 92
SPI_GETWORKAREA 	  = 48
SPI_ICONHORIZONTALSPACING = 13
SPI_ICONVERTICALSPACING   = 24
SPI_LANGDRIVER		  = 12
SPI_SCREENSAVERRUNNING	  = 97
SPI_SETACCESSTIMEOUT	  = 61
SPI_SETANIMATION	  = 73
SPI_SETBEEP		  = 2
SPI_SETBORDER		  = 6
SPI_SETDEFAULTINPUTLANG   = 90
SPI_SETDESKPATTERN	  = 21
SPI_SETDESKWALLPAPER	  = 20
SPI_SETDOUBLECLICKTIME	  = 32
SPI_SETDOUBLECLKHEIGHT	  = 30
SPI_SETDOUBLECLKWIDTH	  = 29
SPI_SETDRAGFULLWINDOWS	  = 37
SPI_SETDRAGHEIGHT	  = 77
SPI_SETDRAGWIDTH	  = 76
SPI_SETFASTTASKSWITCH	  = 36
SPI_SETFILTERKEYS	  = 51
SPI_SETFONTSMOOTHING	  = 75
SPI_SETGRIDGRANULARITY	  = 19
SPI_SETHANDHELD 	  = 78
SPI_SETHIGHCONTRAST	  = 67
SPI_SETICONMETRICS	  = 46
SPI_SETICONTITLELOGFONT   = 34
SPI_SETICONTITLEWRAP	  = 26
SPI_SETKEYBOARDDELAY	  = 23
SPI_SETKEYBOARDPREF	  = 69
SPI_SETKEYBOARDSPEED	  = 11
SPI_SETLANGTOGGLE	  = 91
SPI_SETLOWPOWERACTIVE	  = 85
SPI_SETLOWPOWERTIMEOUT	  = 81
SPI_SETMENUDROPALIGNMENT  = 28
SPI_SETMINIMIZEDMETRICS   = 44
SPI_SETMOUSE		  = 4
SPI_SETMOUSEBUTTONSWAP	  = 33
SPI_SETMOUSEKEYS	  = 55
SPI_SETMOUSETRAILS	  = 93
SPI_SETNONCLIENTMETRICS   = 42
SPI_SETPENWINDOWS	  = 49
SPI_SETPOWEROFFACTIVE	  = 86
SPI_SETPOWEROFFTIMEOUT	  = 82
SPI_SETSCREENREADER	  = 71
SPI_SETSCREENSAVEACTIVE   = 17
SPI_SETSCREENSAVERRUNNING = 97
SPI_SETSCREENSAVETIMEOUT  = 15
SPI_SETSERIALKEYS	  = 63
SPI_SETSHOWSOUNDS	  = 57
SPI_SETSOUNDSENTRY	  = 65
SPI_SETSTICKYKEYS	  = 59
SPI_SETTOGGLEKEYS	  = 53
SPI_SETWORKAREA 	  = 47

; SystemParametersInfo flags

SPIF_UPDATEINIFILE	  = 1
SPIF_SENDWININICHANGE	  = 2


; GDI32.DLL structures and constants

struct SIZE
  cx dd ?
  cy dd ?
ends

struct BITMAP
  bmType       dd ?
  bmWidth      dd ?
  bmHeight     dd ?
  bmWidthBytes dd ?
  bmPlanes     dw ?
  bmBitsPixel  dw ?
  bmBits       dd ?
ends

struct BITMAPCOREHEADER
  bcSize     dd ?
  bcWidth    dw ?
  bcHeight   dw ?
  bcPlanes   dw ?
  bcBitCount dw ?
ends

struct BITMAPINFOHEADER
  biSize	  dd ?
  biWidth	  dd ?
  biHeight	  dd ?
  biPlanes	  dw ?
  biBitCount	  dw ?
  biCompression   dd ?
  biSizeImage	  dd ?
  biXPelsPerMeter dd ?
  biYPelsPerMeter dd ?
  biClrUsed	  dd ?
  biClrImportant  dd ?
ends

struct BITMAPFILEHEADER
  bfType      dw ?
  bfSize      dd ?
  bfReserved1 dw ?
  bfReserved2 dw ?
  bfOffBits   dd ?
ends

struct TEXTMETRIC
  tmHeight	     dd ?
  tmAscent	     dd ?
  tmDescent	     dd ?
  tmInternalLeading  dd ?
  tmExternalLeading  dd ?
  tmAveCharWidth     dd ?
  tmMaxCharWidth     dd ?
  tmWeight	     dd ?
  tmOverhang	     dd ?
  tmDigitizedAspectX dd ?
  tmDigitizedAspectY dd ?
  tmFirstChar	     db ?
  tmLastChar	     db ?
  tmDefaultChar      db ?
  tmBreakChar	     db ?
  tmItalic	     db ?
  tmUnderlined	     db ?
  tmStruckOut	     db ?
  tmPitchAndFamily   db ?
  tmCharSet	     db ?
ends

struct LOGBRUSH
  lbStyle dd ?
  lbColor dd ?
  lbHatch dd ?
ends

struct LOGPEN
  lopnStyle dd ?
  lopnWidth POINT
  lopnColor dd ?
ends

struct EXTLOGPEN
  elpPenStyle	dd ?
  elpWidth	dd ?
  elpBrushStyle dd ?
  elpColor	dd ?
  elpHatch	dd ?
  elpNumEntries dd ?
  elpStyleEntry dd ?
ends

struct LOGFONT
  lfHeight	   dd ?
  lfWidth	   dd ?
  lfEscapement	   dd ?
  lfOrientation    dd ?
  lfWeight	   dd ?
  lfItalic	   db ?
  lfUnderline	   db ?
  lfStrikeOut	   db ?
  lfCharSet	   db ?
  lfOutPrecision   db ?
  lfClipPrecision  db ?
  lfQuality	   db ?
  lfPitchAndFamily db ?
  lfFaceName	   db 32 dup (?)
ends

struct ENUMLOGFONT
  elfLogFont  LOGFONT
  elfFullName db 64 dup (?)
  elfStyle    db 32 dup (?)
ends

struct ENUMLOGFONTEX
  elfLogFont  LOGFONT
  elfFullName db 64 dup (?)
  elfStyle    db 32 dup (?)
  elfScript   db 32 dup (?)
ends

struct PIXELFORMATDESCRIPTOR
  nSize 	  dw ?
  nVersion	  dw ?
  dwFlags	  dd ?
  iPixelType	  db ?
  cColorBits	  db ?
  cRedBits	  db ?
  cRedShift	  db ?
  cGreenBits	  db ?
  cGreenShift	  db ?
  cBlueBits	  db ?
  cBlueShift	  db ?
  cAlphaBits	  db ?
  cAlphaShift	  db ?
  cAccumBits	  db ?
  cAccumRedBits   db ?
  cAccumGreenBits db ?
  cAccumBlueBits  db ?
  cAccumAlphaBits db ?
  cDepthBits	  db ?
  cStencilBits	  db ?
  cAuxBuffers	  db ?
  iLayerType	  db ?
  bReserved	  db ?
  dwLayerMask	  dd ?
  dwVisibleMask   dd ?
  dwDamageMask	  dd ?
ends

struct TRIVERTEX
  x	dd ? 
  y	dd ? 
  Red	dw ? 
  Green dw ? 
  Blue	dw ? 
  Alpha dw ? 
ends

; General constants

GDI_ERROR  = 0FFFFFFFFh
HGDI_ERROR = 0FFFFFFFFh

; Binary raster operations

R2_BLACK       = 1
R2_NOTMERGEPEN = 2
R2_MASKNOTPEN  = 3
R2_NOTCOPYPEN  = 4
R2_MASKPENNOT  = 5
R2_NOT	       = 6
R2_XORPEN      = 7
R2_NOTMASKPEN  = 8
R2_MASKPEN     = 9
R2_NOTXORPEN   = 10
R2_NOP	       = 11
R2_MERGENOTPEN = 12
R2_COPYPEN     = 13
R2_MERGEPENNOT = 14
R2_MERGEPEN    = 15
R2_WHITE       = 16

; Raster operations

SRCCOPY     = 00CC0020h
SRCPAINT    = 00EE0086h
SRCAND	    = 008800C6h
SRCINVERT   = 00660046h
SRCERASE    = 00440328h
NOTSRCCOPY  = 00330008h
NOTSRCERASE = 001100A6h
MERGECOPY   = 00C000CAh
MERGEPAINT  = 00BB0226h
PATCOPY     = 00F00021h
PATPAINT    = 00FB0A09h
PATINVERT   = 005A0049h
DSTINVERT   = 00550009h
BLACKNESS   = 00000042h
WHITENESS   = 00FF0062h

; Region flags

ERROR	      = 0
NULLREGION    = 1
SIMPLEREGION  = 2
COMPLEXREGION = 3

; CombineRgn styles

RGN_AND  = 1
RGN_OR	 = 2
RGN_XOR  = 3
RGN_DIFF = 4
RGN_COPY = 5

; StretchBlt modes

BLACKONWHITE = 1
WHITEONBLACK = 2
COLORONCOLOR = 3
HALFTONE     = 4
STRETCH_ANDSCANS    = BLACKONWHITE
STRETCH_ORSCANS     = WHITEONBLACK
STRETCH_DELETESCANS = COLORONCOLOR
STRETCH_HALFTONE    = HALFTONE

; PolyFill modes

ALTERNATE = 1
WINDING   = 2

; Background modes

TRANSPARENT = 1
OPAQUE	    = 2

; Point types

PT_CLOSEFIGURE = 1
PT_LINETO      = 2
PT_BEZIERTO    = 4
PT_MOVETO      = 6

; Mapping modes

MM_TEXT        = 1
MM_LOMETRIC    = 2
MM_HIMETRIC    = 3
MM_LOENGLISH   = 4
MM_HIENGLISH   = 5
MM_TWIPS       = 6
MM_ISOTROPIC   = 7
MM_ANISOTROPIC = 8

; Coordinate modes

ABSOLUTE = 1
RELATIVE = 2

; Stock logical objects

WHITE_BRUSH	    = 0
LTGRAY_BRUSH	    = 1
GRAY_BRUSH	    = 2
DKGRAY_BRUSH	    = 3
BLACK_BRUSH	    = 4
NULL_BRUSH	    = 5
HOLLOW_BRUSH	    = NULL_BRUSH
WHITE_PEN	    = 6
BLACK_PEN	    = 7
NULL_PEN	    = 8
OEM_FIXED_FONT	    = 10
ANSI_FIXED_FONT     = 11
ANSI_VAR_FONT	    = 12
SYSTEM_FONT	    = 13
DEVICE_DEFAULT_FONT = 14
DEFAULT_PALETTE     = 15
SYSTEM_FIXED_FONT   = 16
DEFAULT_GUI_FONT    = 17

; Brush styles

BS_SOLID	 = 0
BS_NULL 	 = 1
BS_HOLLOW	 = BS_NULL
BS_HATCHED	 = 2
BS_PATTERN	 = 3
BS_INDEXED	 = 4
BS_DIBPATTERN	 = 5
BS_DIBPATTERNPT  = 6
BS_PATTERN8X8	 = 7
BS_DIBPATTERN8X8 = 8
BS_MONOPATTERN	 = 9

; Hatch styles

HS_HORIZONTAL = 0
HS_VERTICAL   = 1
HS_FDIAGONAL  = 2
HS_BDIAGONAL  = 3
HS_CROSS      = 4
HS_DIAGCROSS  = 5

; Pen styles

PS_SOLID	 = 0
PS_DASH 	 = 1
PS_DOT		 = 2
PS_DASHDOT	 = 3
PS_DASHDOTDOT	 = 4
PS_NULL 	 = 5
PS_INSIDEFRAME	 = 6
PS_USERSTYLE	 = 7
PS_ALTERNATE	 = 8
PS_ENDCAP_ROUND  = 0
PS_ENDCAP_SQUARE = 100h
PS_ENDCAP_FLAT	 = 200h
PS_JOIN_ROUND	 = 0
PS_JOIN_BEVEL	 = 1000h
PS_JOIN_MITER	 = 2000h
PS_COSMETIC	 = 0
PS_GEOMETRIC	 = 010000h

; Arc directions

AD_COUNTERCLOCKWISE = 1
AD_CLOCKWISE	    = 2

; Text alignment options

TA_NOUPDATECP = 0
TA_UPDATECP   = 1
TA_LEFT       = 0
TA_RIGHT      = 2
TA_CENTER     = 6
TA_TOP	      = 0
TA_BOTTOM     = 8
TA_BASELINE   = 24
TA_RTLREADING = 100h
VTA_BASELINE  = TA_BASELINE
VTA_LEFT      = TA_BOTTOM
VTA_RIGHT     = TA_TOP
VTA_CENTER    = TA_CENTER
VTA_BOTTOM    = TA_RIGHT
VTA_TOP       = TA_LEFT

; ExtTextOut options

ETO_OPAQUE	   = 0002h
ETO_CLIPPED	   = 0004h
ETO_GLYPH_INDEX    = 0010h
ETO_RTLREADING	   = 0080h
ETO_IGNORELANGUAGE = 1000h

; Bitmap compression types

BI_RGB	     = 0
BI_RLE8      = 1
BI_RLE4      = 2
BI_BITFIELDS = 3

; tmPitchAndFamily flags

TMPF_FIXED_PITCH = 1
TMPF_VECTOR	 = 2
TMPF_TRUETYPE	 = 4
TMPF_DEVICE	 = 8

; Font output precision values

OUT_DEFAULT_PRECIS	  = 0
OUT_STRING_PRECIS	  = 1
OUT_CHARACTER_PRECIS	  = 2
OUT_STROKE_PRECIS	  = 3
OUT_TT_PRECIS		  = 4
OUT_DEVICE_PRECIS	  = 5
OUT_RASTER_PRECIS	  = 6
OUT_TT_ONLY_PRECIS	  = 7
OUT_OUTLINE_PRECIS	  = 8
OUT_SCREEN_OUTLINE_PRECIS = 9

; Font clipping precision values

CLIP_DEFAULT_PRECIS   = 0
CLIP_CHARACTER_PRECIS = 1
CLIP_STROKE_PRECIS    = 2
CLIP_LH_ANGLES	      = 10h
CLIP_TT_ALWAYS	      = 20h
CLIP_EMBEDDED	      = 80h

; Font output quality values

DEFAULT_QUALITY        = 0
DRAFT_QUALITY	       = 1
PROOF_QUALITY	       = 2
NONANTIALIASED_QUALITY = 3
ANTIALIASED_QUALITY    = 4

; Font pitch values

DEFAULT_PITCH  = 0
FIXED_PITCH    = 1
VARIABLE_PITCH = 2
MONO_FONT      = 8

; Font families

FF_DONTCARE   = 00h
FF_ROMAN      = 10h
FF_SWISS      = 20h
FF_MODERN     = 30h
FF_SCRIPT     = 40h
FF_DECORATIVE = 50h

; Font weights

FW_DONTCARE   = 0
FW_THIN       = 100
FW_EXTRALIGHT = 200
FW_LIGHT      = 300
FW_NORMAL     = 400
FW_MEDIUM     = 500
FW_SEMIBOLD   = 600
FW_BOLD       = 700
FW_EXTRABOLD  = 800
FW_HEAVY      = 900
FW_ULTRALIGHT = FW_EXTRALIGHT
FW_REGULAR    = FW_NORMAL
FW_DEMIBOLD   = FW_SEMIBOLD
FW_ULTRABOLD  = FW_EXTRABOLD
FW_BLACK      = FW_HEAVY

; Character set values

ANSI_CHARSET	    = 0
DEFAULT_CHARSET     = 1
SYMBOL_CHARSET	    = 2
SHIFTJIS_CHARSET    = 128
HANGEUL_CHARSET     = 129
GB2312_CHARSET	    = 134
CHINESEBIG5_CHARSET = 136
OEM_CHARSET	    = 255
JOHAB_CHARSET	    = 130
HEBREW_CHARSET	    = 177
ARABIC_CHARSET	    = 178
GREEK_CHARSET	    = 161
TURKISH_CHARSET     = 162
VIETNAMESE_CHARSET  = 163
THAI_CHARSET	    = 222
EASTEUROPE_CHARSET  = 238
RUSSIAN_CHARSET     = 204
MAC_CHARSET	    = 77
BALTIC_CHARSET	    = 186

; Pixel format constants

PFD_TYPE_RGBA		  = 0
PFD_TYPE_COLORINDEX	  = 1
PFD_MAIN_PLANE		  = 0
PFD_OVERLAY_PLANE	  = 1
PFD_UNDERLAY_PLANE	  = -1
PFD_DOUBLEBUFFER	  = 1
PFD_STEREO		  = 2
PFD_DRAW_TO_WINDOW	  = 4
PFD_DRAW_TO_BITMAP	  = 8
PFD_SUPPORT_GDI 	  = 10h
PFD_SUPPORT_OPENGL	  = 20h
PFD_GENERIC_FORMAT	  = 40h
PFD_NEED_PALETTE	  = 80h
PFD_NEED_SYSTEM_PALETTE   = 100h
PFD_SWAP_EXCHANGE	  = 200h
PFD_SWAP_COPY		  = 400h
PFD_SWAP_LAYER_BUFFERS	  = 800h
PFD_GENERIC_ACCELERATED   = 1000h
PFD_DEPTH_DONTCARE	  = 20000000h
PFD_DOUBLEBUFFER_DONTCARE = 40000000h
PFD_STEREO_DONTCARE	  = 80000000h


; COMCTL32.DLL structures and constants

struct PROPSHEETPAGE
  dwSize      dd ?
  dwFlags     dd ?
  hInstance   dd ?
  pszTemplate dd ?
  pszIcon     dd ?
  pszTitle    dd ?
  pfnDlgProc  dd ?
  lParam      dd ?
  pfnCallback dd ?
  pcRefParent dd ?
ends

struct PROPSHEETHEADER
  dwSize      dd ?
  dwFlags     dd ?
  hwndParent  dd ?
  hInstance   dd ?
  pszIcon     dd ?
  pszCaption  dd ?
  nPages      dd ?
  pStartPage  dd ?
  ppsp	      dd ?
  pfnCallback dd ?
ends

struct IMAGEINFO
  hbmImage dd ?
  hbmMask  dd ?
  Unused1  dd ?
  Unused2  dd ?
  rcImage  RECT
ends

struct HD_ITEM
  mask	     dd ?
  cxy	     dd ?
  pszText    dd ?
  hbm	     dd ?
  cchTextMax dd ?
  fmt	     dd ?
  lParam     dd ?
ends

struct HD_LAYOUT
  prc	dd ?
  pwpos dd ?
ends

struct HD_HITTESTINFO
  pt	POINT
  flags dd ?
  iItem dd ?
ends

struct HD_NOTIFY
  hdr	  NMHDR
  iItem   dd ?
  iButton dd ?
  pitem   dd ?
ends

struct TBBUTTON
  iBitmap   dd ?
  idCommand dd ?
  fsState   db ?
  fsStyle   db ?
	    dw ?
  dwData    dd ?
  iString   dd ?
ends

struct COLORMAP
  from dd ?
  to   dd ?
ends

struct TBADDBITMAP
  hInst dd ?
  nID	dd ?
ends

struct TBSAVEPARAMS
  hkr	       dd ?
  pszSubKey    dd ?
  pszValueName dd ?
ends

struct TBREPLACEBITMAP
  hInstOld dd ?
  nIDOld   dd ?
  hInstNew dd ?
  nIDNew   dd ?
  nButtons dd ?
ends

struct NMTOOLBAR
  hdr	   NMHDR
  iItem    dd ?
  tbButton TBBUTTON
  cchText  dd ?
  pszText  dd ?
ends

struct REBARINFO
  cbSize dd ?
  fMask  dd ?
  himl	 dd ?
ends

struct REBARBANDINFO
  cbSize      dd ?
  fMask       dd ?
  fStyle      dd ?
  clrFore     dd ?
  clrBack     dd ?
  lpText      dd ?
  cch	      dd ?
  iImage      dd ?
  hwndChild   dd ?
  cxMinChild  dd ?
  cyMinChild  dd ?
  cx	      dd ?
  hbmBack     dd ?
  wID	      dd ?
ends

struct TOOLINFO
  cbSize   dd ?
  uFlags   dd ?
  hwnd	   dd ?
  uId	   dd ?
  Rect	   RECT
  hInst    dd ?
  lpszText dd ?
ends

struct TTHITTESTINFO
  hwnd dd ?
  pt   POINT
  ti   TOOLINFO
ends

struct TOOLTIPTEXT
  hdr	   NMHDR
  lpszText dd ?
  szText   db 80 dup (?)
  hinst    dd ?
  uFlags   dd ?
ends

struct UDACCEL
  nSec dd ?
  nInc dd ?
ends

struct NM_UPDOWN
  hdr	 NMHDR
  iPos	 dd ?
  iDelta dd ?
ends

struct LV_ITEM
  mask	     dd ?
  iItem      dd ?
  iSubItem   dd ?
  state      dd ?
  stateMask  dd ?
  pszText    dd ?
  cchTextMax dd ?
  iImage     dd ?
  lParam     dd ?
  iIndent    dd ?
ends

struct LV_FINDINFO
  flags       dd ?
  psz	      dd ?
  lParam      dd ?
  pt	      POINT
  vkDirection dd ?
ends

struct LV_HITTESTINFO
  pt	POINT
  flags dd ?
  iItem dd ?
ends

struct LV_COLUMN
  mask	     dd ?
  fmt	     dd ?
  cx	     dd ?
  pszText    dd ?
  cchTextMax dd ?
  iSubItem   dd ?
ends

struct NM_LISTVIEW
  hdr	    NMHDR
  iItem     dd ?
  iSubItem  dd ?
  uNewState dd ?
  uOldState dd ?
  uChanged  dd ?
  ptAction  POINT
  lParam    dd ?
ends

struct NM_CACHEHINT
  hdr	NMHDR
  iFrom dd ?
  iTo	dd ?
ends

struct NM_FINDITEM
  hdr	 NMHDR
  iStart dd ?
  lvfi	 LV_FINDINFO
ends

struct LV_DISPINFO
  hdr  NMHDR
  item LV_ITEM
ends

struct LV_KEYDOWN
  hdr	NMHDR
  wVKey dw ?
  flags dd ?
ends

struct TV_ITEM
  mask		 dd ?
  hItem 	 dd ?
  state 	 dd ?
  stateMask	 dd ?
  pszText	 dd ?
  cchTextMax	 dd ?
  iImage	 dd ?
  iSelectedImage dd ?
  cChildren	 dd ?
  lParam	 dd ?
ends

struct TV_INSERTSTRUCT
  hParent      dd ?
  hInsertAfter dd ?
  item	       TV_ITEM
ends

struct TV_HITTESTINFO
  pt	POINT
  flags dd ?
  hItem dd ?
ends

struct TV_SORTCB
  hParent     dd ?
  lpfnCompare dd ?
  lParam      dd ?
ends

struct NM_TREEVIEW
  hdr	  NMHDR
  action  dd ?
  itemOld TV_ITEM
  itemNew TV_ITEM
  ptDrag  POINT
ends

struct TV_DISPINFO
  hdr  NMHDR
  item TV_ITEM
ends

struct TV_KEYDOWN
  hdr	NMHDR
  wVKey dw ?
  flags dd ?
ends

struct TC_ITEMHEADER
  mask	      dd ?
  lpReserved1 dd ?
  lpReserved2 dd ?
  pszText     dd ?
  cchTextMax  dd ?
  iImage      dd ?
ends

struct TC_ITEM
  mask	      dd ?
  lpReserved1 dd ?
  lpReserved2 dd ?
  pszText     dd ?
  cchTextMax  dd ?
  iImage      dd ?
  lParam      dd ?
ends

struct TC_HITTESTINFO
  pt	POINT
  flags dd ?
ends

struct TC_KEYDOWN
  hdr	NMHDR
  wVKey dw ?
  flags dd ?
ends

struct MC_HITTESTINFO
  cbSize dd ?
  pt	 POINT
  uHit	 dd ?
  st	 SYSTEMTIME
ends

struct NM_SELCHANGE
  nmhdr      NMHDR
  stSelStart SYSTEMTIME
  stSelEnd   SYSTEMTIME
ends

struct NM_DAYSTATE
  nmhdr       NMHDR
  stStart     SYSTEMTIME
  cDayState   dd ?
  prgDayState dd ?
ends

struct NM_DATETIMECHANGE
  nmhdr   NMHDR
  dwFlags dd ?
  st	  SYSTEMTIME
ends

struct NM_DATETIMESTRING
  nmhdr 	NMHDR
  pszUserString dd ?
  st		SYSTEMTIME
  dwFlags	dd ?
ends

struct NM_DATETIMEWMKEYDOWN
  nmhdr     NMHDR
  nVirtKey  dd ?
  pszFormat dd ?
  st	    SYSTEMTIME
ends

struct NM_DATETIMEFORMAT
  nmhdr      NMHDR
  pszFormat  dd ?
  st	     SYSTEMTIME
  pszDisplay dd ?
  szDisplay  db 64 dup (?)
ends

struct NM_DATETIMEFORMATQUERY
  nmhdr     NMHDR
  pszFormat dd ?
  szMax     SIZE
ends

struct INITCOMMONCONTROLSEX
  dwSize dd ?
  dwICC  dd ?
ends

; Common control window classes

HOTKEY_CLASS	   equ 'msctls_hotkey32'
PROGRESS_CLASS	   equ 'msctls_progress32'
STATUS_CLASS	   equ 'msctls_statusbar32'
TRACKBAR_CLASS	   equ 'msctls_trackbar32'
UPDOWN_CLASS	   equ 'msctls_updown32'
TOOLTIPS_CLASS	   equ 'tooltips_class32'
ANIMATE_CLASS	   equ 'SysAnimate32'
HEADER_CLASS	   equ 'SysHeader32'
LISTVIEW_CLASS	   equ 'SysListView32'
TREEVIEW_CLASS	   equ 'SysTreeView32'
TABCONTROL_CLASS   equ 'SysTabControl32'
MONTHCAL_CLASS	   equ 'SysMonthCal32'
DATETIMEPICK_CLASS equ 'SysDateTimePick32'
TOOLBAR_CLASS	   equ 'ToolbarWindow32'
REBAR_CLASS	   equ 'ReBarWindow32'

; Ranges for control message IDs

LVM_FIRST = 1000h
TV_FIRST  = 1100h
HDM_FIRST = 1200h
TCM_FIRST = 1300h
MCM_FIRST = 1000h
DTM_FIRST = 1000h
CCM_FIRST = 2000h

; Ranges for control notification IDs

NM_FIRST   = 0
LVN_FIRST  = -100
PSN_FIRST  = -200
HDN_FIRST  = -300
TVN_FIRST  = -400
TTN_FIRST  = -520
TCN_FIRST  = -550
CDN_FIRST  = -601
TBN_FIRST  = -700
UDN_FIRST  = -721
MCN_FIRST  = -750
DTN_FIRST  = -760
CBEN_FIRST = -800
RBN_FIRST  = -831

; Generic notifications

NM_OUTOFMEMORY = NM_FIRST - 1
NM_CLICK       = NM_FIRST - 2
NM_DBLCLK      = NM_FIRST - 3
NM_RETURN      = NM_FIRST - 4
NM_RCLICK      = NM_FIRST - 5
NM_RDBLCLK     = NM_FIRST - 6
NM_SETFOCUS    = NM_FIRST - 7
NM_KILLFOCUS   = NM_FIRST - 8
NM_CUSTOMDRAW  = NM_FIRST - 12

; Common control styles

CCS_TOP 	  = 01h
CCS_NOMOVEY	  = 02h
CCS_BOTTOM	  = 03h
CCS_NORESIZE	  = 04h
CCS_NOPARENTALIGN = 08h
CCS_ADJUSTABLE	  = 20h
CCS_NODIVIDER	  = 40h
CCS_VERT	  = 80h
CCS_LEFT	  = CCS_VERT or CCS_TOP
CCS_RIGHT	  = CCS_VERT or CCS_BOTTOM
CCS_NOMOVEX	  = CCS_VERT or CCS_NOMOVEY

; Owner-drawn control types

ODT_HEADER   = 100
ODT_TAB      = 101
ODT_LISTVIEW = 102

; InitCommonControlsEx classes

ICC_ANIMATE_CLASS      = 0080h
ICC_BAR_CLASSES        = 0004h
ICC_COOL_CLASSES       = 0400h
ICC_DATE_CLASSES       = 0100h
ICC_HOTKEY_CLASS       = 0040h
ICC_INTERNET_CLASSES   = 0800h
ICC_LISTVIEW_CLASSES   = 0001h
ICC_PAGESCROLLER_CLASS = 1000h
ICC_PROGRESS_CLASS     = 0020h
ICC_TAB_CLASSES        = 0008h
ICC_TREEVIEW_CLASSES   = 0002h
ICC_UPDOWN_CLASS       = 0010h
ICC_USEREX_CLASSES     = 0200h
ICC_WIN95_CLASSES      = 00FFh

; Shared messages

CCM_SETCOLORSCHEME   = CCM_FIRST + 2
CCM_GETCOLORSCHEME   = CCM_FIRST + 3
CCM_GETDROPTARGET    = CCM_FIRST + 4
CCM_SETUNICODEFORMAT = CCM_FIRST + 5
CCM_GETUNICODEFORMAT = CCM_FIRST + 6

; Property sheet page flags

PSP_DEFAULT	 = 0000h
PSP_DLGINDIRECT  = 0001h
PSP_USEHICON	 = 0002h
PSP_USEICONID	 = 0004h
PSP_USETITLE	 = 0008h
PSP_HASHELP	 = 0020h
PSP_USEREFPARENT = 0040h
PSP_USECALLBACK  = 0080h

; Property sheet page actions

PSPCB_RELEASE = 1
PSPCB_CREATE  = 2

; Property sheet header flags

PSH_DEFAULT	  = 0000h
PSH_PROPTITLE	  = 0001h
PSH_USEHICON	  = 0002h
PSH_USEICONID	  = 0004h
PSH_PROPSHEETPAGE = 0008h
PSH_MULTILINETABS = 0010h
PSH_WIZARD	  = 0020h
PSH_USEPSTARTPAGE = 0040h
PSH_NOAPPLYNOW	  = 0080h
PSH_USECALLBACK   = 0100h
PSH_HASHELP	  = 0200h
PSH_MODELESS	  = 0400h

; Property sheet actions

PSCB_INITIALIZED  = 1

; Property sheet notifications

PSN_SETACTIVE	= PSN_FIRST - 0
PSN_KILLACTIVE	= PSN_FIRST - 1
PSN_APPLY	= PSN_FIRST - 2
PSN_RESET	= PSN_FIRST - 3
PSN_HELP	= PSN_FIRST - 5
PSN_WIZBACK	= PSN_FIRST - 6
PSN_WIZNEXT	= PSN_FIRST - 7
PSN_WIZFINISH	= PSN_FIRST - 8
PSN_QUERYCANCEL = PSN_FIRST - 9

; Property sheet return values

PSNRET_NOERROR		    = 0
PSNRET_INVALID		    = 1
PSNRET_INVALID_NOCHANGEPAGE = 2

; Property sheet messages

PSM_SETCURSEL	    = WM_USER + 101
PSM_REMOVEPAGE	    = WM_USER + 102
PSM_ADDPAGE	    = WM_USER + 103
PSM_CHANGED	    = WM_USER + 104
PSM_RESTARTWINDOWS  = WM_USER + 105
PSM_REBOOTSYSTEM    = WM_USER + 106
PSM_CANCELTOCLOSE   = WM_USER + 107
PSM_QUERYSIBLINGS   = WM_USER + 108
PSM_UNCHANGED	    = WM_USER + 109
PSM_APPLY	    = WM_USER + 110
PSM_SETTITLE	    = WM_USER + 111
PSM_SETTITLEW	    = WM_USER + 120
PSM_SETWIZBUTTONS   = WM_USER + 112
PSM_PRESSBUTTON     = WM_USER + 113
PSM_SETCURSELID     = WM_USER + 114
PSM_SETFINISHTEXT   = WM_USER + 115
PSM_SETFINISHTEXTW  = WM_USER + 121
PSM_GETTABCONTROL   = WM_USER + 116
PSM_ISDIALOGMESSAGE = WM_USER + 117

; Property sheet buttons

PSBTN_BACK	      = 0
PSBTN_NEXT	      = 1
PSBTN_FINISH	      = 2
PSBTN_OK	      = 3
PSBTN_APPLYNOW	      = 4
PSBTN_CANCEL	      = 5
PSBTN_HELP	      = 6
PSWIZB_BACK	      = 1
PSWIZB_NEXT	      = 2
PSWIZB_FINISH	      = 4
PSWIZB_DISABLEDFINISH = 8
ID_PSRESTARTWINDOWS   = 2
ID_PSREBOOTSYSTEM     = ID_PSRESTARTWINDOWS or 1

; Property sheet sizes

PROP_SM_CXDLG  = 212
PROP_SM_CYDLG  = 188
PROP_MED_CXDLG = 227
PROP_MED_CYDLG = 215
PROP_LG_CXDLG  = 252
PROP_LG_CYDLG  = 218
WIZ_CXDLG      = 276
WIZ_CYDLG      = 140
WIZ_CXBMP      = 80
WIZ_BODYX      = 92
WIZ_BODYCX     = 184

; Image list types

ILC_MASK     = 001h
ILC_COLOR    = 0FEh
ILC_COLORDDB = 0FEh
ILC_COLOR4   = 004h
ILC_COLOR8   = 008h
ILC_COLOR16  = 010h
ILC_COLOR24  = 018h
ILC_COLOR32  = 020h
ILC_PALETTE  = 800h

; Image list color values

CLR_NONE    = 0FFFFFFFFh
CLR_DEFAULT = 0FF000000h
CLR_HILIGHT = CLR_DEFAULT

; Image list drawing styles

ILD_NORMAL	= 0000h
ILD_TRANSPARENT = 0001h
ILD_MASK	= 0010h
ILD_IMAGE	= 0020h
ILD_BLEND25	= 0002h
ILD_BLEND50	= 0004h
ILD_OVERLAYMASK = 0F00h
ILD_SELECTED	= ILD_BLEND50
ILD_FOCUS	= ILD_BLEND25
ILD_BLEND	= ILD_BLEND50

; Header control styles

HDS_HORZ     = 00h
HDS_BUTTONS  = 02h
HDS_HOTTRACK = 04h
HDS_HIDDEN   = 08h
HDS_DRAGDROP = 40h
HDS_FULLDRAG = 80h

; Header control structure flags

HDI_WIDTH  = 01h
HDI_HEIGHT = HDI_WIDTH
HDI_TEXT   = 02h
HDI_FORMAT = 04h
HDI_LPARAM = 08h
HDI_BITMAP = 10h

; Header control flags

HDF_LEFT	= 0000h
HDF_RIGHT	= 0001h
HDF_CENTER	= 0002h
HDF_JUSTIFYMASK = 0003h
HDF_RTLREADING	= 0004h
HDF_BITMAP	= 2000h
HDF_STRING	= 4000h
HDF_OWNERDRAW	= 8000h

; Header control messages

HDM_GETITEMCOUNT = HDM_FIRST + 0
HDM_INSERTITEMA  = HDM_FIRST + 1
HDM_DELETEITEM	 = HDM_FIRST + 2
HDM_GETITEMA	 = HDM_FIRST + 3
HDM_SETITEMA	 = HDM_FIRST + 4
HDM_LAYOUT	 = HDM_FIRST + 5
HDM_HITTEST	 = HDM_FIRST + 6
HDM_INSERTITEMW  = HDM_FIRST + 10
HDM_GETITEMW	 = HDM_FIRST + 11
HDM_SETITEMW	 = HDM_FIRST + 12
HDM_INSERTITEM	 = HDM_INSERTITEMA
HDM_GETITEM	 = HDM_GETITEMA
HDM_SETITEM	 = HDM_SETITEMA

; Hit test result flags

HHT_NOWHERE   = 001h
HHT_ONHEADER  = 002h
HHT_ONDIVIDER = 004h
HHT_ONDIVOPEN = 008h
HHT_ABOVE     = 100h
HHT_BELOW     = 200h
HHT_TORIGHT   = 400h
HHT_TOLEFT    = 800h

; Header control notifications

HDN_ITEMCHANGINGA    = HDN_FIRST - 0
HDN_ITEMCHANGEDA     = HDN_FIRST - 1
HDN_ITEMCLICKA	     = HDN_FIRST - 2
HDN_ITEMDBLCLICKA    = HDN_FIRST - 3
HDN_DIVIDERDBLCLICKA = HDN_FIRST - 5
HDN_BEGINTRACKA      = HDN_FIRST - 6
HDN_ENDTRACKA	     = HDN_FIRST - 7
HDN_TRACKA	     = HDN_FIRST - 8
HDN_ITEMCHANGINGW    = HDN_FIRST - 20
HDN_ITEMCHANGEDW     = HDN_FIRST - 21
HDN_ITEMCLICKW	     = HDN_FIRST - 22
HDN_ITEMDBLCLICKW    = HDN_FIRST - 23
HDN_DIVIDERDBLCLICKW = HDN_FIRST - 25
HDN_BEGINTRACKW      = HDN_FIRST - 26
HDN_ENDTRACKW	     = HDN_FIRST - 27
HDN_TRACKW	     = HDN_FIRST - 28
HDN_ITEMCHANGING     = HDN_ITEMCHANGINGA
HDN_ITEMCHANGED      = HDN_ITEMCHANGEDA
HDN_ITEMCLICK	     = HDN_ITEMCLICKA
HDN_ITEMDBLCLICK     = HDN_ITEMDBLCLICKA
HDN_DIVIDERDBLCLICK  = HDN_DIVIDERDBLCLICKA
HDN_BEGINTRACK	     = HDN_BEGINTRACKA
HDN_ENDTRACK	     = HDN_ENDTRACKA
HDN_TRACK	     = HDN_TRACKA

; Toolbar bitmap flags

CMB_MASKED = 2

; Toolbar button states

TBSTATE_CHECKED       = 01h
TBSTATE_PRESSED       = 02h
TBSTATE_ENABLED       = 04h
TBSTATE_HIDDEN	      = 08h
TBSTATE_INDETERMINATE = 10h
TBSTATE_WRAP	      = 20h
TBSTATE_ELLIPSES      = 40h

; Toolbar button styles

TBSTYLE_BUTTON	    = 0000h
TBSTYLE_SEP	    = 0001h
TBSTYLE_CHECK	    = 0002h
TBSTYLE_GROUP	    = 0004h
TBSTYLE_CHECKGROUP  = TBSTYLE_GROUP or TBSTYLE_CHECK
TBSTYLE_DROPDOWN    = 0008h
TBSTYLE_TOOLTIPS    = 0100h
TBSTYLE_WRAPABLE    = 0200h
TBSTYLE_ALTDRAG     = 0400h
TBSTYLE_FLAT	    = 0800h
TBSTYLE_LIST	    = 1000h
TBSTYLE_CUSTOMERASE = 2000h
TBSTYLE_TRANSPARENT = 8000h

; Toolbar button extended styles

TBSTYLE_EX_DRAWDDARROWS = 0001h

; Toolbar messages

TB_ENABLEBUTTON 	 = WM_USER + 1
TB_CHECKBUTTON		 = WM_USER + 2
TB_PRESSBUTTON		 = WM_USER + 3
TB_HIDEBUTTON		 = WM_USER + 4
TB_INDETERMINATE	 = WM_USER + 5
TB_ISBUTTONENABLED	 = WM_USER + 9
TB_ISBUTTONCHECKED	 = WM_USER + 10
TB_ISBUTTONPRESSED	 = WM_USER + 11
TB_ISBUTTONHIDDEN	 = WM_USER + 12
TB_ISBUTTONINDETERMINATE = WM_USER + 13
TB_SETSTATE		 = WM_USER + 17
TB_GETSTATE		 = WM_USER + 18
TB_ADDBITMAP		 = WM_USER + 19
TB_ADDBUTTONS		 = WM_USER + 20
TB_INSERTBUTTON 	 = WM_USER + 21
TB_DELETEBUTTON 	 = WM_USER + 22
TB_GETBUTTON		 = WM_USER + 23
TB_BUTTONCOUNT		 = WM_USER + 24
TB_COMMANDTOINDEX	 = WM_USER + 25
TB_SAVERESTOREA 	 = WM_USER + 26
TB_ADDSTRINGA		 = WM_USER + 28
TB_CUSTOMIZE		 = WM_USER + 27
TB_GETITEMRECT		 = WM_USER + 29
TB_BUTTONSTRUCTSIZE	 = WM_USER + 30
TB_SETBUTTONSIZE	 = WM_USER + 31
TB_SETBITMAPSIZE	 = WM_USER + 32
TB_AUTOSIZE		 = WM_USER + 33
TB_GETTOOLTIPS		 = WM_USER + 35
TB_SETTOOLTIPS		 = WM_USER + 36
TB_SETPARENT		 = WM_USER + 37
TB_SETROWS		 = WM_USER + 39
TB_GETROWS		 = WM_USER + 40
TB_GETBITMAPFLAGS	 = WM_USER + 41
TB_SETCMDID		 = WM_USER + 42
TB_CHANGEBITMAP 	 = WM_USER + 43
TB_GETBITMAP		 = WM_USER + 44
TB_GETBUTTONTEXTA	 = WM_USER + 45
TB_REPLACEBITMAP	 = WM_USER + 46
TB_SETINDENT		 = WM_USER + 47
TB_SETIMAGELIST 	 = WM_USER + 48
TB_GETIMAGELIST 	 = WM_USER + 49
TB_LOADIMAGES		 = WM_USER + 50
TB_GETRECT		 = WM_USER + 51
TB_SETHOTIMAGELIST	 = WM_USER + 52
TB_GETHOTIMAGELIST	 = WM_USER + 53
TB_SETDISABLEDIMAGELIST  = WM_USER + 54
TB_GETDISABLEDIMAGELIST  = WM_USER + 55
TB_SETSTYLE		 = WM_USER + 56
TB_GETSTYLE		 = WM_USER + 57
TB_GETBUTTONSIZE	 = WM_USER + 58
TB_SETBUTTONWIDTH	 = WM_USER + 59
TB_SETMAXTEXTROWS	 = WM_USER + 60
TB_GETTEXTROWS		 = WM_USER + 61
TB_GETBUTTONTEXTW	 = WM_USER + 75
TB_SAVERESTOREW 	 = WM_USER + 76
TB_ADDSTRINGW		 = WM_USER + 77
TB_SETEXTENDEDSTYLE	 = WM_USER + 84
TB_GETEXTENDEDSTYLE	 = WM_USER + 85
TB_GETBUTTONTEXT	 = TB_GETBUTTONTEXTA
TB_SAVERESTORE		 = TB_SAVERESTOREA
TB_ADDSTRING		 = TB_ADDSTRINGA

; System-defined button bitmaps

HINST_COMMCTRL	     = -1
IDB_STD_SMALL_COLOR  = 0
IDB_STD_LARGE_COLOR  = 1
IDB_VIEW_SMALL_COLOR = 4
IDB_VIEW_LARGE_COLOR = 5
IDB_HIST_SMALL_COLOR = 8
IDB_HIST_LARGE_COLOR = 9

; Icon indexes for standard bitmap

STD_CUT        = 0
STD_COPY       = 1
STD_PASTE      = 2
STD_UNDO       = 3
STD_REDOW      = 4
STD_DELETE     = 5
STD_FILENEW    = 6
STD_FILEOPEN   = 7
STD_FILESAVE   = 8
STD_PRINTPRE   = 9
STD_PROPERTIES = 10
STD_HELP       = 11
STD_FIND       = 12
STD_REPLACE    = 13
STD_PRINT      = 14

; Icon indexes for standard view bitmap

VIEW_LARGEICONS    = 0
VIEW_SMALLICONS    = 1
VIEW_LIST	   = 2
VIEW_DETAILS	   = 3
VIEW_SORTNAME	   = 4
VIEW_SORTSIZE	   = 5
VIEW_SORTDATE	   = 6
VIEW_SORTTYPE	   = 7
VIEW_PARENTFOLDER  = 8
VIEW_NETCONNECT    = 9
VIEW_NETDISCONNECT = 10
VIEW_NEWFOLDER	   = 11

; Icon indexes for history bitmap

HIST_BACK	    = 0
HIST_FORWARD	    = 1
HIST_FAVORITES	    = 2
HIST_ADDTOFAVORITES = 3
HIST_VIEWTREE	    = 4

; Toolbar bitmap flags

TBBF_LARGE = 1

; Toolbar notifications

TBN_GETBUTTONINFOA = TBN_FIRST - 0
TBN_BEGINDRAG	   = TBN_FIRST - 1
TBN_ENDDRAG	   = TBN_FIRST - 2
TBN_BEGINADJUST    = TBN_FIRST - 3
TBN_ENDADJUST	   = TBN_FIRST - 4
TBN_RESET	   = TBN_FIRST - 5
TBN_QUERYINSERT    = TBN_FIRST - 6
TBN_QUERYDELETE    = TBN_FIRST - 7
TBN_TOOLBARCHANGE  = TBN_FIRST - 8
TBN_CUSTHELP	   = TBN_FIRST - 9
TBN_DROPDOWN	   = TBN_FIRST - 10
TBN_CLOSEUP	   = TBN_FIRST - 11
TBN_GETBUTTONINFOW = TBN_FIRST - 20
TBN_GETBUTTONINFO  = TBN_GETBUTTONINFOA

; ReBar styles

RBS_TOOLTIPS	    = 100h
RBS_VARHEIGHT	    = 200h
RBS_BANDBORDERS     = 400h
RBS_FIXEDORDER	    = 800h
RBS_REGISTERDROP    = 1000h
RBS_AUTOSIZE	    = 2000h
RBS_VERTICALGRIPPER = 4000h
RBS_DBLCLKTOGGLE    = 8000h

; ReBar band info structure flags

RBBIM_STYLE	 = 001h
RBBIM_COLORS	 = 002h
RBBIM_TEXT	 = 004h
RBBIM_IMAGE	 = 008h
RBBIM_CHILD	 = 010h
RBBIM_CHILDSIZE  = 020h
RBBIM_SIZE	 = 040h
RBBIM_BACKGROUND = 080h
RBBIM_ID	 = 100h
RBBIM_IDEALSIZE  = 200h
RBBIM_LPARAM	 = 400h
RBBIM_HEADERSIZE = 800h

; ReBar band styles

RBBS_BREAK	    = 001h
RBBS_FIXEDSIZE	    = 002h
RBBS_CHILDEDGE	    = 004h
RBBS_HIDDEN	    = 008h
RBBS_NOVERT	    = 010h
RBBS_FIXEDBMP	    = 020h
RBBS_VARIABLEHEIGHT = 040h
RBBS_GRIPPERALWAYS  = 080h
RBBS_NOGRIPPER	    = 100h

; ReBar messages

RB_INSERTBANDA	    = WM_USER + 1
RB_DELETEBAND	    = WM_USER + 2
RB_GETBARINFO	    = WM_USER + 3
RB_SETBARINFO	    = WM_USER + 4
RB_GETBANDINFO	    = WM_USER + 5
RB_SETBANDINFOA     = WM_USER + 6
RB_SETPARENT	    = WM_USER + 7
RB_INSERTBANDW	    = WM_USER + 10
RB_SETBANDINFOW     = WM_USER + 11
RB_GETBANDCOUNT     = WM_USER + 12
RB_GETROWCOUNT	    = WM_USER + 13
RB_GETROWHEIGHT     = WM_USER + 14
RB_IDTOINDEX	    = WM_USER + 16
RB_GETTOOLTIPS	    = WM_USER + 17
RB_SETTOOLTIPS	    = WM_USER + 18
RB_SETBKCOLOR	    = WM_USER + 19
RB_GETBKCOLOR	    = WM_USER + 20
RB_SETTEXTCOLOR     = WM_USER + 21
RB_GETTEXTCOLOR     = WM_USER + 22
RB_SIZETORECT	    = WM_USER + 23
RB_BEGINDRAG	    = WM_USER + 24
RB_ENDDRAG	    = WM_USER + 25
RB_DRAGMOVE	    = WM_USER + 26
RB_GETBARHEIGHT     = WM_USER + 27
RB_GETBANDINFOW     = WM_USER + 28
RB_GETBANDINFOA     = WM_USER + 29
RB_MINIMIZEBAND     = WM_USER + 30
RB_MAXIMIZEBAND     = WM_USER + 31
RB_GETDROPTARGET    = CCM_GETDROPTARGET
RB_GETBANDBORDERS   = WM_USER + 34
RB_SHOWBAND	    = WM_USER + 35
RB_SETPALETTE	    = WM_USER + 37
RB_GETPALETTE	    = WM_USER + 38
RB_MOVEBAND	    = WM_USER + 39
RB_SETUNICODEFORMAT = CCM_SETUNICODEFORMAT
RB_GETUNICODEFORMAT = CCM_GETUNICODEFORMAT
RB_INSERTBAND	    = RB_INSERTBANDA
RB_SETBANDINFO	    = RB_SETBANDINFOA

; ReBar notifications

RBN_HEIGHTCHANGE  = RBN_FIRST - 0
RBN_GETOBJECT	  = RBN_FIRST - 1
RBN_LAYOUTCHANGED = RBN_FIRST - 2
RBN_AUTOSIZE	  = RBN_FIRST - 3
RBN_BEGINDRAG	  = RBN_FIRST - 4
RBN_ENDDRAG	  = RBN_FIRST - 5
RBN_DELETINGBAND  = RBN_FIRST - 6
RBN_DELETEDBAND   = RBN_FIRST - 7
RBN_CHILDSIZE	  = RBN_FIRST - 8

; Tooltip styles

TTS_ALWAYSTIP = 1
TTS_NOPREFIX  = 2

; Tooltip flags

TTF_IDISHWND   = 01h
TTF_CENTERTIP  = 02h
TTF_RTLREADING = 04h
TTF_SUBCLASS   = 10h

; Tooltip durations

TTDT_AUTOMATIC = 0
TTDT_RESHOW    = 1
TTDT_AUTOPOP   = 2
TTDT_INITIAL   = 3

; Tooltip messages

TTM_ACTIVATE	    = WM_USER + 1
TTM_SETDELAYTIME    = WM_USER + 3
TTM_ADDTOOLA	    = WM_USER + 4
TTM_DELTOOLA	    = WM_USER + 5
TTM_NEWTOOLRECTA    = WM_USER + 6
TTM_RELAYEVENT	    = WM_USER + 7
TTM_GETTOOLINFOA    = WM_USER + 8
TTM_SETTOOLINFOA    = WM_USER + 9
TTM_HITTESTA	    = WM_USER + 10
TTM_GETTEXTA	    = WM_USER + 11
TTM_UPDATETIPTEXTA  = WM_USER + 12
TTM_GETTOOLCOUNT    = WM_USER + 13
TTM_ENUMTOOLSA	    = WM_USER + 14
TTM_GETCURRENTTOOLA = WM_USER + 15
TTM_WINDOWFROMPOINT = WM_USER + 16
TTM_ADDTOOLW	    = WM_USER + 50
TTM_DELTOOLW	    = WM_USER + 51
TTM_NEWTOOLRECTW    = WM_USER + 52
TTM_GETTOOLINFOW    = WM_USER + 53
TTM_SETTOOLINFOW    = WM_USER + 54
TTM_HITTESTW	    = WM_USER + 55
TTM_GETTEXTW	    = WM_USER + 56
TTM_UPDATETIPTEXTW  = WM_USER + 57
TTM_ENUMTOOLSW	    = WM_USER + 58
TTM_GETCURRENTTOOLW = WM_USER + 59
TTM_ADDTOOL	    = TTM_ADDTOOLA
TTM_DELTOOL	    = TTM_DELTOOLA
TTM_NEWTOOLRECT     = TTM_NEWTOOLRECTA
TTM_GETTOOLINFO     = TTM_GETTOOLINFOA
TTM_SETTOOLINFO     = TTM_SETTOOLINFOA
TTM_HITTEST	    = TTM_HITTESTA
TTM_GETTEXT	    = TTM_GETTEXTA
TTM_UPDATETIPTEXT   = TTM_UPDATETIPTEXTA
TTM_ENUMTOOLS	    = TTM_ENUMTOOLSA
TTM_GETCURRENTTOOL  = TTM_GETCURRENTTOOLA

; Tooltip notifications

TTN_NEEDTEXTA = TTN_FIRST - 0
TTN_SHOW      = TTN_FIRST - 1
TTN_POP       = TTN_FIRST - 2
TTN_NEEDTEXTW = TTN_FIRST - 10
TTN_NEEDTEXT  = TTN_NEEDTEXTA

; Status bar styles

SBARS_SIZEGRIP = 100h

; Status bar messages

SB_SETTEXTA	  = WM_USER + 1
SB_GETTEXTA	  = WM_USER + 2
SB_GETTEXTLENGTHA = WM_USER + 3
SB_SETPARTS	  = WM_USER + 4
SB_GETPARTS	  = WM_USER + 6
SB_GETBORDERS	  = WM_USER + 7
SB_SETMINHEIGHT   = WM_USER + 8
SB_SIMPLE	  = WM_USER + 9
SB_GETRECT	  = WM_USER + 10
SB_SETTEXTW	  = WM_USER + 11
SB_GETTEXTW	  = WM_USER + 13
SB_GETTEXTLENGTHW = WM_USER + 12
SB_SETTEXT	  = SB_SETTEXTA
SB_GETTEXT	  = SB_GETTEXTA
SB_GETTEXTLENGTH  = SB_GETTEXTLENGTHA

; Status bar drawing types

SBT_OWNERDRAW  = 1000h
SBT_NOBORDERS  = 0100h
SBT_POPOUT     = 0200h
SBT_RTLREADING = 0400h

; Trackbar styles

TBS_AUTOTICKS	   = 01h
TBS_VERT	   = 02h
TBS_HORZ	   = 00h
TBS_TOP 	   = 04h
TBS_BOTTOM	   = 00h
TBS_LEFT	   = 04h
TBS_RIGHT	   = 00h
TBS_BOTH	   = 08h
TBS_NOTICKS	   = 10h
TBS_ENABLESELRANGE = 20h
TBS_FIXEDLENGTH    = 40h
TBS_NOTHUMB	   = 80h

; Trackbar messages

TBM_GETPOS	   = WM_USER + 0
TBM_GETRANGEMIN    = WM_USER + 1
TBM_GETRANGEMAX    = WM_USER + 2
TBM_GETTIC	   = WM_USER + 3
TBM_SETTIC	   = WM_USER + 4
TBM_SETPOS	   = WM_USER + 5
TBM_SETRANGE	   = WM_USER + 6
TBM_SETRANGEMIN    = WM_USER + 7
TBM_SETRANGEMAX    = WM_USER + 8
TBM_CLEARTICS	   = WM_USER + 9
TBM_SETSEL	   = WM_USER + 10
TBM_SETSELSTART    = WM_USER + 11
TBM_SETSELEND	   = WM_USER + 12
TBM_GETPTICS	   = WM_USER + 14
TBM_GETTICPOS	   = WM_USER + 15
TBM_GETNUMTICS	   = WM_USER + 16
TBM_GETSELSTART    = WM_USER + 17
TBM_GETSELEND	   = WM_USER + 18
TBM_CLEARSEL	   = WM_USER + 19
TBM_SETTICFREQ	   = WM_USER + 20
TBM_SETPAGESIZE    = WM_USER + 21
TBM_GETPAGESIZE    = WM_USER + 22
TBM_SETLINESIZE    = WM_USER + 23
TBM_GETLINESIZE    = WM_USER + 24
TBM_GETTHUMBRECT   = WM_USER + 25
TBM_GETCHANNELRECT = WM_USER + 26
TBM_SETTHUMBLENGTH = WM_USER + 27
TBM_GETTHUMBLENGTH = WM_USER + 28

; Trackbar notifications

TB_LINEUP	 = 0
TB_LINEDOWN	 = 1
TB_PAGEUP	 = 2
TB_PAGEDOWN	 = 3
TB_THUMBPOSITION = 4
TB_THUMBTRACK	 = 5
TB_TOP		 = 6
TB_BOTTOM	 = 7
TB_ENDTRACK	 = 8

; Up-down control styles

UDS_WRAP	= 01h
UDS_SETBUDDYINT = 02h
UDS_ALIGNRIGHT	= 04h
UDS_ALIGNLEFT	= 08h
UDS_AUTOBUDDY	= 10h
UDS_ARROWKEYS	= 20h
UDS_HORZ	= 40h
UDS_NOTHOUSANDS = 80h

; Up-down control messages

UDM_SETRANGE = WM_USER + 101
UDM_GETRANGE = WM_USER + 102
UDM_SETPOS   = WM_USER + 103
UDM_GETPOS   = WM_USER + 104
UDM_SETBUDDY = WM_USER + 105
UDM_GETBUDDY = WM_USER + 106
UDM_SETACCEL = WM_USER + 107
UDM_GETACCEL = WM_USER + 108
UDM_SETBASE  = WM_USER + 109
UDM_GETBASE  = WM_USER + 110

; Up-down control notifications

UDN_DELTAPOS = UDN_FIRST - 1

; Progress bar messages

PBM_SETRANGE   = WM_USER + 1
PBM_SETPOS     = WM_USER + 2
PBM_DELTAPOS   = WM_USER + 3
PBM_SETSTEP    = WM_USER + 4
PBM_STEPIT     = WM_USER + 5
PBM_SETRANGE32 = WM_USER + 6
PBM_GETRANGE   = WM_USER + 7
PBM_GETPOS     = WM_USER + 8

; Hot-key control messages

HKM_SETHOTKEY = WM_USER + 1
HKM_GETHOTKEY = WM_USER + 2
HKM_SETRULES  = WM_USER + 3

; Hot key flags

HOTKEYF_SHIFT	= 1
HOTKEYF_CONTROL = 2
HOTKEYF_ALT	= 4
HOTKEYF_EXT	= 8

; Key combination flags

HKCOMB_NONE = 01h
HKCOMB_S    = 02h
HKCOMB_C    = 04h
HKCOMB_A    = 08h
HKCOMB_SC   = 10h
HKCOMB_SA   = 20h
HKCOMB_CA   = 40h
HKCOMB_SCA  = 80h

; List view styles

LVS_ICON	    = 0000h
LVS_REPORT	    = 0001h
LVS_SMALLICON	    = 0002h
LVS_LIST	    = 0003h
LVS_TYPEMASK	    = 0003h
LVS_SINGLESEL	    = 0004h
LVS_SHOWSELALWAYS   = 0008h
LVS_SORTASCENDING   = 0010h
LVS_SORTDESCENDING  = 0020h
LVS_SHAREIMAGELISTS = 0040h
LVS_NOLABELWRAP     = 0080h
LVS_AUTOARRANGE     = 0100h
LVS_EDITLABELS	    = 0200h
LVS_OWNERDATA	    = 1000h
LVS_NOSCROLL	    = 2000h
LVS_ALIGNTOP	    = 0000h
LVS_ALIGNLEFT	    = 0800h
LVS_OWNERDRAWFIXED  = 0400h
LVS_NOCOLUMNHEADER  = 4000h
LVS_NOSORTHEADER    = 8000h

; List view extended styles

LVS_EX_GRIDLINES	= 0001h
LVS_EX_SUBITEMIMAGES	= 0002h
LVS_EX_CHECKBOXES	= 0004h
LVS_EX_TRACKSELECT	= 0008h
LVS_EX_HEADERDRAGDROP	= 0010h
LVS_EX_FULLROWSELECT	= 0020h
LVS_EX_ONECLICKACTIVATE = 0040h
LVS_EX_TWOCLICKACTIVATE = 0080h
LVS_EX_FLATSB		= 0100h
LVS_EX_REGIONAL 	= 0200h
LVS_EX_INFOTIP		= 0400h
LVS_EX_UNDERLINEHOT	= 0800h
LVS_EX_UNDERLINECOLD	= 1000h
LVS_EX_MULTIWORKAREAS	= 2000h
LVS_EX_LABELTIP 	= 4000h

; List view messages

LVM_GETBKCOLOR		     = LVM_FIRST + 0
LVM_SETBKCOLOR		     = LVM_FIRST + 1
LVM_GETIMAGELIST	     = LVM_FIRST + 2
LVM_SETIMAGELIST	     = LVM_FIRST + 3
LVM_GETITEMCOUNT	     = LVM_FIRST + 4
LVM_GETITEMA		     = LVM_FIRST + 5
LVM_SETITEMA		     = LVM_FIRST + 6
LVM_INSERTITEMA 	     = LVM_FIRST + 7
LVM_DELETEITEM		     = LVM_FIRST + 8
LVM_DELETEALLITEMS	     = LVM_FIRST + 9
LVM_GETCALLBACKMASK	     = LVM_FIRST + 10
LVM_SETCALLBACKMASK	     = LVM_FIRST + 11
LVM_GETNEXTITEM 	     = LVM_FIRST + 12
LVM_FINDITEMA		     = LVM_FIRST + 13
LVM_GETITEMRECT 	     = LVM_FIRST + 14
LVM_SETITEMPOSITION	     = LVM_FIRST + 15
LVM_GETITEMPOSITION	     = LVM_FIRST + 16
LVM_GETSTRINGWIDTHA	     = LVM_FIRST + 17
LVM_HITTEST		     = LVM_FIRST + 18
LVM_ENSUREVISIBLE	     = LVM_FIRST + 19
LVM_SCROLL		     = LVM_FIRST + 20
LVM_REDRAWITEMS 	     = LVM_FIRST + 21
LVM_ARRANGE		     = LVM_FIRST + 22
LVM_EDITLABELA		     = LVM_FIRST + 23
LVM_GETEDITCONTROL	     = LVM_FIRST + 24
LVM_GETCOLUMNA		     = LVM_FIRST + 25
LVM_SETCOLUMNA		     = LVM_FIRST + 26
LVM_INSERTCOLUMNA	     = LVM_FIRST + 27
LVM_DELETECOLUMN	     = LVM_FIRST + 28
LVM_GETCOLUMNWIDTH	     = LVM_FIRST + 29
LVM_SETCOLUMNWIDTH	     = LVM_FIRST + 30
LVM_CREATEDRAGIMAGE	     = LVM_FIRST + 33
LVM_GETVIEWRECT 	     = LVM_FIRST + 34
LVM_GETTEXTCOLOR	     = LVM_FIRST + 35
LVM_SETTEXTCOLOR	     = LVM_FIRST + 36
LVM_GETTEXTBKCOLOR	     = LVM_FIRST + 37
LVM_SETTEXTBKCOLOR	     = LVM_FIRST + 38
LVM_GETTOPINDEX 	     = LVM_FIRST + 39
LVM_GETCOUNTPERPAGE	     = LVM_FIRST + 40
LVM_GETORIGIN		     = LVM_FIRST + 41
LVM_UPDATE		     = LVM_FIRST + 42
LVM_SETITEMSTATE	     = LVM_FIRST + 43
LVM_GETITEMSTATE	     = LVM_FIRST + 44
LVM_GETITEMTEXTA	     = LVM_FIRST + 45
LVM_SETITEMTEXTA	     = LVM_FIRST + 46
LVM_SETITEMCOUNT	     = LVM_FIRST + 47
LVM_SORTITEMS		     = LVM_FIRST + 48
LVM_SETITEMPOSITION32	     = LVM_FIRST + 49
LVM_GETSELECTEDCOUNT	     = LVM_FIRST + 50
LVM_GETITEMSPACING	     = LVM_FIRST + 51
LVM_GETISEARCHSTRINGA	     = LVM_FIRST + 52
LVM_SETICONSPACING	     = LVM_FIRST + 53
LVM_SETEXTENDEDLISTVIEWSTYLE = LVM_FIRST + 54
LVM_GETEXTENDEDLISTVIEWSTYLE = LVM_FIRST + 55
LVM_GETSUBITEMRECT	     = LVM_FIRST + 56
LVM_SUBITEMHITTEST	     = LVM_FIRST + 57
LVM_SETCOLUMNORDERARRAY      = LVM_FIRST + 58
LVM_GETCOLUMNORDERARRAY      = LVM_FIRST + 59
LVM_SETHOTITEM		     = LVM_FIRST + 60
LVM_GETHOTITEM		     = LVM_FIRST + 61
LVM_SETHOTCURSOR	     = LVM_FIRST + 62
LVM_GETHOTCURSOR	     = LVM_FIRST + 63
LVM_APPROXIMATEVIEWRECT      = LVM_FIRST + 64
LVM_SETWORKAREA 	     = LVM_FIRST + 65
LVM_GETITEMW		     = LVM_FIRST + 75
LVM_SETITEMW		     = LVM_FIRST + 76
LVM_INSERTITEMW 	     = LVM_FIRST + 77
LVM_FINDITEMW		     = LVM_FIRST + 83
LVM_GETSTRINGWIDTHW	     = LVM_FIRST + 87
LVM_GETCOLUMNW		     = LVM_FIRST + 95
LVM_SETCOLUMNW		     = LVM_FIRST + 96
LVM_INSERTCOLUMNW	     = LVM_FIRST + 97
LVM_GETITEMTEXTW	     = LVM_FIRST + 115
LVM_SETITEMTEXTW	     = LVM_FIRST + 116
LVM_GETISEARCHSTRINGW	     = LVM_FIRST + 117
LVM_EDITLABELW		     = LVM_FIRST + 118
LVM_GETITEM		     = LVM_GETITEMA
LVM_SETITEM		     = LVM_SETITEMA
LVM_INSERTITEM		     = LVM_INSERTITEMA
LVM_FINDITEM		     = LVM_FINDITEMA
LVM_GETSTRINGWIDTH	     = LVM_GETSTRINGWIDTHA
LVM_GETCOLUMN		     = LVM_GETCOLUMNA
LVM_SETCOLUMN		     = LVM_SETCOLUMNA
LVM_INSERTCOLUMN	     = LVM_INSERTCOLUMNA
LVM_GETITEMTEXT 	     = LVM_GETITEMTEXTA
LVM_SETITEMTEXT 	     = LVM_SETITEMTEXTA
LVM_GETISEARCHSTRING	     = LVM_GETISEARCHSTRINGA
LVM_EDITLABEL		     = LVM_EDITLABELA

; List view image list types

LVSIL_NORMAL = 0
LVSIL_SMALL  = 1
LVSIL_STATE  = 2

; LVM_SETITEMCOUNT flags

LVSICF_NOINVALIDATEALL = 1
LVSICF_NOSCROLL        = 2

; List view item structure flags

LVIF_TEXT	 = 0001h
LVIF_IMAGE	 = 0002h
LVIF_PARAM	 = 0004h
LVIF_STATE	 = 0008h
LVIF_INDENT	 = 0010h
LVIF_NORECOMPUTE = 0800h
LVIF_DI_SETITEM  = 1000h

; List view item states

LVIS_FOCUSED	    = 00001h
LVIS_SELECTED	    = 00002h
LVIS_CUT	    = 00004h
LVIS_DROPHILITED    = 00008h
LVIS_ACTIVATING  = 0020h
LVIS_OVERLAYMASK    = 00F00h
LVIS_STATEIMAGEMASK = 0F000h

; List view callback item values

LPSTR_TEXTCALLBACK = -1
I_IMAGECALLBACK    = -1
I_CHILDRENCALLBACK = -1

; List view next item relations

LVNI_ALL	 = 000h
LVNI_FOCUSED	 = 001h
LVNI_SELECTED	 = 002h
LVNI_CUT	 = 004h
LVNI_DROPHILITED = 008h
LVNI_ABOVE	 = 100h
LVNI_BELOW	 = 200h
LVNI_TOLEFT	 = 400h
LVNI_TORIGHT	 = 800h

; List view search types

LVFI_PARAM     = 01h
LVFI_STRING    = 02h
LVFI_PARTIAL   = 08h
LVFI_WRAP      = 20h
LVFI_NEARESTXY = 40h

; List view item rectangle types

LVIR_BOUNDS	  = 0
LVIR_ICON	  = 1
LVIR_LABEL	  = 2
LVIR_SELECTBOUNDS = 3

; List view hit test flags

LVHT_NOWHERE	    = 01h
LVHT_ONITEMICON     = 02h
LVHT_ONITEMLABEL    = 04h
LVHT_ONITEMSTATEICON= 08h
LVHT_ONITEM	    = LVHT_ONITEMICON or LVHT_ONITEMLABEL or LVHT_ONITEMSTATEICON
LVHT_ABOVE	    = 08h
LVHT_BELOW	    = 10h
LVHT_TORIGHT	    = 20h
LVHT_TOLEFT	    = 40h

; List view alignment values

LVA_DEFAULT	   = 000h
LVA_ALIGNLEFT	   = 001h
LVA_ALIGNTOP	   = 002h
LVA_ALIGNRIGHT	   = 003h
LVA_ALIGNBOTTOM    = 004h
LVA_SNAPTOGRID	   = 005h
LVA_SORTASCENDING  = 100h
LVA_SORTDESCENDING = 200h

; List view column structure flags

LVCF_FMT     = 1
LVCF_WIDTH   = 2
LVCF_TEXT    = 4
LVCF_SUBITEM = 8

; List view column alignment values

LVCFMT_LEFT	   = 0
LVCFMT_RIGHT	   = 1
LVCFMT_CENTER	   = 2
LVCFMT_JUSTIFYMASK = 3

; List view column width values

LVSCW_AUTOSIZE		 = -1
LVSCW_AUTOSIZE_USEHEADER = -2

; List view notifications

LVN_ITEMCHANGING    = LVN_FIRST - 0
LVN_ITEMCHANGED     = LVN_FIRST - 1
LVN_INSERTITEM	    = LVN_FIRST - 2
LVN_DELETEITEM	    = LVN_FIRST - 3
LVN_DELETEALLITEMS  = LVN_FIRST - 4
LVN_BEGINLABELEDITA = LVN_FIRST - 5
LVN_ENDLABELEDITA   = LVN_FIRST - 6
LVN_COLUMNCLICK     = LVN_FIRST - 8
LVN_BEGINDRAG	    = LVN_FIRST - 9
LVN_BEGINRDRAG	    = LVN_FIRST - 11
LVN_ODCACHEHINT     = LVN_FIRST - 13
LVN_GETDISPINFOA    = LVN_FIRST - 50
LVN_SETDISPINFOA    = LVN_FIRST - 51
LVN_ODFINDITEMA     = LVN_FIRST - 52
LVN_KEYDOWN	    = LVN_FIRST - 55
LVN_BEGINLABELEDITW = LVN_FIRST - 75
LVN_ENDLABELEDITW   = LVN_FIRST - 76
LVN_GETDISPINFOW    = LVN_FIRST - 77
LVN_SETDISPINFOW    = LVN_FIRST - 78
LVN_ODFINDITEMW     = LVN_FIRST - 79
LVN_BEGINLABELEDIT  = LVN_BEGINLABELEDITA
LVN_ENDLABELEDIT    = LVN_ENDLABELEDITA
LVN_GETDISPINFO     = LVN_GETDISPINFOA
LVN_SETDISPINFO     = LVN_SETDISPINFOA
LVN_ODFINDITEM	    = LVN_ODFINDITEMA

; Tree view styles

TVS_HASBUTTONS	    = 0001h
TVS_HASLINES	    = 0002h
TVS_LINESATROOT     = 0004h
TVS_EDITLABELS	    = 0008h
TVS_DISABLEDRAGDROP = 0010h
TVS_SHOWSELALWAYS   = 0020h
TVS_RTLREADING	    = 0040h
TVS_NOTOOLTIPS	    = 0080h
TVS_CHECKBOXES	    = 0100h
TVS_TRACKSELECT     = 0200h
TVS_SINGLEEXPAND    = 0400h
TVS_INFOTIP	    = 0800h
TVS_FULLROWSELECT   = 1000h
TVS_NOSCROLL	    = 2000h
TVS_NONEVENHEIGHT   = 4000h

; Tree view item structure flags

TVIF_TEXT	   = 0001h
TVIF_IMAGE	   = 0002h
TVIF_PARAM	   = 0004h
TVIF_STATE	   = 0008h
TVIF_HANDLE	   = 0010h
TVIF_SELECTEDIMAGE = 0020h
TVIF_CHILDREN	   = 0040h
TVIF_DI_SETITEM    = 1000h

; Tree view item states

TVIS_FOCUSED	    = 00001h
TVIS_SELECTED	    = 00002h
TVIS_CUT	    = 00004h
TVIS_DROPHILITED    = 00008h
TVIS_BOLD	    = 00010h
TVIS_EXPANDED	    = 00020h
TVIS_EXPANDEDONCE   = 00040h
TVIS_EXPANDPARTIAL  = 00080h
TVIS_OVERLAYMASK    = 00F00h
TVIS_STATEIMAGEMASK = 0F000h
TVIS_USERMASK	    = 0F000h

; Tree view predefined item values

TVI_ROOT  = 0FFFF0000h
TVI_FIRST = 0FFFF0001h
TVI_LAST  = 0FFFF0002h
TVI_SORT  = 0FFFF0003h

; Tree view messages

TVM_INSERTITEMA       = TV_FIRST + 0
TVM_DELETEITEM	      = TV_FIRST + 1
TVM_EXPAND	      = TV_FIRST + 2
TVM_GETITEMRECT       = TV_FIRST + 4
TVM_GETCOUNT	      = TV_FIRST + 5
TVM_GETINDENT	      = TV_FIRST + 6
TVM_SETINDENT	      = TV_FIRST + 7
TVM_GETIMAGELIST      = TV_FIRST + 8
TVM_SETIMAGELIST      = TV_FIRST + 9
TVM_GETNEXTITEM       = TV_FIRST + 10
TVM_SELECTITEM	      = TV_FIRST + 11
TVM_GETITEMA	      = TV_FIRST + 12
TVM_SETITEMA	      = TV_FIRST + 13
TVM_EDITLABELA	      = TV_FIRST + 14
TVM_GETEDITCONTROL    = TV_FIRST + 15
TVM_GETVISIBLECOUNT   = TV_FIRST + 16
TVM_HITTEST	      = TV_FIRST + 17
TVM_CREATEDRAGIMAGE   = TV_FIRST + 18
TVM_SORTCHILDREN      = TV_FIRST + 19
TVM_ENSUREVISIBLE     = TV_FIRST + 20
TVM_SORTCHILDRENCB    = TV_FIRST + 21
TVM_ENDEDITLABELNOW   = TV_FIRST + 22
TVM_GETISEARCHSTRINGA = TV_FIRST + 23
TVM_INSERTITEMW       = TV_FIRST + 50
TVM_GETITEMW	      = TV_FIRST + 62
TVM_SETITEMW	      = TV_FIRST + 63
TVM_GETISEARCHSTRINGW = TV_FIRST + 64
TVM_EDITLABELW	      = TV_FIRST + 65
TVM_INSERTITEM	      = TVM_INSERTITEMA
TVM_GETITEM	      = TVM_GETITEMA
TVM_SETITEM	      = TVM_SETITEMA
TVM_GETISEARCHSTRING  = TVM_GETISEARCHSTRINGA
TVM_EDITLABEL	      = TVM_EDITLABELA

; Tree view action flags

TVE_COLLAPSE	  = 0001h
TVE_EXPAND	  = 0002h
TVE_TOGGLE	  = 0003h
TVE_EXPANDPARTIAL = 4000h
TVE_COLLAPSERESET = 8000h

; Tree view image list types

TVSIL_NORMAL = 0
TVSIL_STATE  = 2

; Tree view next item types

TVGN_ROOT	     = 0
TVGN_NEXT	     = 1
TVGN_PREVIOUS	     = 2
TVGN_PARENT	     = 3
TVGN_CHILD	     = 4
TVGN_FIRSTVISIBLE    = 5
TVGN_NEXTVISIBLE     = 6
TVGN_PREVIOUSVISIBLE = 7
TVGN_DROPHILITE      = 8
TVGN_CARET	     = 9

; Tree view hit test flags

TVHT_NOWHERE	     = 001h
TVHT_ONITEMICON      = 002h
TVHT_ONITEMLABEL     = 004h
TVHT_ONITEMINDENT    = 008h
TVHT_ONITEMBUTTON    = 010h
TVHT_ONITEMRIGHT     = 020h
TVHT_ONITEMSTATEICON = 040h
TVHT_ONITEM	     = TVHT_ONITEMICON or TVHT_ONITEMLABEL or TVHT_ONITEMSTATEICON
TVHT_ABOVE	     = 100h
TVHT_BELOW	     = 200h
TVHT_TORIGHT	     = 400h
TVHT_TOLEFT	     = 800h

; Tree view notifications

TVN_SELCHANGINGA    = TVN_FIRST - 1
TVN_SELCHANGEDA     = TVN_FIRST - 2
TVN_GETDISPINFOA    = TVN_FIRST - 3
TVN_SETDISPINFOA    = TVN_FIRST - 4
TVN_ITEMEXPANDINGA  = TVN_FIRST - 5
TVN_ITEMEXPANDEDA   = TVN_FIRST - 6
TVN_BEGINDRAGA	    = TVN_FIRST - 7
TVN_BEGINRDRAGA     = TVN_FIRST - 8
TVN_DELETEITEMA     = TVN_FIRST - 9
TVN_BEGINLABELEDITA = TVN_FIRST - 10
TVN_ENDLABELEDITA   = TVN_FIRST - 11
TVN_KEYDOWN	    = TVN_FIRST - 12
TVN_SELCHANGINGW    = TVN_FIRST - 50
TVN_SELCHANGEDW     = TVN_FIRST - 51
TVN_GETDISPINFOW    = TVN_FIRST - 52
TVN_SETDISPINFOW    = TVN_FIRST - 53
TVN_ITEMEXPANDINGW  = TVN_FIRST - 54
TVN_ITEMEXPANDEDW   = TVN_FIRST - 55
TVN_BEGINDRAGW	    = TVN_FIRST - 56
TVN_BEGINRDRAGW     = TVN_FIRST - 57
TVN_DELETEITEMW     = TVN_FIRST - 58
TVN_BEGINLABELEDITW = TVN_FIRST - 59
TVN_ENDLABELEDITW   = TVN_FIRST - 60
TVN_SELCHANGING     = TVN_SELCHANGINGA
TVN_SELCHANGED	    = TVN_SELCHANGEDA
TVN_GETDISPINFO     = TVN_GETDISPINFOA
TVN_SETDISPINFO     = TVN_SETDISPINFOA
TVN_ITEMEXPANDING   = TVN_ITEMEXPANDINGA
TVN_ITEMEXPANDED    = TVN_ITEMEXPANDEDA
TVN_BEGINDRAG	    = TVN_BEGINDRAGA
TVN_BEGINRDRAG	    = TVN_BEGINRDRAGA
TVN_DELETEITEM	    = TVN_DELETEITEMA
TVN_BEGINLABELEDIT  = TVN_BEGINLABELEDITA
TVN_ENDLABELEDIT    = TVN_ENDLABELEDITA

; Tree view action flags

TVC_UNKNOWN    = 0
TVC_BYMOUSE    = 1
TVC_BYKEYBOARD = 2

; Tab control styles

TCS_SCROLLOPPOSITE    = 0001h
TCS_BOTTOM	      = 0002h
TCS_RIGHT	      = 0002h
TCS_FORCEICONLEFT     = 0010h
TCS_FORCELABELLEFT    = 0020h
TCS_HOTTRACK	      = 0040h
TCS_VERTICAL	      = 0080h
TCS_TABS	      = 0000h
TCS_BUTTONS	      = 0100h
TCS_SINGLELINE	      = 0000h
TCS_MULTILINE	      = 0200h
TCS_RIGHTJUSTIFY      = 0000h
TCS_FIXEDWIDTH	      = 0400h
TCS_RAGGEDRIGHT       = 0800h
TCS_FOCUSONBUTTONDOWN = 1000h
TCS_OWNERDRAWFIXED    = 2000h
TCS_TOOLTIPS	      = 4000h
TCS_FOCUSNEVER	      = 8000h

; Tab control messages

TCM_GETIMAGELIST   = TCM_FIRST + 2
TCM_SETIMAGELIST   = TCM_FIRST + 3
TCM_GETITEMCOUNT   = TCM_FIRST + 4
TCM_GETITEMA	   = TCM_FIRST + 5
TCM_SETITEMA	   = TCM_FIRST + 6
TCM_INSERTITEMA    = TCM_FIRST + 7
TCM_DELETEITEM	   = TCM_FIRST + 8
TCM_DELETEALLITEMS = TCM_FIRST + 9
TCM_GETITEMRECT    = TCM_FIRST + 10
TCM_GETCURSEL	   = TCM_FIRST + 11
TCM_SETCURSEL	   = TCM_FIRST + 12
TCM_HITTEST	   = TCM_FIRST + 13
TCM_SETITEMEXTRA   = TCM_FIRST + 14
TCM_ADJUSTRECT	   = TCM_FIRST + 40
TCM_SETITEMSIZE    = TCM_FIRST + 41
TCM_REMOVEIMAGE    = TCM_FIRST + 42
TCM_SETPADDING	   = TCM_FIRST + 43
TCM_GETROWCOUNT    = TCM_FIRST + 44
TCM_GETTOOLTIPS    = TCM_FIRST + 45
TCM_SETTOOLTIPS    = TCM_FIRST + 46
TCM_GETCURFOCUS    = TCM_FIRST + 47
TCM_SETCURFOCUS    = TCM_FIRST + 48
TCM_GETITEMW	   = TCM_FIRST + 60
TCM_SETITEMW	   = TCM_FIRST + 61
TCM_INSERTITEMW    = TCM_FIRST + 62
TCM_GETITEM	   = TCM_GETITEMA
TCM_SETITEM	   = TCM_SETITEMA
TCM_INSERTITEM	   = TCM_INSERTITEMA

; Tab control item structure flags

TCIF_TEXT	= 1
TCIF_IMAGE	= 2
TCIF_RTLREADING = 4
TCIF_PARAM	= 8

; Tab control hit test flags

TCHT_NOWHERE	 = 1
TCHT_ONITEMICON  = 2
TCHT_ONITEMLABEL = 4
TCHT_ONITEM	 = TCHT_ONITEMICON or TCHT_ONITEMLABEL

; Tab control notifications

TCN_KEYDOWN	= TCN_FIRST - 0
TCN_SELCHANGE	= TCN_FIRST - 1
TCN_SELCHANGING = TCN_FIRST - 2

; Animation control styles

ACS_CENTER	= 1
ACS_TRANSPARENT = 2
ACS_AUTOPLAY	= 4
ACS_TIMER	= 8

; Animation control messages

ACM_OPENA = WM_USER + 100
ACM_PLAY  = WM_USER + 101
ACM_STOP  = WM_USER + 102
ACM_OPENW = WM_USER + 103
ACM_OPEN  = ACM_OPENA

; Animation control notifications

ACN_START = 1
ACN_STOP  = 2

; Month calendar styles

MCS_DAYSTATE	    = 1
MCS_MULTISELECT     = 2
MCS_WEEKNUMBERS     = 4
MCS_NOTODAY_PRE_IE4 = 8
MCS_NOTODAYCIRCLE   = 8
MCS_NOTODAY	    = 16

; Month calendar messages

MCM_GETCURSEL	      = MCM_FIRST + 1
MCM_SETCURSEL	      = MCM_FIRST + 2
MCM_GETMAXSELCOUNT    = MCM_FIRST + 3
MCM_SETMAXSELCOUNT    = MCM_FIRST + 4
MCM_GETSELRANGE       = MCM_FIRST + 5
MCM_SETSELRANGE       = MCM_FIRST + 6
MCM_GETMONTHRANGE     = MCM_FIRST + 7
MCM_SETDAYSTATE       = MCM_FIRST + 8
MCM_GETMINREQRECT     = MCM_FIRST + 9
MCM_SETCOLOR	      = MCM_FIRST + 10
MCM_GETCOLOR	      = MCM_FIRST + 11
MCM_SETTODAY	      = MCM_FIRST + 12
MCM_GETTODAY	      = MCM_FIRST + 13
MCM_HITTEST	      = MCM_FIRST + 14
MCM_SETFIRSTDAYOFWEEK = MCM_FIRST + 15
MCM_GETFIRSTDAYOFWEEK = MCM_FIRST + 16
MCM_GETRANGE	      = MCM_FIRST + 17
MCM_SETRANGE	      = MCM_FIRST + 18
MCM_GETMONTHDELTA     = MCM_FIRST + 19
MCM_SETMONTHDELTA     = MCM_FIRST + 20

; Month calendar hit test flags

MCHT_TITLE	      = 0010000h
MCHT_CALENDAR	      = 0020000h
MCHT_TODAYLINK	      = 0030000h
MCHT_NEXT	      = 1000000h
MCHT_PREV	      = 2000000h
MCHT_NOWHERE	      = 0000000h
MCHT_TITLEBK	      = MCHT_TITLE
MCHT_TITLEMONTH       = MCHT_TITLE or 1
MCHT_TITLEYEAR	      = MCHT_TITLE or 2
MCHT_TITLEBTNNEXT     = MCHT_TITLE or MCHT_NEXT or 3
MCHT_TITLEBTNPREV     = MCHT_TITLE or MCHT_PREV or 3
MCHT_CALENDARBK       = MCHT_CALENDAR
MCHT_CALENDARDATE     = MCHT_CALENDAR or 1
MCHT_CALENDARDATENEXT = MCHT_CALENDARDATE or MCHT_NEXT
MCHT_CALENDARDATEPREV = MCHT_CALENDARDATE or MCHT_PREV
MCHT_CALENDARDAY      = MCHT_CALENDAR or 2
MCHT_CALENDARWEEKNUM  = MCHT_CALENDAR or 3

; Month calendar color codes

MCSC_BACKGROUND   = 0
MCSC_TEXT	  = 1
MCSC_TITLEBK	  = 2
MCSC_TITLETEXT	  = 3
MCSC_MONTHBK	  = 4
MCSC_TRAILINGTEXT = 5

; Month calendar notifications

MCN_SELCHANGE	= MCN_FIRST + 1
MCN_GETDAYSTATE = MCN_FIRST + 3
MCN_SELECT	= MCN_FIRST + 4

; Date-time pick control messages

DTM_GETSYSTEMTIME = DTM_FIRST + 1
DTM_SETSYSTEMTIME = DTM_FIRST + 2
DTM_GETRANGE	  = DTM_FIRST + 3
DTM_SETRANGE	  = DTM_FIRST + 4
DTM_SETFORMATA	  = DTM_FIRST + 5
DTM_SETMCCOLOR	  = DTM_FIRST + 6
DTM_GETMCCOLOR	  = DTM_FIRST + 7
DTM_GETMONTHCAL   = DTM_FIRST + 8
DTM_SETMCFONT	  = DTM_FIRST + 9
DTM_GETMCFONT	  = DTM_FIRST + 10
DTM_SETFORMATW	  = DTM_FIRST + 50
DTM_SETFORMAT	  = DTM_SETFORMATA

; Date-time pick control styles

DTS_UPDOWN	    = 01h
DTS_SHOWNONE	    = 02h
DTS_SHORTDATEFORMAT = 00h
DTS_LONGDATEFORMAT  = 04h
DTS_TIMEFORMAT	    = 09h
DTS_APPCANPARSE     = 10h
DTS_RIGHTALIGN	    = 20h

; Date-time pick control notifications

DTN_DATETIMECHANGE = DTN_FIRST + 1
DTN_USERSTRINGA    = DTN_FIRST + 2
DTN_WMKEYDOWNA	   = DTN_FIRST + 3
DTN_FORMATA	   = DTN_FIRST + 4
DTN_FORMATQUERYA   = DTN_FIRST + 5
DTN_DROPDOWN	   = DTN_FIRST + 6
DTN_CLOSEUP	   = DTN_FIRST + 7
DTN_USERSTRINGW    = DTN_FIRST + 15
DTN_WMKEYDOWNW	   = DTN_FIRST + 16
DTN_FORMATW	   = DTN_FIRST + 17
DTN_FORMATQUERYW   = DTN_FIRST + 18
DTN_USERSTRING	   = DTN_USERSTRINGA
DTN_WMKEYDOWN	   = DTN_WMKEYDOWNA
DTN_FORMAT	   = DTN_FORMATA
DTN_FORMATQUERY    = DTN_FORMATQUERYA

; ImageList_LoadImage types

IMAGE_BITMAP	  = 0
IMAGE_ICON	  = 1
IMAGE_CURSOR	  = 2
IMAGE_ENHMETAFILE = 3

; ImageList_LoadImage flags

LR_DEFAULTCOLOR     = 0000h
LR_MONOCHROME	    = 0001h
LR_COLOR	    = 0002h
LR_COPYRETURNORG    = 0004h
LR_COPYDELETEORG    = 0008h
LR_LOADFROMFILE     = 0010h
LR_LOADTRANSPARENT  = 0020h
LR_DEFAULTSIZE	    = 0040h
LR_VGACOLOR	    = 0080h
LR_LOADMAP3DCOLORS  = 1000h
LR_CREATEDIBSECTION = 2000h
LR_COPYFROMRESOURCE = 4000h
LR_SHARED	    = 8000h

; IP control messages

IPM_CLEARADDRESS = WM_USER + 100
IPM_SETADDRESS	 = WM_USER + 101
IPM_GETADDRESS	 = WM_USER + 102
IPM_SETRANGE	 = WM_USER + 103
IPM_SETFOCUS	 = WM_USER + 104
IPM_ISBLANK	 = WM_USER + 105

; Custom Draw flags

CDRF_DODEFAULT	       = 0
CDRF_NEWFONT	       = 2
CDRF_SKIPDEFAULT       = 4
CDRF_NOTIFYPOSTPAINT   = 10h
CDRF_NOTIFYITEMDRAW    = 20h
CDRF_NOTIFYSUBITEMDRAW = 20h
CDRF_NOTIFYPOSTERASE   = 40h
CDDS_PREPAINT	       = 1
CDDS_POSTPAINT	       = 2
CDDS_PREERASE	       = 3
CDDS_POSTERASE	       = 4
CDDS_ITEM	       = 10000h
CDDS_ITEMPREPAINT      = CDDS_ITEM or CDDS_PREPAINT
CDDS_ITEMPOSTPAINT     = CDDS_ITEM or CDDS_POSTPAINT
CDDS_ITEMPREERASE      = CDDS_ITEM or CDDS_PREERASE
CDDS_ITEMPOSTERASE     = CDDS_ITEM or CDDS_POSTERASE
CDDS_SUBITEM	       = 20000h
CDIS_SELECTED	       = 1
CDIS_GRAYED	       = 2
CDIS_DISABLED	       = 4
CDIS_CHECKED	       = 8
CDIS_FOCUS	       = 10h
CDIS_DEFAULT	       = 20h
CDIS_HOT	       = 40h
CDIS_MARKED	       = 80h
CDIS_INDETERMINATE     = 100h


; COMDLG32.DLL structures and constants

struct OPENFILENAME
  lStructSize       dd ?
  hwndOwner         dd ?
  hInstance         dd ?
  lpstrFilter       dd ?
  lpstrCustomFilter dd ?
  nMaxCustFilter    dd ?
  nFilterIndex      dd ?
  lpstrFile         dd ?
  nMaxFile          dd ?
  lpstrFileTitle    dd ?
  nMaxFileTitle     dd ?
  lpstrInitialDir   dd ?
  lpstrTitle        dd ?
  Flags             dd ?
  nFileOffset       dw ?
  nFileExtension    dw ?
  lpstrDefExt       dd ?
  lCustData         dd ?
  lpfnHook          dd ?
  lpTemplateName    dd ?
ends

struct CHOOSECOLOR
  lStructSize    dd ?
  hwndOwner      dd ?
  hInstance      dd ?
  rgbResult      dd ?
  lpCustColors   dd ?
  Flags          dd ?
  lCustData      dd ?
  lpfnHook       dd ?
  lpTemplateName dd ?
ends

struct FINDREPLACE
  lStructSize      dd ?
  hwndOwner        dd ?
  hInstance        dd ?
  Flags            dd ?
  lpstrFindWhat    dd ?
  lpstrReplaceWith dd ?
  wFindWhatLen     dw ?
  wReplaceWithLen  dw ?
  lCustData        dd ?
  lpfnHook         dd ?
  lpTemplateName   dd ?
ends

struct CHOOSEFONT
  lStructSize    dd ?
  hwndOwner      dd ?
  hDC            dd ?
  lpLogFont      dd ?
  iPointSize     dd ?
  Flags          dd ?
  rgbColors      dd ?
  lCustData      dd ?
  lpfnHook       dd ?
  lpTemplateName dd ?
  hInstance      dd ?
  lpszStyle      dd ?
  nFontType      dw ?
  wReserved      dw ?
  nSizeMin       dd ?
  nSizeMax       dd ?
ends

struct PRINTDLG
  lStructSize         dd ?
  hwndOwner           dd ?
  hDevMode            dd ?
  hDevNames           dd ?
  hDC                 dd ?
  Flags               dd ?
  nFromPage           dw ?
  nToPage             dw ?
  nMinPage            dw ?
  nMaxPage            dw ?
  nCopies             dw ?
  hInstance           dd ?
  lCustData           dd ?
  lpfnPrintHook       dd ?
  lpfnSetupHook       dd ?
  lpPrintTemplateName dd ?
  lpSetupTemplateName dd ?
  hPrintTemplate      dd ?
  hSetupTemplate      dd ?
ends

struct DEVNAMES
  wDriverOffset dw ?
  wDeviceOffset dw ?
  wOutputOffset dw ?
  wDefault      dw ?
ends

struct PAGESETUPDLG
  lStructSize             dd ?
  hwndOwner               dd ?
  hDevMode                dd ?
  hDevNames               dd ?
  Flags                   dd ?
  ptPaperSize             POINT
  rtMinMargin             RECT
  rtMargin                RECT
  hInstance               dd ?
  lCustData               dd ?
  lpfnPageSetupHook       dd ?
  lpfnPagePaintHook       dd ?
  lpPageSetupTemplateName dd ?
  hPageSetupTemplate      dd ?
ends

; OPENFILENAME flags

OFN_READONLY             = 000001h
OFN_OVERWRITEPROMPT      = 000002h
OFN_HIDEREADONLY         = 000004h
OFN_NOCHANGEDIR          = 000008h
OFN_SHOWHELP             = 000010h
OFN_ENABLEHOOK           = 000020h
OFN_ENABLETEMPLATE       = 000040h
OFN_ENABLETEMPLATEHANDLE = 000080h
OFN_NOVALIDATE           = 000100h
OFN_ALLOWMULTISELECT     = 000200h
OFN_EXTENSIONDIFFERENT   = 000400h
OFN_PATHMUSTEXIST        = 000800h
OFN_FILEMUSTEXIST        = 001000h
OFN_CREATEPROMPT         = 002000h
OFN_SHAREAWARE           = 004000h
OFN_NOREADONLYRETURN     = 008000h
OFN_NOTESTFILECREATE     = 010000h
OFN_NONETWORKBUTTON      = 020000h
OFN_NOLONGNAMES          = 040000h
OFN_EXPLORER             = 080000h
OFN_NODEREFERENCELINKS   = 100000h
OFN_LONGNAMES            = 200000h

; Common dialog notifications

CDN_FIRST          = -601
CDN_LAST           = -699
CDN_INITDONE       = CDN_FIRST - 0
CDN_SELCHANGE      = CDN_FIRST - 1
CDN_FOLDERCHANGE   = CDN_FIRST - 2
CDN_SHAREVIOLATION = CDN_FIRST - 3
CDN_HELP           = CDN_FIRST - 4
CDN_FILEOK         = CDN_FIRST - 5
CDN_TYPECHANGE     = CDN_FIRST - 6

; Common dialog messages

CDM_FIRST           = WM_USER + 100
CDM_LAST            = WM_USER + 200
CDM_GETSPEC         = CDM_FIRST + 0
CDM_GETFILEPATH     = CDM_FIRST + 1
CDM_GETFOLDERPATH   = CDM_FIRST + 2
CDM_GETFOLDERIDLIST = CDM_FIRST + 3
CDM_SETCONTROLTEXT  = CDM_FIRST + 4
CDM_HIDECONTROL     = CDM_FIRST + 5
CDM_SETDEFEXT       = CDM_FIRST + 6

; CHOOSECOLOR flags

CC_RGBINIT              = 001h
CC_FULLOPEN             = 002h
CC_PREVENTFULLOPEN      = 004h
CC_SHOWHELP             = 008h
CC_ENABLEHOOK           = 010h
CC_ENABLETEMPLATE       = 020h
CC_ENABLETEMPLATEHANDLE = 040h
CC_SOLIDCOLOR           = 080h
CC_ANYCOLOR             = 100h

; FINDREPLACE flags

FR_DOWN                 = 00001h
FR_WHOLEWORD            = 00002h
FR_MATCHCASE            = 00004h
FR_FINDNEXT             = 00008h
FR_REPLACE              = 00010h
FR_REPLACEALL           = 00020h
FR_DIALOGTERM           = 00040h
FR_SHOWHELP             = 00080h
FR_ENABLEHOOK           = 00100h
FR_ENABLETEMPLATE       = 00200h
FR_NOUPDOWN             = 00400h
FR_NOMATCHCASE          = 00800h
FR_NOWHOLEWORD          = 01000h
FR_ENABLETEMPLATEHANDLE = 02000h
FR_HIDEUPDOWN           = 04000h
FR_HIDEMATCHCASE        = 08000h
FR_HIDEWHOLEWORD        = 10000h

; CHOOSEFONT flags

CF_SCREENFONTS          = 0000001h
CF_PRINTERFONTS         = 0000002h
CF_BOTH                 = CF_SCREENFONTS or CF_PRINTERFONTS
CF_SHOWHELP             = 0000004h
CF_ENABLEHOOK           = 0000008h
CF_ENABLETEMPLATE       = 0000010h
CF_ENABLETEMPLATEHANDLE = 0000020h
CF_INITTOLOGFONTSTRUCT  = 0000040h
CF_USESTYLE             = 0000080h
CF_EFFECTS              = 0000100h
CF_APPLY                = 0000200h
CF_ANSIONLY             = 0000400h
CF_SCRIPTSONLY          = CF_ANSIONLY
CF_NOVECTORFONTS        = 0000800h
CF_NOOEMFONTS           = CF_NOVECTORFONTS
CF_NOSIMULATIONS        = 0001000h
CF_LIMITSIZE            = 0002000h
CF_FIXEDPITCHONLY       = 0004000h
CF_WYSIWYG              = 0008000h
CF_FORCEFONTEXIST       = 0010000h
CF_SCALABLEONLY         = 0020000h
CF_TTONLY               = 0040000h
CF_NOFACESEL            = 0080000h
CF_NOSTYLESEL           = 0100000h
CF_NOSIZESEL            = 0200000h
CF_SELECTSCRIPT         = 0400000h
CF_NOSCRIPTSEL          = 0800000h
CF_NOVERTFONTS          = 1000000h

; ChooseFont messages

WM_CHOOSEFONT_GETLOGFONT = WM_USER + 1
WM_CHOOSEFONT_SETLOGFONT = WM_USER + 101
WM_CHOOSEFONT_SETFLAGS   = WM_USER + 102

; PRINTDLG flags

PD_ALLPAGES                   = 000000h
PD_SELECTION                  = 000001h
PD_PAGENUMS                   = 000002h
PD_NOSELECTION                = 000004h
PD_NOPAGENUMS                 = 000008h
PD_COLLATE                    = 000010h
PD_PRINTTOFILE                = 000020h
PD_PRINTSETUP                 = 000040h
PD_NOWARNING                  = 000080h
PD_RETURNDC                   = 000100h
PD_RETURNIC                   = 000200h
PD_RETURNDEFAULT              = 000400h
PD_SHOWHELP                   = 000800h
PD_ENABLEPRINTHOOK            = 001000h
PD_ENABLESETUPHOOK            = 002000h
PD_ENABLEPRINTTEMPLATE        = 004000h
PD_ENABLESETUPTEMPLATE        = 008000h
PD_ENABLEPRINTTEMPLATEHANDLE  = 010000h
PD_ENABLESETUPTEMPLATEHANDLE  = 020000h
PD_USEDEVMODECOPIES           = 040000h
PD_USEDEVMODECOPIESANDCOLLATE = 040000h
PD_DISABLEPRINTTOFILE         = 080000h
PD_HIDEPRINTTOFILE            = 100000h
PD_NONETWORKBUTTON            = 200000h

; PAGESETUPDLG flags

PSD_DEFAULTMINMARGINS             = 000000h
PSD_INWININIINTLMEASURE           = 000000h
PSD_MINMARGINS                    = 000001h
PSD_MARGINS                       = 000002h
PSD_INTHOUSANDTHSOFINCHES         = 000004h
PSD_INHUNDREDTHSOFMILLIMETERS     = 000008h
PSD_DISABLEMARGINS                = 000010h
PSD_DISABLEPRINTER                = 000020h
PSD_NOWARNING                     = 000080h
PSD_DISABLEORIENTATION            = 000100h
PSD_RETURNDEFAULT                 = 000400h
PSD_DISABLEPAPER                  = 000200h
PSD_SHOWHELP                      = 000800h
PSD_ENABLEPAGESETUPHOOK           = 002000h
PSD_ENABLEPAGESETUPTEMPLATE       = 008000h
PSD_ENABLEPAGESETUPTEMPLATEHANDLE = 020000h
PSD_ENABLEPAGEPAINTHOOK           = 040000h
PSD_DISABLEPAGEPAINTING           = 080000h
PSD_NONETWORKBUTTON               = 200000h

; PageSetupDlg messages

WM_PSD_PAGESETUPDLG   = WM_USER
WM_PSD_FULLPAGERECT   = WM_USER + 1
WM_PSD_MINMARGINRECT  = WM_USER + 2
WM_PSD_MARGINRECT     = WM_USER + 3
WM_PSD_GREEKTEXTRECT  = WM_USER + 4
WM_PSD_ENVSTAMPRECT   = WM_USER + 5
WM_PSD_YAFULLPAGERECT = WM_USER + 6

; Common dialog error codes

CDERR_DIALOGFAILURE    = 0FFFFh
CDERR_GENERALCODES     = 00000h
CDERR_STRUCTSIZE       = 00001h
CDERR_INITIALIZATION   = 00002h
CDERR_NOTEMPLATE       = 00003h
CDERR_NOHINSTANCE      = 00004h
CDERR_LOADSTRFAILURE   = 00005h
CDERR_FINDRESFAILURE   = 00006h
CDERR_LOADRESFAILURE   = 00007h
CDERR_LOCKRESFAILURE   = 00008h
CDERR_MEMALLOCFAILURE  = 00009h
CDERR_MEMLOCKFAILURE   = 0000Ah
CDERR_NOHOOK           = 0000Bh
CDERR_REGISTERMSGFAIL  = 0000Ch
PDERR_PRINTERCODES     = 01000h
PDERR_SETUPFAILURE     = 01001h
PDERR_PARSEFAILURE     = 01002h
PDERR_RETDEFFAILURE    = 01003h
PDERR_LOADDRVFAILURE   = 01004h
PDERR_GETDEVMODEFAIL   = 01005h
PDERR_INITFAILURE      = 01006h
PDERR_NODEVICES        = 01007h
PDERR_NODEFAULTPRN     = 01008h
PDERR_DNDMMISMATCH     = 01009h
PDERR_CREATEICFAILURE  = 0100Ah
PDERR_PRINTERNOTFOUND  = 0100Bh
PDERR_DEFAULTDIFFERENT = 0100Ch
CFERR_CHOOSEFONTCODES  = 02000h
CFERR_NOFONTS          = 02001h
CFERR_MAXLESSTHANMIN   = 02002h
FNERR_FILENAMECODES    = 03000h
FNERR_SUBCLASSFAILURE  = 03001h
FNERR_INVALIDFILENAME  = 03002h
FNERR_BUFFERTOOSMALL   = 03003h
FRERR_FINDREPLACECODES = 04000h
FRERR_BUFFERLENGTHZERO = 04001h
CCERR_CHOOSECOLORCODES = 05000h


; SHELL32.DLL structures and constants

struct NOTIFYICONDATA
  cbSize	   dd ?
  hWnd		   dd ?
  uID		   dd ?
  uFlags	   dd ?
  uCallbackMessage dd ?
  hIcon 	   dd ?
  szTip 	   TCHAR 64 dup (?)
ends

struct NOTIFYICONDATAA
  cbSize	   dd ?
  hWnd		   dd ?
  uID		   dd ?
  uFlags	   dd ?
  uCallbackMessage dd ?
  hIcon 	   dd ?
  szTip 	   db 64 dup (?)
ends

struct NOTIFYICONDATAW
  cbSize	   dd ?
  hWnd		   dd ?
  uID		   dd ?
  uFlags	   dd ?
  uCallbackMessage dd ?
  hIcon 	   dd ?
  szTip 	   du 64 dup (?)
ends

struct BROWSEINFO
  hwndOwner	 dd ?
  pidlRoot	 dd ?
  pszDisplayName dd ?
  lpszTitle	 dd ?
  ulFlags	 dd ?
  lpfn		 dd ?
  lParam	 dd ?
  iImage	 dd ?
ends

; Taskbar icon messages

NIM_ADD        = 0
NIM_MODIFY     = 1
NIM_DELETE     = 2
NIM_SETFOCUS   = 3
NIM_SETVERSION = 4

; Taskbar icon flags

NIF_MESSAGE    = 01h
NIF_ICON       = 02h
NIF_TIP        = 04h
NIF_STATE      = 08h
NIF_INFO       = 10h
NIF_GUID       = 20h

; Constant Special Item ID List

CSIDL_DESKTOP		      = 0x0000
CSIDL_INTERNET		      = 0x0001
CSIDL_PROGRAMS		      = 0x0002
CSIDL_CONTROLS		      = 0x0003
CSIDL_PRINTERS		      = 0x0004
CSIDL_PERSONAL		      = 0x0005
CSIDL_FAVORITES 	      = 0x0006
CSIDL_STARTUP		      = 0x0007
CSIDL_RECENT		      = 0x0008
CSIDL_SENDTO		      = 0x0009
CSIDL_BITBUCKET 	      = 0x000A
CSIDL_STARTMENU 	      = 0x000B
CSIDL_MYDOCUMENTS	      = 0x000C
CSIDL_MYMUSIC		      = 0x000D
CSIDL_MYVIDEO		      = 0x000E
CSIDL_DESKTOPDIRECTORY	      = 0x0010
CSIDL_DRIVES		      = 0x0011
CSIDL_NETWORK		      = 0x0012
CSIDL_NETHOOD		      = 0x0013
CSIDL_FONTS		      = 0x0014
CSIDL_TEMPLATES 	      = 0x0015
CSIDL_COMMON_STARTMENU	      = 0x0016
CSIDL_COMMON_PROGRAMS	      = 0x0017
CSIDL_COMMON_STARTUP	      = 0x0018
CSIDL_COMMON_DESKTOPDIRECTORY = 0x0019
CSIDL_APPDATA		      = 0x001A
CSIDL_PRINTHOOD 	      = 0x001B
CSIDL_LOCAL_APPDATA	      = 0x001C
CSIDL_ALTSTARTUP	      = 0x001D
CSIDL_COMMON_ALTSTARTUP       = 0x001E
CSIDL_COMMON_FAVORITES	      = 0x001F
CSIDL_INTERNET_CACHE	      = 0x0020
CSIDL_COOKIES		      = 0x0021
CSIDL_HISTORY		      = 0x0022
CSIDL_COMMON_APPDATA	      = 0x0023
CSIDL_WINDOWS		      = 0x0024
CSIDL_SYSTEM		      = 0x0025
CSIDL_PROGRAM_FILES	      = 0x0026
CSIDL_MYPICTURES	      = 0x0027
CSIDL_PROFILE		      = 0x0028
CSIDL_SYSTEMX86 	      = 0x0029
CSIDL_PROGRAM_FILESX86	      = 0x002A
CSIDL_PROGRAM_FILES_COMMON    = 0x002B
CSIDL_PROGRAM_FILES_COMMONX86 = 0x002C
CSIDL_COMMON_TEMPLATES	      = 0x002D
CSIDL_COMMON_DOCUMENTS	      = 0x002E
CSIDL_COMMON_ADMINTOOLS       = 0x002F
CSIDL_ADMINTOOLS	      = 0x0030
CSIDL_CONNECTIONS	      = 0x0031
CSIDL_COMMON_MUSIC	      = 0x0035
CSIDL_COMMON_PICTURES	      = 0x0036
CSIDL_COMMON_VIDEO	      = 0x0037
CSIDL_RESOURCES 	      = 0x0038
CSIDL_RESOURCES_LOCALIZED     = 0x0039
CSIDL_COMMON_OEM_LINKS	      = 0x003A
CSIDL_CDBURN_AREA	      = 0x003B
CSIDL_COMPUTERSNEARME	      = 0x003D
CSIDL_PROFILES		      = 0x003E
CSIDL_FOLDER_MASK	      = 0x00FF
CSIDL_FLAG_PER_USER_INIT      = 0x0800
CSIDL_FLAG_NO_ALIAS	      = 0x1000
CSIDL_FLAG_DONT_VERIFY	      = 0x4000
CSIDL_FLAG_CREATE	      = 0x8000
CSIDL_FLAG_MASK 	      = 0xFF00
				      

; WSOCK32.DLL structures and constants

struct WSADATA
  wVersion	 dw ?
  wHighVersion	 dw ?
  szDescription  db 256+1 dup (?)
  szSystemStatus db 128+1 dup (?)
  iMaxSockets	 dw ?
  iMaxUdpDg	 dw ?
  _padding_	 db 2 dup (?)
  lpVendorInfo	 dd ?
ends

struct hostent
  h_name      dd ?
  h_aliases   dd ?
  h_addrtype  dw ?
  h_length    dw ?
  h_addr_list dd ?
ends

struct sockaddr_in
  sin_family dw ?
  sin_port   dw ?
  sin_addr   dd ?
  sin_zero   db 8 dup (?)
ends

struct sockaddr
  sa_family dw ?
  sa_data   db 14 dup (?)
ends

; Socket types

SOCK_STREAM    = 1
SOCK_DGRAM     = 2
SOCK_RAW       = 3
SOCK_RDM       = 4
SOCK_SEQPACKET = 5

; Address formats

AF_UNSPEC    = 0
AF_UNIX      = 1
AF_INET      = 2
AF_IMPLINK   = 3
AF_PUP	     = 4
AF_CHAOS     = 5
AF_NS	     = 6
AF_IPX	     = 6
AF_ISO	     = 7
AF_OSI	     = AF_ISO
AF_ECMA      = 8
AF_DATAKIT   = 9
AF_CCITT     = 10
AF_SNA	     = 11
AF_DECnet    = 12
AF_DLI	     = 13
AF_LAT	     = 14
AF_HYLINK    = 15
AF_APPLETALK = 16
AF_NETBIOS   = 17

; Protocol formats

PF_UNSPEC    = 0
PF_UNIX      = 1
PF_INET      = 2
PF_IMPLINK   = 3
PF_PUP	     = 4
PF_CHAOS     = 5
PF_NS	     = 6
PF_IPX	     = 6
PF_ISO	     = 7
PF_OSI	     = PF_ISO
PF_ECMA      = 8
PF_DATAKIT   = 9
PF_CCITT     = 10
PF_SNA	     = 11
PF_DECnet    = 12
PF_DLI	     = 13
PF_LAT	     = 14
PF_HYLINK    = 15
PF_APPLETALK = 16
PF_NETBIOS   = 17

; IP Ports

IPPORT_ECHO	   = 7
IPPORT_DISCARD	   = 9
IPPORT_SYSTAT	   = 11
IPPORT_DAYTIME	   = 13
IPPORT_NETSTAT	   = 15
IPPORT_FTP	   = 21
IPPORT_TELNET	   = 23
IPPORT_SMTP	   = 25
IPPORT_TIMESERVER  = 37
IPPORT_NAMESERVER  = 42
IPPORT_WHOIS	   = 43
IPPORT_MTP	   = 57
IPPORT_TFTP	   = 69
IPPORT_RJE	   = 77
IPPORT_FINGER	   = 79
IPPORT_TTYLINK	   = 87
IPPORT_SUPDUP	   = 95
IPPORT_EXECSERVER  = 512
IPPORT_LOGINSERVER = 513
IPPORT_CMDSERVER   = 514
IPPORT_EFSSERVER   = 520
IPPORT_BIFFUDP	   = 512
IPPORT_WHOSERVER   = 513
IPPORT_ROUTESERVER = 520
IPPORT_RESERVED    = 1024

; Notifications

FD_READ    = 01h
FD_WRITE   = 02h
FD_OOB	   = 04h
FD_ACCEPT  = 08h
FD_CONNECT = 10h
FD_CLOSE   = 20h


macro api [name] { if used name
                    label name dword at name#A
                   end if }



; Macroinstructions for HLL-style conditional operations

macro .if [arg]
{
  common
  __IF equ
  local ..endif
  __ENDIF equ ..endif
  local ..else
  __ELSE equ ..else
  JNCOND __ELSE,arg
}

macro .else
{
  jmp __ENDIF
  __ELSE:
  restore __IF
  __IF equ ,
}

macro .elseif [arg]
{
  common
  jmp __ENDIF
  __ELSE:
  restore __ELSE
  local ..else
  __ELSE equ ..else
  JNCOND __ELSE,arg
}

macro .endif
{
  if __IF eq
   __ELSE:
  end if
  __ENDIF:
  restore __ELSE
  restore __ENDIF
  restore __IF
}

macro .while [arg]
{
  common
  local ..while
  __WHILE equ ..while
  local ..endw
  __ENDW equ ..endw
  __WHILE:
  JNCOND __ENDW,arg
}

macro .endw
{
  jmp __WHILE
  __ENDW:
  restore __ENDW
  restore __WHILE
}

macro .repeat
{
  local ..repeat
  __REPEAT equ ..repeat
  __REPEAT:
}

macro .until [arg]
{
  common
  JNCOND __REPEAT,arg
  restore __REPEAT
}

jnne equ je
jnna equ ja
jnnb equ jb
jnng equ jg
jnnl equ jl
jnnae equ jae
jnnbe equ jbe
jnnge equ jge
jnnle equ jle

macro JNCOND label,v1,c,v2
{
 match any,c
 \{
   cmp v1,v2
   jn\#c label
 \}
 match ,c
 \{
   PARSECOND parsed@cond,v1
   match cond,parsed@cond \\{ JNCONDEXPR label,cond \\}
 \}
}

gt equ >
lt equ <

macro PARSECOND parsed,cond
{
 define parsed
 define neg@cond
 define status@cond
 define nest@cond
 irps symb,cond
 \{
   define symb@cond symb
   match >,symb
   \\{
      define symb@cond gt
   \\}
   match <,symb
   \\{
      define symb@cond lt
   \\}
   current@cond equ status@cond
   match ,current@cond
   \\{
      match ~,symb
      \\\{
	  neg@cond equ neg@cond ~
	  match ~~,neg@cond
	  \\\\{
	       define neg@cond
	  \\\\}
	  define symb@cond
      \\\}
      match (,symb
      \\\{
	  parsed equ parsed neg@cond,<
	  define nest@cond +
	  define symb@cond
      \\\}
      match any,symb@cond
      \\\{
	  parsed equ parsed neg@cond,symb@cond
	  define status@cond +
      \\\}
   \\}
   match status,current@cond
   \\{
      match &,symb
      \\\{
	  parsed equ parsed,&,
	  define status@cond
	  define symb@cond
	  define neg@cond
      \\\}
      match |,symb
      \\\{
	  parsed equ parsed,|,
	  define status@cond
	  define symb@cond
	  define neg@cond
      \\\}
      match (,symb
      \\\{
	  define nest@cond (
      \\\}
      match ),symb
      \\\{
	  match +,nest@cond
	  \\\\{
	       parsed equ parsed>
	       define symb@cond
	  \\\\}
	  restore nest@cond
      \\\}
      match any,symb@cond
      \\\{
	  parsed equ parsed symb@cond
      \\\}
   \\}
 \}
}

macro define_JNCONDEXPR
{
 macro JNCONDEXPR elabel,[mod,cond,op]
 \{
  \common
   \local ..t,..f
   define t@cond ..t
   define f@cond ..f
  \forward
   match ,op
   \\{
      match ,mod \\\{ JNCONDEL elabel,<cond> \\\}
      match ~,mod \\\{ JCONDEL elabel,<cond> \\\}
   \\}
   match &:flabel:tlabel, op:f@cond:t@cond
   \\{
      match ,mod \\\{ JNCONDEL flabel,<cond> \\\}
      match ~,mod \\\{ JCONDEL flabel,<cond> \\\}
      tlabel:
      \\local ..tnew
      restore t@cond
      define t@cond ..tnew
   \\}
   match |:flabel:tlabel, op:f@cond:t@cond
   \\{
      match ,mod \\\{ JCONDEL tlabel,<cond> \\\}
      match ~,mod \\\{ JNCONDEL tlabel,<cond> \\\}
      flabel:
      \\local ..fnew
      restore f@cond
      define f@cond ..fnew
   \\}
  \common
   label f@cond at elabel
   t@cond:
   restore t@cond
   restore f@cond
 \}
}

macro define_JCONDEXPR
{
 macro JCONDEXPR elabel,[mod,cond,op]
 \{
  \common
   \local ..t,..f
   define t@cond ..t
   define f@cond ..f
  \forward
   match ,op
   \\{
      match ,mod \\\{ JCONDEL elabel,<cond> \\\}
      match ~,mod \\\{ JNCONDEL elabel,<cond> \\\}
   \\}
   match |:flabel:tlabel, op:f@cond:t@cond
   \\{
      match ,mod \\\{ JCONDEL flabel,<cond> \\\}
      match ~,mod \\\{ JNCONDEL flabel,<cond> \\\}
      tlabel:
      \\local ..tnew
      restore t@cond
      define t@cond ..tnew
   \\}
   match &:flabel:tlabel, op:f@cond:t@cond
   \\{
      match ,mod \\\{ JNCONDEL tlabel,<cond> \\\}
      match ~,mod \\\{ JCONDEL tlabel,<cond> \\\}
      flabel:
      \\local ..fnew
      restore f@cond
      define f@cond ..fnew
   \\}
  \common
   label f@cond at elabel
   t@cond:
   restore t@cond
   restore f@cond
 \}
}

macro define_JNCONDEL
{
 macro JNCONDEL label,cond
 \{
   \local COND
   match car=,cdr,:cond
   \\{
      define_JNCONDEXPR
      define_JCONDEXPR
      define_JCONDEL
      define_JNCONDEL
      JNCONDEXPR label,cond
      purge JNCONDEXPR,JCONDEXPR,JCONDEL,JNCONDEL
      define COND
   \\}
   match c,cond ; replace gt and lt
   \\{
      match =COND =signed v1>==v2, COND c
      \\\{
	  cmp v1,v2
	  jl label
	  define COND
      \\\}
      match =COND =signed v1<==v2, COND c
      \\\{
	  cmp v1,v2
	  jg label
	  define COND
      \\\}
      match =COND v1>==v2, COND c
      \\\{
	  cmp v1,v2
	  jb label
	  define COND
      \\\}
      match =COND v1<==v2, COND c
      \\\{
	  cmp v1,v2
	  ja label
	  define COND
      \\\}
      match =COND v1==v2, COND c
      \\\{
	  cmp v1,v2
	  jne label
	  define COND
      \\\}
      match =COND v1<>v2, COND c
      \\\{
	  cmp v1,v2
	  je label
	  define COND
      \\\}
      match =COND =signed v1>v2, COND c
      \\\{
	  cmp v1,v2
	  jle label
	  define COND
      \\\}
      match =COND =signed v1<v2, COND c
      \\\{
	  cmp v1,v2
	  jge label
	  define COND
      \\\}
      match =COND v1>v2, COND c
      \\\{
	  cmp v1,v2
	  jbe label
	  define COND
      \\\}
      match =COND v1<v2, COND c
      \\\{
	  cmp v1,v2
	  jae label
	  define COND
      \\\}
      match =COND =ZERO?, COND c
      \\\{
	  jnz label
	  define COND
      \\\}
      match =COND =CARRY?, COND c
      \\\{
	  jnc label
	  define COND
      \\\}
      match =COND =OVERFLOW?, COND c
      \\\{
	  jno label
	  define COND
      \\\}
      match =COND =SIGN?, COND c
      \\\{
	  jns label
	  define COND
      \\\}
      match =COND =PARITY?, COND c
      \\\{
	  jnp label
	  define COND
      \\\}
      match =COND v, COND c
      \\\{
	  if v eqtype 0
	   if ~ v
	    jmp label
	   end if
	  else if v eqtype eax
	   test v,v
	   jz label
	  else
	   cmp v,0
	   je label
	  end if
      \\\}
   \\}
 \}
}

macro define_JCONDEL
{
 macro JCONDEL label,cond
 \{
   \local COND
   match car=,cdr,:cond
   \\{
      define_JNCONDEXPR
      define_JCONDEXPR
      define_JCONDEL
      define_JNCONDEL
      JCONDEXPR label,cond
      purge JNCONDEXPR,JCONDEXPR,JCONDEL,JNCONDEL
      define COND
   \\}
   match c,cond ; replace gt and lt
   \\{
      match =COND =signed v1>==v2, COND c
      \\\{
	  cmp v1,v2
	  jge label
	  define COND
      \\\}
      match =COND =signed v1<==v2, COND c
      \\\{
	  cmp v1,v2
	  jle label
	  define COND
      \\\}
      match =COND v1>==v2, COND c
      \\\{
	  cmp v1,v2
	  jae label
	  define COND
      \\\}
      match =COND v1<==v2, COND c
      \\\{
	  cmp v1,v2
	  jbe label
	  define COND
      \\\}
      match =COND v1==v2, COND c
      \\\{
	  cmp v1,v2
	  je label
	  define COND
      \\\}
      match =COND v1<>v2, COND c
      \\\{
	  cmp v1,v2
	  jne label
	  define COND
      \\\}
      match =COND =signed v1>v2, COND c
      \\\{
	  cmp v1,v2
	  jg label
	  define COND
      \\\}
      match =COND =signed v1<v2, COND c
      \\\{
	  cmp v1,v2
	  jl label
	  define COND
      \\\}
      match =COND v1>v2, COND c
      \\\{
	  cmp v1,v2
	  ja label
	  define COND
      \\\}
      match =COND v1<v2, COND c
      \\\{
	  cmp v1,v2
	  jb label
	  define COND
      \\\}
      match =COND =ZERO?, COND c
      \\\{
	  jz label
	  define COND
      \\\}
      match =COND =CARRY?, COND c
      \\\{
	  jc label
	  define COND
      \\\}
      match =COND =OVERFLOW?, COND c
      \\\{
	  jo label
	  define COND
      \\\}
      match =COND =SIGN?, COND c
      \\\{
	  js label
	  define COND
      \\\}
      match =COND =PARITY?, COND c
      \\\{
	  jp label
	  define COND
      \\\}
      match =COND v, COND c
      \\\{
	  if v eqtype 0
	   if v
	    jmp label
	   end if
	  else if v eqtype eax
	   test v,v
	   jnz label
	  else
	   cmp v,0
	   jne label
	  end if
      \\\}
   \\}
 \}
}

define_JNCONDEXPR
define_JCONDEXPR
define_JNCONDEL
define_JCONDEL


macro allow_nesting
{ macro pushd value
  \{ match ,value \\{
      pushx equ \\}
     match =pushx =invoke proc,pushx value \\{
      allow_nesting
      invoke proc
      purge pushd,invoke,stdcall,cinvoke,ccall
      push eax
      pushx equ \\}
     match =pushx =stdcall proc,pushx value \\{
      allow_nesting
      stdcall proc
      purge pushd,invoke,stdcall,cinvoke,ccall
      push eax
      pushx equ \\}
     match =pushx =cinvoke proc,pushx value \\{
      allow_nesting
      cinvoke proc
      purge pushd,invoke,stdcall,cinvoke,ccall
      push eax
      pushx equ \\}
     match =pushx =ccall proc,pushx value \\{
      allow_nesting
      ccall proc
      purge pushd,invoke,stdcall,cinvoke,ccall
      push eax
      pushx equ \\}
     match =pushx,pushx \\{
      pushd <value>
      pushx equ \\}
     restore pushx \}
  macro invoke proc,[arg]
  \{ \reverse pushd <arg>
     \common call [proc] \}
  macro stdcall proc,[arg]
  \{ \reverse pushd <arg>
     \common call proc \}
  macro cinvoke proc,[arg]
  \{ \common \local size
	     size = 0
	     if ~ arg eq
     \reverse pushd <arg>
	      size = size+4
	      match =double any,arg \\{ size = size+4 \\}
     \common end if
	     call [proc]
	     if size
	     add esp,size
	     end if \}
  macro ccall proc,[arg]
  \{ \common \local size
	     size = 0
	     if ~ arg eq
     \reverse pushd <arg>
	      size = size+4
	      match =double any,arg \\{ size = size+4 \\}
     \common end if
	     call proc
	     if size
	     add esp,size
	     end if \} }

macro pushd value
{ match first=,more, value \{ \local ..continue
   call ..continue
   db value,0
   ..continue:
   pushd equ \}
  match pushd =addr var,pushd value \{ \local ..opcode,..address
   virtual at 0
    label ..address at var
    mov eax,dword [..address]
    load ..opcode from 0
   end virtual
   if ..opcode = 0A1h
    push var
   else
    lea edx,[..address]
    push edx
   end if
   pushd equ \}
  match pushd =double [var],pushd value \{
   push dword [var+4]
   push dword [var]
   pushd equ \}
  match pushd =double =ptr var,pushd value \{
   push dword [var+4]
   push dword [var]
   pushd equ \}
  match pushd =double num,pushd value \{ \local ..high,..low
   virtual at 0
    dq num
    load ..low dword from 0
    load ..high dword from 4
   end virtual
   push ..high
   push ..low
   pushd equ \}
  match pushd,pushd \{ \local ..continue
   if value eqtype ''
    call ..continue
    db value,0
    ..continue:
   else
    push value
   end if
   pushd equ \}
  restore pushd }

allow_nesting

macro import lib,[functions]
{ common macro import_#lib \{ import lib,functions \} }

macro api [functions]
{ common macro all_api \{ all_api
			  api functions \} }
macro all_api {}


; KERNEL32 API calls

import kernel32,\
       AddAtomA,'AddAtomA',\
       AddAtomW,'AddAtomW',\
       AddConsoleAliasA,'AddConsoleAliasA',\
       AddConsoleAliasW,'AddConsoleAliasW',\
       AllocConsole,'AllocConsole',\
       AreFileApisANSI,'AreFileApisANSI',\
       AssignProcessToJobObject,'AssignProcessToJobObject',\
       BackupRead,'BackupRead',\
       BackupSeek,'BackupSeek',\
       BackupWrite,'BackupWrite',\
       BaseAttachCompleteThunk,'BaseAttachCompleteThunk',\
       Beep,'Beep',\
       BeginUpdateResourceA,'BeginUpdateResourceA',\
       BeginUpdateResourceW,'BeginUpdateResourceW',\
       BuildCommDCBA,'BuildCommDCBA',\
       BuildCommDCBW,'BuildCommDCBW',\
       BuildCommDCBAndTimeoutsA,'BuildCommDCBAndTimeoutsA',\
       BuildCommDCBAndTimeoutsW,'BuildCommDCBAndTimeoutsW',\
       CallNamedPipeA,'CallNamedPipeA',\
       CallNamedPipeW,'CallNamedPipeW',\
       CancelIo,'CancelIo',\
       CancelWaitableTimer,'CancelWaitableTimer',\
       ClearCommBreak,'ClearCommBreak',\
       ClearCommError,'ClearCommError',\
       CloseConsoleHandle,'CloseConsoleHandle',\
       CloseHandle,'CloseHandle',\
       CloseProfileUserMapping,'CloseProfileUserMapping',\
       CmdBatNotification,'CmdBatNotification',\
       CommConfigDialogA,'CommConfigDialogA',\
       CommConfigDialogW,'CommConfigDialogW',\
       CompareFileTime,'CompareFileTime',\
       CompareStringA,'CompareStringA',\
       CompareStringW,'CompareStringW',\
       ConnectNamedPipe,'ConnectNamedPipe',\
       ConsoleMenuControl,'ConsoleMenuControl',\
       ContinueDebugEvent,'ContinueDebugEvent',\
       ConvertDefaultLocale,'ConvertDefaultLocale',\
       ConvertThreadToFiber,'ConvertThreadToFiber',\
       CopyFileA,'CopyFileA',\
       CopyFileW,'CopyFileW',\
       CopyFileExA,'CopyFileExA',\
       CopyFileExW,'CopyFileExW',\
       CreateConsoleScreenBuffer,'CreateConsoleScreenBuffer',\
       CreateDirectoryA,'CreateDirectoryA',\
       CreateDirectoryW,'CreateDirectoryW',\
       CreateDirectoryExA,'CreateDirectoryExA',\
       CreateDirectoryExW,'CreateDirectoryExW',\
       CreateEventA,'CreateEventA',\
       CreateEventW,'CreateEventW',\
       CreateFiber,'CreateFiber',\
       CreateFileA,'CreateFileA',\
       CreateFileW,'CreateFileW',\
       CreateFileMappingA,'CreateFileMappingA',\
       CreateFileMappingW,'CreateFileMappingW',\
       CreateHardLinkA,'CreateHardLinkA',\
       CreateHardLinkW,'CreateHardLinkW',\
       CreateIoCompletionPort,'CreateIoCompletionPort',\
       CreateJobObjectA,'CreateJobObjectA',\
       CreateJobObjectW,'CreateJobObjectW',\
       CreateMailslotA,'CreateMailslotA',\
       CreateMailslotW,'CreateMailslotW',\
       CreateMutexA,'CreateMutexA',\
       CreateMutexW,'CreateMutexW',\
       CreateNamedPipeA,'CreateNamedPipeA',\
       CreateNamedPipeW,'CreateNamedPipeW',\
       CreatePipe,'CreatePipe',\
       CreateProcessA,'CreateProcessA',\
       CreateProcessW,'CreateProcessW',\
       CreateRemoteThread,'CreateRemoteThread',\
       CreateSemaphoreA,'CreateSemaphoreA',\
       CreateSemaphoreW,'CreateSemaphoreW',\
       CreateTapePartition,'CreateTapePartition',\
       CreateThread,'CreateThread',\
       CreateToolhelp32Snapshot,'CreateToolhelp32Snapshot',\
       CreateVirtualBuffer,'CreateVirtualBuffer',\
       CreateWaitableTimerA,'CreateWaitableTimerA',\
       CreateWaitableTimerW,'CreateWaitableTimerW',\
       DebugActiveProcess,'DebugActiveProcess',\
       DebugBreak,'DebugBreak',\
       DefineDosDeviceA,'DefineDosDeviceA',\
       DefineDosDeviceW,'DefineDosDeviceW',\
       DeleteAtom,'DeleteAtom',\
       DeleteCriticalSection,'DeleteCriticalSection',\
       DeleteFiber,'DeleteFiber',\
       DeleteFileA,'DeleteFileA',\
       DeleteFileW,'DeleteFileW',\
       DeviceIoControl,'DeviceIoControl',\
       DisableThreadLibraryCalls,'DisableThreadLibraryCalls',\
       DisconnectNamedPipe,'DisconnectNamedPipe',\
       DosDateTimeToFileTime,'DosDateTimeToFileTime',\
       DuplicateConsoleHandle,'DuplicateConsoleHandle',\
       DuplicateHandle,'DuplicateHandle',\
       EndUpdateResourceA,'EndUpdateResourceA',\
       EndUpdateResourceW,'EndUpdateResourceW',\
       EnterCriticalSection,'EnterCriticalSection',\
       EnumCalendarInfoA,'EnumCalendarInfoA',\
       EnumCalendarInfoW,'EnumCalendarInfoW',\
       EnumCalendarInfoExA,'EnumCalendarInfoExA',\
       EnumCalendarInfoExW,'EnumCalendarInfoExW',\
       EnumDateFormatsA,'EnumDateFormatsA',\
       EnumDateFormatsW,'EnumDateFormatsW',\
       EnumDateFormatsExA,'EnumDateFormatsExA',\
       EnumDateFormatsExW,'EnumDateFormatsExW',\
       EnumResourceLanguagesA,'EnumResourceLanguagesA',\
       EnumResourceLanguagesW,'EnumResourceLanguagesW',\
       EnumResourceNamesA,'EnumResourceNamesA',\
       EnumResourceNamesW,'EnumResourceNamesW',\
       EnumResourceTypesA,'EnumResourceTypesA',\
       EnumResourceTypesW,'EnumResourceTypesW',\
       EnumSystemCodePagesA,'EnumSystemCodePagesA',\
       EnumSystemCodePagesW,'EnumSystemCodePagesW',\
       EnumSystemLocalesA,'EnumSystemLocalesA',\
       EnumSystemLocalesW,'EnumSystemLocalesW',\
       EnumTimeFormatsA,'EnumTimeFormatsA',\
       EnumTimeFormatsW,'EnumTimeFormatsW',\
       EraseTape,'EraseTape',\
       EscapeCommFunction,'EscapeCommFunction',\
       ExitProcess,'ExitProcess',\
       ExitThread,'ExitThread',\
       ExitVDM,'ExitVDM',\
       ExpandEnvironmentStringsA,'ExpandEnvironmentStringsA',\
       ExpandEnvironmentStringsW,'ExpandEnvironmentStringsW',\
       ExpungeConsoleCommandHistoryA,'ExpungeConsoleCommandHistoryA',\
       ExpungeConsoleCommandHistoryW,'ExpungeConsoleCommandHistoryW',\
       ExtendVirtualBuffer,'ExtendVirtualBuffer',\
       FatalAppExitA,'FatalAppExitA',\
       FatalAppExitW,'FatalAppExitW',\
       FatalExit,'FatalExit',\
       FileTimeToDosDateTime,'FileTimeToDosDateTime',\
       FileTimeToLocalFileTime,'FileTimeToLocalFileTime',\
       FileTimeToSystemTime,'FileTimeToSystemTime',\
       FillConsoleOutputAttribute,'FillConsoleOutputAttribute',\
       FillConsoleOutputCharacterA,'FillConsoleOutputCharacterA',\
       FillConsoleOutputCharacterW,'FillConsoleOutputCharacterW',\
       FindAtomA,'FindAtomA',\
       FindAtomW,'FindAtomW',\
       FindClose,'FindClose',\
       FindCloseChangeNotification,'FindCloseChangeNotification',\
       FindFirstChangeNotificationA,'FindFirstChangeNotificationA',\
       FindFirstChangeNotificationW,'FindFirstChangeNotificationW',\
       FindFirstFileA,'FindFirstFileA',\
       FindFirstFileW,'FindFirstFileW',\
       FindFirstFileExA,'FindFirstFileExA',\
       FindFirstFileExW,'FindFirstFileExW',\
       FindNextChangeNotification,'FindNextChangeNotification',\
       FindNextFileA,'FindNextFileA',\
       FindNextFileW,'FindNextFileW',\
       FindResourceA,'FindResourceA',\
       FindResourceW,'FindResourceW',\
       FindResourceExA,'FindResourceExA',\
       FindResourceExW,'FindResourceExW',\
       FlushConsoleInputBuffer,'FlushConsoleInputBuffer',\
       FlushFileBuffers,'FlushFileBuffers',\
       FlushInstructionCache,'FlushInstructionCache',\
       FlushViewOfFile,'FlushViewOfFile',\
       FoldStringA,'FoldStringA',\
       FoldStringW,'FoldStringW',\
       FormatMessageA,'FormatMessageA',\
       FormatMessageW,'FormatMessageW',\
       FreeConsole,'FreeConsole',\
       FreeEnvironmentStringsA,'FreeEnvironmentStringsA',\
       FreeEnvironmentStringsW,'FreeEnvironmentStringsW',\
       FreeLibrary,'FreeLibrary',\
       FreeLibraryAndExitThread,'FreeLibraryAndExitThread',\
       FreeResource,'FreeResource',\
       FreeVirtualBuffer,'FreeVirtualBuffer',\
       GenerateConsoleCtrlEvent,'GenerateConsoleCtrlEvent',\
       GetACP,'GetACP',\
       GetAtomNameA,'GetAtomNameA',\
       GetAtomNameW,'GetAtomNameW',\
       GetBinaryTypeA,'GetBinaryTypeA',\
       GetBinaryTypeW,'GetBinaryTypeW',\
       GetCPInfo,'GetCPInfo',\
       GetCPInfoExA,'GetCPInfoExA',\
       GetCPInfoExW,'GetCPInfoExW',\
       GetCommConfig,'GetCommConfig',\
       GetCommMask,'GetCommMask',\
       GetCommModemStatus,'GetCommModemStatus',\
       GetCommProperties,'GetCommProperties',\
       GetCommState,'GetCommState',\
       GetCommTimeouts,'GetCommTimeouts',\
       GetCommandLineA,'GetCommandLineA',\
       GetCommandLineW,'GetCommandLineW',\
       GetCompressedFileSizeA,'GetCompressedFileSizeA',\
       GetCompressedFileSizeW,'GetCompressedFileSizeW',\
       GetComputerNameA,'GetComputerNameA',\
       GetComputerNameW,'GetComputerNameW',\
       GetConsoleAliasA,'GetConsoleAliasA',\
       GetConsoleAliasW,'GetConsoleAliasW',\
       GetConsoleAliasExesA,'GetConsoleAliasExesA',\
       GetConsoleAliasExesW,'GetConsoleAliasExesW',\
       GetConsoleAliasExesLengthA,'GetConsoleAliasExesLengthA',\
       GetConsoleAliasExesLengthW,'GetConsoleAliasExesLengthW',\
       GetConsoleAliasesA,'GetConsoleAliasesA',\
       GetConsoleAliasesW,'GetConsoleAliasesW',\
       GetConsoleAliasesLengthA,'GetConsoleAliasesLengthA',\
       GetConsoleAliasesLengthW,'GetConsoleAliasesLengthW',\
       GetConsoleCP,'GetConsoleCP',\
       GetConsoleCommandHistoryA,'GetConsoleCommandHistoryA',\
       GetConsoleCommandHistoryW,'GetConsoleCommandHistoryW',\
       GetConsoleCommandHistoryLengthA,'GetConsoleCommandHistoryLengthA',\
       GetConsoleCommandHistoryLengthW,'GetConsoleCommandHistoryLengthW',\
       GetConsoleCursorInfo,'GetConsoleCursorInfo',\
       GetConsoleDisplayMode,'GetConsoleDisplayMode',\
       GetConsoleFontInfo,'GetConsoleFontInfo',\
       GetConsoleFontSize,'GetConsoleFontSize',\
       GetConsoleHardwareState,'GetConsoleHardwareState',\
       GetConsoleInputExeNameA,'GetConsoleInputExeNameA',\
       GetConsoleInputExeNameW,'GetConsoleInputExeNameW',\
       GetConsoleInputWaitHandle,'GetConsoleInputWaitHandle',\
       GetConsoleKeyboardLayoutNameA,'GetConsoleKeyboardLayoutNameA',\
       GetConsoleKeyboardLayoutNameW,'GetConsoleKeyboardLayoutNameW',\
       GetConsoleMode,'GetConsoleMode',\
       GetConsoleOutputCP,'GetConsoleOutputCP',\
       GetConsoleScreenBufferInfo,'GetConsoleScreenBufferInfo',\
       GetConsoleTitleA,'GetConsoleTitleA',\
       GetConsoleTitleW,'GetConsoleTitleW',\
       GetConsoleWindow,'GetConsoleWindow',\
       GetCurrencyFormatA,'GetCurrencyFormatA',\
       GetCurrencyFormatW,'GetCurrencyFormatW',\
       GetCurrentConsoleFont,'GetCurrentConsoleFont',\
       GetCurrentDirectoryA,'GetCurrentDirectoryA',\
       GetCurrentDirectoryW,'GetCurrentDirectoryW',\
       GetCurrentProcess,'GetCurrentProcess',\
       GetCurrentProcessId,'GetCurrentProcessId',\
       GetCurrentThread,'GetCurrentThread',\
       GetCurrentThreadId,'GetCurrentThreadId',\
       GetDateFormatA,'GetDateFormatA',\
       GetDateFormatW,'GetDateFormatW',\
       GetDefaultCommConfigA,'GetDefaultCommConfigA',\
       GetDefaultCommConfigW,'GetDefaultCommConfigW',\
       GetDevicePowerState,'GetDevicePowerState',\
       GetDiskFreeSpaceA,'GetDiskFreeSpaceA',\
       GetDiskFreeSpaceW,'GetDiskFreeSpaceW',\
       GetDiskFreeSpaceExA,'GetDiskFreeSpaceExA',\
       GetDiskFreeSpaceExW,'GetDiskFreeSpaceExW',\
       GetDriveTypeA,'GetDriveTypeA',\
       GetDriveTypeW,'GetDriveTypeW',\
       GetEnvironmentStringsA,'GetEnvironmentStringsA',\
       GetEnvironmentStringsW,'GetEnvironmentStringsW',\
       GetEnvironmentVariableA,'GetEnvironmentVariableA',\
       GetEnvironmentVariableW,'GetEnvironmentVariableW',\
       GetExitCodeProcess,'GetExitCodeProcess',\
       GetExitCodeThread,'GetExitCodeThread',\
       GetFileAttributesA,'GetFileAttributesA',\
       GetFileAttributesW,'GetFileAttributesW',\
       GetFileAttributesExA,'GetFileAttributesExA',\
       GetFileAttributesExW,'GetFileAttributesExW',\
       GetFileInformationByHandle,'GetFileInformationByHandle',\
       GetFileSize,'GetFileSize',\
       GetFileTime,'GetFileTime',\
       GetFileType,'GetFileType',\
       GetFullPathNameA,'GetFullPathNameA',\
       GetFullPathNameW,'GetFullPathNameW',\
       GetHandleInformation,'GetHandleInformation',\
       GetLargestConsoleWindowSize,'GetLargestConsoleWindowSize',\
       GetLastError,'GetLastError',\
       GetLocalTime,'GetLocalTime',\
       GetLocaleInfoA,'GetLocaleInfoA',\
       GetLocaleInfoW,'GetLocaleInfoW',\
       GetLogicalDriveStringsA,'GetLogicalDriveStringsA',\
       GetLogicalDriveStringsW,'GetLogicalDriveStringsW',\
       GetLogicalDrives,'GetLogicalDrives',\
       GetLongPathNameA,'GetLongPathNameA',\
       GetLongPathNameW,'GetLongPathNameW',\
       GetMailslotInfo,'GetMailslotInfo',\
       GetModuleFileNameA,'GetModuleFileNameA',\
       GetModuleFileNameW,'GetModuleFileNameW',\
       GetModuleHandleA,'GetModuleHandleA',\
       GetModuleHandleW,'GetModuleHandleW',\
       GetNamedPipeHandleStateA,'GetNamedPipeHandleStateA',\
       GetNamedPipeHandleStateW,'GetNamedPipeHandleStateW',\
       GetNamedPipeInfo,'GetNamedPipeInfo',\
       GetNextVDMCommand,'GetNextVDMCommand',\
       GetNumberFormatA,'GetNumberFormatA',\
       GetNumberFormatW,'GetNumberFormatW',\
       GetNumberOfConsoleFonts,'GetNumberOfConsoleFonts',\
       GetNumberOfConsoleInputEvents,'GetNumberOfConsoleInputEvents',\
       GetNumberOfConsoleMouseButtons,'GetNumberOfConsoleMouseButtons',\
       GetOEMCP,'GetOEMCP',\
       GetOverlappedResult,'GetOverlappedResult',\
       GetPriorityClass,'GetPriorityClass',\
       GetPrivateProfileIntA,'GetPrivateProfileIntA',\
       GetPrivateProfileIntW,'GetPrivateProfileIntW',\
       GetPrivateProfileSectionA,'GetPrivateProfileSectionA',\
       GetPrivateProfileSectionW,'GetPrivateProfileSectionW',\
       GetPrivateProfileSectionNamesA,'GetPrivateProfileSectionNamesA',\
       GetPrivateProfileSectionNamesW,'GetPrivateProfileSectionNamesW',\
       GetPrivateProfileStringA,'GetPrivateProfileStringA',\
       GetPrivateProfileStringW,'GetPrivateProfileStringW',\
       GetPrivateProfileStructA,'GetPrivateProfileStructA',\
       GetPrivateProfileStructW,'GetPrivateProfileStructW',\
       GetProcAddress,'GetProcAddress',\
       GetProcessAffinityMask,'GetProcessAffinityMask',\
       GetProcessHeap,'GetProcessHeap',\
       GetProcessHeaps,'GetProcessHeaps',\
       GetProcessPriorityBoost,'GetProcessPriorityBoost',\
       GetProcessShutdownParameters,'GetProcessShutdownParameters',\
       GetProcessTimes,'GetProcessTimes',\
       GetProcessVersion,'GetProcessVersion',\
       GetProcessWorkingSetSize,'GetProcessWorkingSetSize',\
       GetProfileIntA,'GetProfileIntA',\
       GetProfileIntW,'GetProfileIntW',\
       GetProfileSectionA,'GetProfileSectionA',\
       GetProfileSectionW,'GetProfileSectionW',\
       GetProfileStringA,'GetProfileStringA',\
       GetProfileStringW,'GetProfileStringW',\
       GetQueuedCompletionStatus,'GetQueuedCompletionStatus',\
       GetShortPathNameA,'GetShortPathNameA',\
       GetShortPathNameW,'GetShortPathNameW',\
       GetStartupInfoA,'GetStartupInfoA',\
       GetStartupInfoW,'GetStartupInfoW',\
       GetStdHandle,'GetStdHandle',\
       GetStringTypeA,'GetStringTypeA',\
       GetStringTypeW,'GetStringTypeW',\
       GetStringTypeExA,'GetStringTypeExA',\
       GetStringTypeExW,'GetStringTypeExW',\
       GetSystemDefaultLCID,'GetSystemDefaultLCID',\
       GetSystemDefaultLangID,'GetSystemDefaultLangID',\
       GetSystemDirectoryA,'GetSystemDirectoryA',\
       GetSystemDirectoryW,'GetSystemDirectoryW',\
       GetSystemInfo,'GetSystemInfo',\
       GetSystemPowerStatus,'GetSystemPowerStatus',\
       GetSystemTime,'GetSystemTime',\
       GetSystemTimeAdjustment,'GetSystemTimeAdjustment',\
       GetSystemTimeAsFileTime,'GetSystemTimeAsFileTime',\
       GetTapeParameters,'GetTapeParameters',\
       GetTapePosition,'GetTapePosition',\
       GetTapeStatus,'GetTapeStatus',\
       GetTempFileNameA,'GetTempFileNameA',\
       GetTempFileNameW,'GetTempFileNameW',\
       GetTempPathA,'GetTempPathA',\
       GetTempPathW,'GetTempPathW',\
       GetThreadContext,'GetThreadContext',\
       GetThreadLocale,'GetThreadLocale',\
       GetThreadPriority,'GetThreadPriority',\
       GetThreadPriorityBoost,'GetThreadPriorityBoost',\
       GetThreadSelectorEntry,'GetThreadSelectorEntry',\
       GetThreadTimes,'GetThreadTimes',\
       GetTickCount,'GetTickCount',\
       GetTimeFormatA,'GetTimeFormatA',\
       GetTimeFormatW,'GetTimeFormatW',\
       GetTimeZoneInformation,'GetTimeZoneInformation',\
       GetUserDefaultLCID,'GetUserDefaultLCID',\
       GetUserDefaultLangID,'GetUserDefaultLangID',\
       GetVDMCurrentDirectories,'GetVDMCurrentDirectories',\
       GetVersion,'GetVersion',\
       GetVersionExA,'GetVersionExA',\
       GetVersionExW,'GetVersionExW',\
       GetVolumeInformationA,'GetVolumeInformationA',\
       GetVolumeInformationW,'GetVolumeInformationW',\
       GetWindowsDirectoryA,'GetWindowsDirectoryA',\
       GetWindowsDirectoryW,'GetWindowsDirectoryW',\
       GlobalAddAtomA,'GlobalAddAtomA',\
       GlobalAddAtomW,'GlobalAddAtomW',\
       GlobalAlloc,'GlobalAlloc',\
       GlobalCompact,'GlobalCompact',\
       GlobalDeleteAtom,'GlobalDeleteAtom',\
       GlobalFindAtomA,'GlobalFindAtomA',\
       GlobalFindAtomW,'GlobalFindAtomW',\
       GlobalFix,'GlobalFix',\
       GlobalFlags,'GlobalFlags',\
       GlobalFree,'GlobalFree',\
       GlobalGetAtomNameA,'GlobalGetAtomNameA',\
       GlobalGetAtomNameW,'GlobalGetAtomNameW',\
       GlobalHandle,'GlobalHandle',\
       GlobalLock,'GlobalLock',\
       GlobalMemoryStatus,'GlobalMemoryStatus',\
       GlobalMemoryStatusVlm,'GlobalMemoryStatusVlm',\
       GlobalReAlloc,'GlobalReAlloc',\
       GlobalSize,'GlobalSize',\
       GlobalUnWire,'GlobalUnWire',\
       GlobalUnfix,'GlobalUnfix',\
       GlobalUnlock,'GlobalUnlock',\
       GlobalWire,'GlobalWire',\
       Heap32First,'Heap32First',\
       Heap32ListFirst,'Heap32ListFirst',\
       Heap32ListNext,'Heap32ListNext',\
       Heap32Next,'Heap32Next',\
       HeapAlloc,'HeapAlloc',\
       HeapCompact,'HeapCompact',\
       HeapCreate,'HeapCreate',\
       HeapDestroy,'HeapDestroy',\
       HeapExtend,'HeapExtend',\
       HeapFree,'HeapFree',\
       HeapLock,'HeapLock',\
       HeapReAlloc,'HeapReAlloc',\
       HeapSize,'HeapSize',\
       HeapSummary,'HeapSummary',\
       HeapUnlock,'HeapUnlock',\
       HeapUsage,'HeapUsage',\
       HeapValidate,'HeapValidate',\
       HeapWalk,'HeapWalk',\
       InitAtomTable,'InitAtomTable',\
       InitializeCriticalSection,'InitializeCriticalSection',\
       InitializeCriticalSectionAndSpinCount,'InitializeCriticalSectionAndSpinCount',\
       InterlockedCompareExchange,'InterlockedCompareExchange',\
       InterlockedDecrement,'InterlockedDecrement',\
       InterlockedExchange,'InterlockedExchange',\
       InterlockedExchangeAdd,'InterlockedExchangeAdd',\
       InterlockedIncrement,'InterlockedIncrement',\
       InvalidateConsoleDIBits,'InvalidateConsoleDIBits',\
       IsBadCodePtr,'IsBadCodePtr',\
       IsBadHugeReadPtr,'IsBadHugeReadPtr',\
       IsBadHugeWritePtr,'IsBadHugeWritePtr',\
       IsBadReadPtr,'IsBadReadPtr',\
       IsBadStringPtrA,'IsBadStringPtrA',\
       IsBadStringPtrW,'IsBadStringPtrW',\
       IsBadWritePtr,'IsBadWritePtr',\
       IsDBCSLeadByte,'IsDBCSLeadByte',\
       IsDBCSLeadByteEx,'IsDBCSLeadByteEx',\
       IsDebuggerPresent,'IsDebuggerPresent',\
       IsProcessorFeaturePresent,'IsProcessorFeaturePresent',\
       IsValidCodePage,'IsValidCodePage',\
       IsValidLocale,'IsValidLocale',\
       LCMapStringA,'LCMapStringA',\
       LCMapStringW,'LCMapStringW',\
       LeaveCriticalSection,'LeaveCriticalSection',\
       LoadLibraryA,'LoadLibraryA',\
       LoadLibraryW,'LoadLibraryW',\
       LoadLibraryExA,'LoadLibraryExA',\
       LoadLibraryExW,'LoadLibraryExW',\
       LoadModule,'LoadModule',\
       LoadResource,'LoadResource',\
       LocalAlloc,'LocalAlloc',\
       LocalCompact,'LocalCompact',\
       LocalFileTimeToFileTime,'LocalFileTimeToFileTime',\
       LocalFlags,'LocalFlags',\
       LocalFree,'LocalFree',\
       LocalHandle,'LocalHandle',\
       LocalLock,'LocalLock',\
       LocalReAlloc,'LocalReAlloc',\
       LocalShrink,'LocalShrink',\
       LocalSize,'LocalSize',\
       LocalUnlock,'LocalUnlock',\
       LockFile,'LockFile',\
       LockFileEx,'LockFileEx',\
       LockResource,'LockResource',\
       MapViewOfFile,'MapViewOfFile',\
       MapViewOfFileEx,'MapViewOfFileEx',\
       MapViewOfFileVlm,'MapViewOfFileVlm',\
       Module32First,'Module32First',\
       Module32Next,'Module32Next',\
       MoveFileA,'MoveFileA',\
       MoveFileW,'MoveFileW',\
       MoveFileExA,'MoveFileExA',\
       MoveFileExW,'MoveFileExW',\
       MoveFileWithProgressA,'MoveFileWithProgressA',\
       MoveFileWithProgressW,'MoveFileWithProgressW',\
       MulDiv,'MulDiv',\
       MultiByteToWideChar,'MultiByteToWideChar',\
       OpenEventA,'OpenEventA',\
       OpenEventW,'OpenEventW',\
       OpenFile,'OpenFile',\
       OpenFileMappingA,'OpenFileMappingA',\
       OpenFileMappingW,'OpenFileMappingW',\
       OpenJobObjectA,'OpenJobObjectA',\
       OpenJobObjectW,'OpenJobObjectW',\
       OpenMutexA,'OpenMutexA',\
       OpenMutexW,'OpenMutexW',\
       OpenProcess,'OpenProcess',\
       OpenProfileUserMapping,'OpenProfileUserMapping',\
       OpenSemaphoreA,'OpenSemaphoreA',\
       OpenSemaphoreW,'OpenSemaphoreW',\
       OpenWaitableTimerA,'OpenWaitableTimerA',\
       OpenWaitableTimerW,'OpenWaitableTimerW',\
       OutputDebugStringA,'OutputDebugStringA',\
       OutputDebugStringW,'OutputDebugStringW',\
       PeekConsoleInputA,'PeekConsoleInputA',\
       PeekConsoleInputW,'PeekConsoleInputW',\
       PeekNamedPipe,'PeekNamedPipe',\
       PostQueuedCompletionStatus,'PostQueuedCompletionStatus',\
       PrepareTape,'PrepareTape',\
       Process32First,'Process32First',\
       Process32Next,'Process32Next',\
       PulseEvent,'PulseEvent',\
       PurgeComm,'PurgeComm',\
       QueryDosDeviceA,'QueryDosDeviceA',\
       QueryDosDeviceW,'QueryDosDeviceW',\
       QueryInformationJobObject,'QueryInformationJobObject',\
       QueryPerformanceCounter,'QueryPerformanceCounter',\
       QueryPerformanceFrequency,'QueryPerformanceFrequency',\
       QueryWin31IniFilesMappedToRegistry,'QueryWin31IniFilesMappedToRegistry',\
       QueueUserAPC,'QueueUserAPC',\
       RaiseException,'RaiseException',\
       ReadConsoleA,'ReadConsoleA',\
       ReadConsoleW,'ReadConsoleW',\
       ReadConsoleInputA,'ReadConsoleInputA',\
       ReadConsoleInputW,'ReadConsoleInputW',\
       ReadConsoleInputExA,'ReadConsoleInputExA',\
       ReadConsoleInputExW,'ReadConsoleInputExW',\
       ReadConsoleOutputA,'ReadConsoleOutputA',\
       ReadConsoleOutputW,'ReadConsoleOutputW',\
       ReadConsoleOutputAttribute,'ReadConsoleOutputAttribute',\
       ReadConsoleOutputCharacterA,'ReadConsoleOutputCharacterA',\
       ReadConsoleOutputCharacterW,'ReadConsoleOutputCharacterW',\
       ReadFile,'ReadFile',\
       ReadFileEx,'ReadFileEx',\
       ReadFileScatter,'ReadFileScatter',\
       ReadFileVlm,'ReadFileVlm',\
       ReadProcessMemory,'ReadProcessMemory',\
       ReadProcessMemoryVlm,'ReadProcessMemoryVlm',\
       RegisterConsoleVDM,'RegisterConsoleVDM',\
       RegisterWaitForInputIdle,'RegisterWaitForInputIdle',\
       RegisterWowBaseHandlers,'RegisterWowBaseHandlers',\
       RegisterWowExec,'RegisterWowExec',\
       ReleaseMutex,'ReleaseMutex',\
       ReleaseSemaphore,'ReleaseSemaphore',\
       RemoveDirectoryA,'RemoveDirectoryA',\
       RemoveDirectoryW,'RemoveDirectoryW',\
       RequestWakeupLatency,'RequestWakeupLatency',\
       ResetEvent,'ResetEvent',\
       ResumeThread,'ResumeThread',\
       RtlFillMemory,'RtlFillMemory',\
       RtlMoveMemory,'RtlMoveMemory',\
       RtlUnwind,'RtlUnwind',\
       RtlZeroMemory,'RtlZeroMemory',\
       ScrollConsoleScreenBufferA,'ScrollConsoleScreenBufferA',\
       ScrollConsoleScreenBufferW,'ScrollConsoleScreenBufferW',\
       SearchPathA,'SearchPathA',\
       SearchPathW,'SearchPathW',\
       SetCommBreak,'SetCommBreak',\
       SetCommConfig,'SetCommConfig',\
       SetCommMask,'SetCommMask',\
       SetCommState,'SetCommState',\
       SetCommTimeouts,'SetCommTimeouts',\
       SetComputerNameA,'SetComputerNameA',\
       SetComputerNameW,'SetComputerNameW',\
       SetConsoleActiveScreenBuffer,'SetConsoleActiveScreenBuffer',\
       SetConsoleCP,'SetConsoleCP',\
       SetConsoleCommandHistoryMode,'SetConsoleCommandHistoryMode',\
       SetConsoleCtrlHandler,'SetConsoleCtrlHandler',\
       SetConsoleCursor,'SetConsoleCursor',\
       SetConsoleCursorInfo,'SetConsoleCursorInfo',\
       SetConsoleCursorPosition,'SetConsoleCursorPosition',\
       SetConsoleDisplayMode,'SetConsoleDisplayMode',\
       SetConsoleFont,'SetConsoleFont',\
       SetConsoleHardwareState,'SetConsoleHardwareState',\
       SetConsoleIcon,'SetConsoleIcon',\
       SetConsoleInputExeNameA,'SetConsoleInputExeNameA',\
       SetConsoleInputExeNameW,'SetConsoleInputExeNameW',\
       SetConsoleKeyShortcuts,'SetConsoleKeyShortcuts',\
       SetConsoleMaximumWindowSize,'SetConsoleMaximumWindowSize',\
       SetConsoleMenuClose,'SetConsoleMenuClose',\
       SetConsoleMode,'SetConsoleMode',\
       SetConsoleNumberOfCommandsA,'SetConsoleNumberOfCommandsA',\
       SetConsoleNumberOfCommandsW,'SetConsoleNumberOfCommandsW',\
       SetConsoleOutputCP,'SetConsoleOutputCP',\
       SetConsolePalette,'SetConsolePalette',\
       SetConsoleScreenBufferSize,'SetConsoleScreenBufferSize',\
       SetConsoleTextAttribute,'SetConsoleTextAttribute',\
       SetConsoleTitleA,'SetConsoleTitleA',\
       SetConsoleTitleW,'SetConsoleTitleW',\
       SetConsoleWindowInfo,'SetConsoleWindowInfo',\
       SetCriticalSectionSpinCount,'SetCriticalSectionSpinCount',\
       SetCurrentDirectoryA,'SetCurrentDirectoryA',\
       SetCurrentDirectoryW,'SetCurrentDirectoryW',\
       SetDefaultCommConfigA,'SetDefaultCommConfigA',\
       SetDefaultCommConfigW,'SetDefaultCommConfigW',\
       SetEndOfFile,'SetEndOfFile',\
       SetEnvironmentVariableA,'SetEnvironmentVariableA',\
       SetEnvironmentVariableW,'SetEnvironmentVariableW',\
       SetErrorMode,'SetErrorMode',\
       SetEvent,'SetEvent',\
       SetFileApisToANSI,'SetFileApisToANSI',\
       SetFileApisToOEM,'SetFileApisToOEM',\
       SetFileAttributesA,'SetFileAttributesA',\
       SetFileAttributesW,'SetFileAttributesW',\
       SetFilePointer,'SetFilePointer',\
       SetFileTime,'SetFileTime',\
       SetHandleCount,'SetHandleCount',\
       SetHandleInformation,'SetHandleInformation',\
       SetInformationJobObject,'SetInformationJobObject',\
       SetLastConsoleEventActive,'SetLastConsoleEventActive',\
       SetLastError,'SetLastError',\
       SetLocalTime,'SetLocalTime',\
       SetLocaleInfoA,'SetLocaleInfoA',\
       SetLocaleInfoW,'SetLocaleInfoW',\
       SetMailslotInfo,'SetMailslotInfo',\
       SetNamedPipeHandleState,'SetNamedPipeHandleState',\
       SetPriorityClass,'SetPriorityClass',\
       SetProcessAffinityMask,'SetProcessAffinityMask',\
       SetProcessPriorityBoost,'SetProcessPriorityBoost',\
       SetProcessShutdownParameters,'SetProcessShutdownParameters',\
       SetProcessWorkingSetSize,'SetProcessWorkingSetSize',\
       SetStdHandle,'SetStdHandle',\
       SetSystemPowerState,'SetSystemPowerState',\
       SetSystemTime,'SetSystemTime',\
       SetSystemTimeAdjustment,'SetSystemTimeAdjustment',\
       SetTapeParameters,'SetTapeParameters',\
       SetTapePosition,'SetTapePosition',\
       SetThreadAffinityMask,'SetThreadAffinityMask',\
       SetThreadContext,'SetThreadContext',\
       SetThreadExecutionState,'SetThreadExecutionState',\
       SetThreadIdealProcessor,'SetThreadIdealProcessor',\
       SetThreadLocale,'SetThreadLocale',\
       SetThreadPriority,'SetThreadPriority',\
       SetThreadPriorityBoost,'SetThreadPriorityBoost',\
       SetTimeZoneInformation,'SetTimeZoneInformation',\
       SetUnhandledExceptionFilter,'SetUnhandledExceptionFilter',\
       SetVDMCurrentDirectories,'SetVDMCurrentDirectories',\
       SetVolumeLabelA,'SetVolumeLabelA',\
       SetVolumeLabelW,'SetVolumeLabelW',\
       SetWaitableTimer,'SetWaitableTimer',\
       SetupComm,'SetupComm',\
       ShowConsoleCursor,'ShowConsoleCursor',\
       SignalObjectAndWait,'SignalObjectAndWait',\
       SizeofResource,'SizeofResource',\
       Sleep,'Sleep',\
       SleepEx,'SleepEx',\
       SuspendThread,'SuspendThread',\
       SwitchToFiber,'SwitchToFiber',\
       SwitchToThread,'SwitchToThread',\
       SystemTimeToFileTime,'SystemTimeToFileTime',\
       SystemTimeToTzSpecificLocalTime,'SystemTimeToTzSpecificLocalTime',\
       TerminateJobObject,'TerminateJobObject',\
       TerminateProcess,'TerminateProcess',\
       TerminateThread,'TerminateThread',\
       Thread32First,'Thread32First',\
       Thread32Next,'Thread32Next',\
       TlsAlloc,'TlsAlloc',\
       TlsFree,'TlsFree',\
       TlsGetValue,'TlsGetValue',\
       TlsSetValue,'TlsSetValue',\
       Toolhelp32ReadProcessMemory,'Toolhelp32ReadProcessMemory',\
       TransactNamedPipe,'TransactNamedPipe',\
       TransmitCommChar,'TransmitCommChar',\
       TrimVirtualBuffer,'TrimVirtualBuffer',\
       TryEnterCriticalSection,'TryEnterCriticalSection',\
       UnhandledExceptionFilter,'UnhandledExceptionFilter',\
       UnlockFile,'UnlockFile',\
       UnlockFileEx,'UnlockFileEx',\
       UnmapViewOfFile,'UnmapViewOfFile',\
       UnmapViewOfFileVlm,'UnmapViewOfFileVlm',\
       UpdateResourceA,'UpdateResourceA',\
       UpdateResourceW,'UpdateResourceW',\
       VDMConsoleOperation,'VDMConsoleOperation',\
       VDMOperationStarted,'VDMOperationStarted',\
       VerLanguageNameA,'VerLanguageNameA',\
       VerLanguageNameW,'VerLanguageNameW',\
       VerifyConsoleIoHandle,'VerifyConsoleIoHandle',\
       VirtualAlloc,'VirtualAlloc',\
       VirtualAllocEx,'VirtualAllocEx',\
       VirtualAllocVlm,'VirtualAllocVlm',\
       VirtualBufferExceptionHandler,'VirtualBufferExceptionHandler',\
       VirtualFree,'VirtualFree',\
       VirtualFreeEx,'VirtualFreeEx',\
       VirtualFreeVlm,'VirtualFreeVlm',\
       VirtualLock,'VirtualLock',\
       VirtualProtect,'VirtualProtect',\
       VirtualProtectEx,'VirtualProtectEx',\
       VirtualProtectVlm,'VirtualProtectVlm',\
       VirtualQuery,'VirtualQuery',\
       VirtualQueryEx,'VirtualQueryEx',\
       VirtualQueryVlm,'VirtualQueryVlm',\
       VirtualUnlock,'VirtualUnlock',\
       WaitCommEvent,'WaitCommEvent',\
       WaitForDebugEvent,'WaitForDebugEvent',\
       WaitForMultipleObjects,'WaitForMultipleObjects',\
       WaitForMultipleObjectsEx,'WaitForMultipleObjectsEx',\
       WaitForSingleObject,'WaitForSingleObject',\
       WaitForSingleObjectEx,'WaitForSingleObjectEx',\
       WaitNamedPipeA,'WaitNamedPipeA',\
       WaitNamedPipeW,'WaitNamedPipeW',\
       WideCharToMultiByte,'WideCharToMultiByte',\
       WinExec,'WinExec',\
       WriteConsoleA,'WriteConsoleA',\
       WriteConsoleW,'WriteConsoleW',\
       WriteConsoleInputA,'WriteConsoleInputA',\
       WriteConsoleInputW,'WriteConsoleInputW',\
       WriteConsoleInputVDMA,'WriteConsoleInputVDMA',\
       WriteConsoleInputVDMW,'WriteConsoleInputVDMW',\
       WriteConsoleOutputA,'WriteConsoleOutputA',\
       WriteConsoleOutputW,'WriteConsoleOutputW',\
       WriteConsoleOutputAttribute,'WriteConsoleOutputAttribute',\
       WriteConsoleOutputCharacterA,'WriteConsoleOutputCharacterA',\
       WriteConsoleOutputCharacterW,'WriteConsoleOutputCharacterW',\
       WriteFile,'WriteFile',\
       WriteFileEx,'WriteFileEx',\
       WriteFileGather,'WriteFileGather',\
       WriteFileVlm,'WriteFileVlm',\
       WritePrivateProfileSectionA,'WritePrivateProfileSectionA',\
       WritePrivateProfileSectionW,'WritePrivateProfileSectionW',\
       WritePrivateProfileStringA,'WritePrivateProfileStringA',\
       WritePrivateProfileStringW,'WritePrivateProfileStringW',\
       WritePrivateProfileStructA,'WritePrivateProfileStructA',\
       WritePrivateProfileStructW,'WritePrivateProfileStructW',\
       WriteProcessMemory,'WriteProcessMemory',\
       WriteProcessMemoryVlm,'WriteProcessMemoryVlm',\
       WriteProfileSectionA,'WriteProfileSectionA',\
       WriteProfileSectionW,'WriteProfileSectionW',\
       WriteProfileStringA,'WriteProfileStringA',\
       WriteProfileStringW,'WriteProfileStringW',\
       WriteTapemark,'WriteTapemark',\
       _hread,'_hread',\
       _hwrite,'_hwrite',\
       _lclose,'_lclose',\
       _lcreat,'_lcreat',\
       _llseek,'_llseek',\
       _lopen,'_lopen',\
       _lread,'_lread',\
       _lwrite,'_lwrite',\
       lstrcatA,'lstrcatA',\
       lstrcatW,'lstrcatW',\
       lstrcmpA,'lstrcmpA',\
       lstrcmpW,'lstrcmpW',\
       lstrcmpiA,'lstrcmpiA',\
       lstrcmpiW,'lstrcmpiW',\
       lstrcpyA,'lstrcpyA',\
       lstrcpyW,'lstrcpyW',\
       lstrcpynA,'lstrcpynA',\
       lstrcpynW,'lstrcpynW',\
       lstrlenA,'lstrlenA',\
       lstrlenW,'lstrlenW'

api AddAtom,\
    AddConsoleAlias,\
    BeginUpdateResource,\
    BuildCommDCB,\
    BuildCommDCBAndTimeouts,\
    CallNamedPipe,\
    CommConfigDialog,\
    CompareString,\
    CopyFile,\
    CopyFileEx,\
    CreateDirectory,\
    CreateDirectoryEx,\
    CreateEvent,\
    CreateFile,\
    CreateFileMapping,\
    CreateHardLink,\
    CreateJobObject,\
    CreateMailslot,\
    CreateMutex,\
    CreateNamedPipe,\
    CreateProcess,\
    CreateSemaphore,\
    CreateWaitableTimer,\
    DefineDosDevice,\
    DeleteFile,\
    EndUpdateResource,\
    EnumCalendarInfo,\
    EnumCalendarInfoEx,\
    EnumDateFormats,\
    EnumDateFormatsEx,\
    EnumResourceLanguages,\
    EnumResourceNames,\
    EnumResourceTypes,\
    EnumSystemCodePages,\
    EnumSystemLocales,\
    EnumTimeFormats,\
    ExpandEnvironmentStrings,\
    ExpungeConsoleCommandHistory,\
    FatalAppExit,\
    FillConsoleOutputCharacter,\
    FindAtom,\
    FindFirstChangeNotification,\
    FindFirstFile,\
    FindFirstFileEx,\
    FindNextFile,\
    FindResource,\
    FindResourceEx,\
    FoldString,\
    FormatMessage,\
    FreeEnvironmentStrings,\
    GetAtomName,\
    GetBinaryType,\
    GetCPInfoEx,\
    GetCommandLine,\
    GetCompressedFileSize,\
    GetComputerName,\
    GetConsoleAlias,\
    GetConsoleAliasExes,\
    GetConsoleAliasExesLength,\
    GetConsoleAliases,\
    GetConsoleAliasesLength,\
    GetConsoleCommandHistory,\
    GetConsoleCommandHistoryLength,\
    GetConsoleInputExeName,\
    GetConsoleKeyboardLayoutName,\
    GetConsoleTitle,\
    GetCurrencyFormat,\
    GetCurrentDirectory,\
    GetDateFormat,\
    GetDefaultCommConfig,\
    GetDiskFreeSpace,\
    GetDiskFreeSpaceEx,\
    GetDriveType,\
    GetEnvironmentStrings,\
    GetEnvironmentVariable,\
    GetFileAttributes,\
    GetFileAttributesEx,\
    GetFullPathName,\
    GetLocaleInfo,\
    GetLogicalDriveStrings,\
    GetLongPathName,\
    GetModuleFileName,\
    GetModuleHandle,\
    GetNamedPipeHandleState,\
    GetNumberFormat,\
    GetPrivateProfileInt,\
    GetPrivateProfileSection,\
    GetPrivateProfileSectionNames,\
    GetPrivateProfileString,\
    GetPrivateProfileStruct,\
    GetProfileInt,\
    GetProfileSection,\
    GetProfileString,\
    GetShortPathName,\
    GetStartupInfo,\
    GetStringType,\
    GetStringTypeEx,\
    GetSystemDirectory,\
    GetTempFileName,\
    GetTempPath,\
    GetTimeFormat,\
    GetVersionEx,\
    GetVolumeInformation,\
    GetWindowsDirectory,\
    GlobalAddAtom,\
    GlobalFindAtom,\
    GlobalGetAtomName,\
    IsBadStringPtr,\
    LCMapString,\
    LoadLibrary,\
    LoadLibraryEx,\
    MoveFile,\
    MoveFileEx,\
    MoveFileWithProgress,\
    OpenEvent,\
    OpenFileMapping,\
    OpenJobObject,\
    OpenMutex,\
    OpenSemaphore,\
    OpenWaitableTimer,\
    OutputDebugString,\
    PeekConsoleInput,\
    QueryDosDevice,\
    ReadConsole,\
    ReadConsoleInput,\
    ReadConsoleInputEx,\
    ReadConsoleOutput,\
    ReadConsoleOutputCharacter,\
    RemoveDirectory,\
    ScrollConsoleScreenBuffer,\
    SearchPath,\
    SetComputerName,\
    SetConsoleInputExeName,\
    SetConsoleNumberOfCommands,\
    SetConsoleTitle,\
    SetCurrentDirectory,\
    SetDefaultCommConfig,\
    SetEnvironmentVariable,\
    SetFileAttributes,\
    SetLocaleInfo,\
    SetVolumeLabel,\
    UpdateResource,\
    VerLanguageName,\
    WaitNamedPipe,\
    WriteConsole,\
    WriteConsoleInput,\
    WriteConsoleInputVDM,\
    WriteConsoleOutput,\
    WriteConsoleOutputCharacter,\
    WritePrivateProfileSection,\
    WritePrivateProfileString,\
    WritePrivateProfileStruct,\
    WriteProfileSection,\
    WriteProfileString,\
    lstrcat,\
    lstrcmp,\
    lstrcmpi,\
    lstrcpy,\
    lstrcpyn,\
    lstrlen


; USER32 API calls

import user32,\
       ActivateKeyboardLayout,'ActivateKeyboardLayout',\
       AdjustWindowRect,'AdjustWindowRect',\
       AdjustWindowRectEx,'AdjustWindowRectEx',\
       AnimateWindow,'AnimateWindow',\
       AnyPopup,'AnyPopup',\
       AppendMenuA,'AppendMenuA',\
       AppendMenuW,'AppendMenuW',\
       ArrangeIconicWindows,'ArrangeIconicWindows',\
       AttachThreadInput,'AttachThreadInput',\
       BeginDeferWindowPos,'BeginDeferWindowPos',\
       BeginPaint,'BeginPaint',\
       BlockInput,'BlockInput',\
       BringWindowToTop,'BringWindowToTop',\
       BroadcastSystemMessageA,'BroadcastSystemMessageA',\
       BroadcastSystemMessageW,'BroadcastSystemMessageW',\
       CallMsgFilterA,'CallMsgFilterA',\
       CallMsgFilterW,'CallMsgFilterW',\
       CallNextHookEx,'CallNextHookEx',\
       CallWindowProcA,'CallWindowProcA',\
       CallWindowProcW,'CallWindowProcW',\
       CascadeChildWindows,'CascadeChildWindows',\
       CascadeWindows,'CascadeWindows',\
       ChangeClipboardChain,'ChangeClipboardChain',\
       ChangeDisplaySettingsA,'ChangeDisplaySettingsA',\
       ChangeDisplaySettingsW,'ChangeDisplaySettingsW',\
       ChangeDisplaySettingsExA,'ChangeDisplaySettingsExA',\
       ChangeDisplaySettingsExW,'ChangeDisplaySettingsExW',\
       ChangeMenuA,'ChangeMenuA',\
       ChangeMenuW,'ChangeMenuW',\
       CharLowerA,'CharLowerA',\
       CharLowerW,'CharLowerW',\
       CharLowerBuffA,'CharLowerBuffA',\
       CharLowerBuffW,'CharLowerBuffW',\
       CharNextA,'CharNextA',\
       CharNextW,'CharNextW',\
       CharNextExA,'CharNextExA',\
       CharNextExW,'CharNextExW',\
       CharPrevA,'CharPrevA',\
       CharPrevW,'CharPrevW',\
       CharPrevExA,'CharPrevExA',\
       CharPrevExW,'CharPrevExW',\
       CharToOemA,'CharToOemA',\
       CharToOemW,'CharToOemW',\
       CharToOemBuffA,'CharToOemBuffA',\
       CharToOemBuffW,'CharToOemBuffW',\
       CharUpperA,'CharUpperA',\
       CharUpperW,'CharUpperW',\
       CharUpperBuffA,'CharUpperBuffA',\
       CharUpperBuffW,'CharUpperBuffW',\
       CheckDlgButton,'CheckDlgButton',\
       CheckMenuItem,'CheckMenuItem',\
       CheckMenuRadioItem,'CheckMenuRadioItem',\
       CheckRadioButton,'CheckRadioButton',\
       ChildWindowFromPoint,'ChildWindowFromPoint',\
       ChildWindowFromPointEx,'ChildWindowFromPointEx',\
       ClientToScreen,'ClientToScreen',\
       ClipCursor,'ClipCursor',\
       CloseClipboard,'CloseClipboard',\
       CloseDesktop,'CloseDesktop',\
       CloseWindow,'CloseWindow',\
       CloseWindowStation,'CloseWindowStation',\
       CopyAcceleratorTableA,'CopyAcceleratorTableA',\
       CopyAcceleratorTableW,'CopyAcceleratorTableW',\
       CopyIcon,'CopyIcon',\
       CopyImage,'CopyImage',\
       CopyRect,'CopyRect',\
       CountClipboardFormats,'CountClipboardFormats',\
       CreateAcceleratorTableA,'CreateAcceleratorTableA',\
       CreateAcceleratorTableW,'CreateAcceleratorTableW',\
       CreateCaret,'CreateCaret',\
       CreateCursor,'CreateCursor',\
       CreateDesktopA,'CreateDesktopA',\
       CreateDesktopW,'CreateDesktopW',\
       CreateDialogIndirectParamA,'CreateDialogIndirectParamA',\
       CreateDialogIndirectParamW,'CreateDialogIndirectParamW',\
       CreateDialogParamA,'CreateDialogParamA',\
       CreateDialogParamW,'CreateDialogParamW',\
       CreateIcon,'CreateIcon',\
       CreateIconFromResource,'CreateIconFromResource',\
       CreateIconFromResourceEx,'CreateIconFromResourceEx',\
       CreateIconIndirect,'CreateIconIndirect',\
       CreateMDIWindowA,'CreateMDIWindowA',\
       CreateMDIWindowW,'CreateMDIWindowW',\
       CreateMenu,'CreateMenu',\
       CreatePopupMenu,'CreatePopupMenu',\
       CreateWindowExA,'CreateWindowExA',\
       CreateWindowExW,'CreateWindowExW',\
       CreateWindowStationA,'CreateWindowStationA',\
       CreateWindowStationW,'CreateWindowStationW',\
       DdeAbandonTransaction,'DdeAbandonTransaction',\
       DdeAccessData,'DdeAccessData',\
       DdeAddData,'DdeAddData',\
       DdeClientTransaction,'DdeClientTransaction',\
       DdeCmpStringHandles,'DdeCmpStringHandles',\
       DdeConnect,'DdeConnect',\
       DdeConnectList,'DdeConnectList',\
       DdeCreateDataHandle,'DdeCreateDataHandle',\
       DdeCreateStringHandleA,'DdeCreateStringHandleA',\
       DdeCreateStringHandleW,'DdeCreateStringHandleW',\
       DdeDisconnect,'DdeDisconnect',\
       DdeDisconnectList,'DdeDisconnectList',\
       DdeEnableCallback,'DdeEnableCallback',\
       DdeFreeDataHandle,'DdeFreeDataHandle',\
       DdeFreeStringHandle,'DdeFreeStringHandle',\
       DdeGetData,'DdeGetData',\
       DdeGetLastError,'DdeGetLastError',\
       DdeGetQualityOfService,'DdeGetQualityOfService',\
       DdeImpersonateClient,'DdeImpersonateClient',\
       DdeInitializeA,'DdeInitializeA',\
       DdeInitializeW,'DdeInitializeW',\
       DdeKeepStringHandle,'DdeKeepStringHandle',\
       DdeNameService,'DdeNameService',\
       DdePostAdvise,'DdePostAdvise',\
       DdeQueryConvInfo,'DdeQueryConvInfo',\
       DdeQueryNextServer,'DdeQueryNextServer',\
       DdeQueryStringA,'DdeQueryStringA',\
       DdeQueryStringW,'DdeQueryStringW',\
       DdeReconnect,'DdeReconnect',\
       DdeSetQualityOfService,'DdeSetQualityOfService',\
       DdeSetUserHandle,'DdeSetUserHandle',\
       DdeUnaccessData,'DdeUnaccessData',\
       DdeUninitialize,'DdeUninitialize',\
       DefDlgProcA,'DefDlgProcA',\
       DefDlgProcW,'DefDlgProcW',\
       DefFrameProcA,'DefFrameProcA',\
       DefFrameProcW,'DefFrameProcW',\
       DefMDIChildProcA,'DefMDIChildProcA',\
       DefMDIChildProcW,'DefMDIChildProcW',\
       DefWindowProcA,'DefWindowProcA',\
       DefWindowProcW,'DefWindowProcW',\
       DeferWindowPos,'DeferWindowPos',\
       DeleteMenu,'DeleteMenu',\
       DestroyAcceleratorTable,'DestroyAcceleratorTable',\
       DestroyCaret,'DestroyCaret',\
       DestroyCursor,'DestroyCursor',\
       DestroyIcon,'DestroyIcon',\
       DestroyMenu,'DestroyMenu',\
       DestroyWindow,'DestroyWindow',\
       DialogBoxIndirectParamA,'DialogBoxIndirectParamA',\
       DialogBoxIndirectParamW,'DialogBoxIndirectParamW',\
       DialogBoxParamA,'DialogBoxParamA',\
       DialogBoxParamW,'DialogBoxParamW',\
       DispatchMessageA,'DispatchMessageA',\
       DispatchMessageW,'DispatchMessageW',\
       DlgDirListA,'DlgDirListA',\
       DlgDirListW,'DlgDirListW',\
       DlgDirListComboBoxA,'DlgDirListComboBoxA',\
       DlgDirListComboBoxW,'DlgDirListComboBoxW',\
       DlgDirSelectComboBoxExA,'DlgDirSelectComboBoxExA',\
       DlgDirSelectComboBoxExW,'DlgDirSelectComboBoxExW',\
       DlgDirSelectExA,'DlgDirSelectExA',\
       DlgDirSelectExW,'DlgDirSelectExW',\
       DragDetect,'DragDetect',\
       DragObject,'DragObject',\
       DrawAnimatedRects,'DrawAnimatedRects',\
       DrawCaption,'DrawCaption',\
       DrawEdge,'DrawEdge',\
       DrawFocusRect,'DrawFocusRect',\
       DrawFrame,'DrawFrame',\
       DrawFrameControl,'DrawFrameControl',\
       DrawIcon,'DrawIcon',\
       DrawIconEx,'DrawIconEx',\
       DrawMenuBar,'DrawMenuBar',\
       DrawStateA,'DrawStateA',\
       DrawStateW,'DrawStateW',\
       DrawTextA,'DrawTextA',\
       DrawTextW,'DrawTextW',\
       DrawTextExA,'DrawTextExA',\
       DrawTextExW,'DrawTextExW',\
       EditWndProc,'EditWndProc',\
       EmptyClipboard,'EmptyClipboard',\
       EnableMenuItem,'EnableMenuItem',\
       EnableScrollBar,'EnableScrollBar',\
       EnableWindow,'EnableWindow',\
       EndDeferWindowPos,'EndDeferWindowPos',\
       EndDialog,'EndDialog',\
       EndMenu,'EndMenu',\
       EndPaint,'EndPaint',\
       EnumChildWindows,'EnumChildWindows',\
       EnumClipboardFormats,'EnumClipboardFormats',\
       EnumDesktopWindows,'EnumDesktopWindows',\
       EnumDesktopsA,'EnumDesktopsA',\
       EnumDesktopsW,'EnumDesktopsW',\
       EnumDisplayMonitors,'EnumDisplayMonitors',\
       EnumDisplaySettingsA,'EnumDisplaySettingsA',\
       EnumDisplaySettingsW,'EnumDisplaySettingsW',\
       EnumDisplaySettingsExA,'EnumDisplaySettingsExA',\
       EnumDisplaySettingsExW,'EnumDisplaySettingsExW',\
       EnumPropsA,'EnumPropsA',\
       EnumPropsW,'EnumPropsW',\
       EnumPropsExA,'EnumPropsExA',\
       EnumPropsExW,'EnumPropsExW',\
       EnumThreadWindows,'EnumThreadWindows',\
       EnumWindowStationsA,'EnumWindowStationsA',\
       EnumWindowStationsW,'EnumWindowStationsW',\
       EnumWindows,'EnumWindows',\
       EqualRect,'EqualRect',\
       ExcludeUpdateRgn,'ExcludeUpdateRgn',\
       ExitWindowsEx,'ExitWindowsEx',\
       FillRect,'FillRect',\
       FindWindowA,'FindWindowA',\
       FindWindowW,'FindWindowW',\
       FindWindowExA,'FindWindowExA',\
       FindWindowExW,'FindWindowExW',\
       FlashWindow,'FlashWindow',\
       FrameRect,'FrameRect',\
       FreeDDElParam,'FreeDDElParam',\
       GetActiveWindow,'GetActiveWindow',\
       GetAltTabInfoA,'GetAltTabInfoA',\
       GetAltTabInfoW,'GetAltTabInfoW',\
       GetAncestor,'GetAncestor',\
       GetAsyncKeyState,'GetAsyncKeyState',\
       GetCapture,'GetCapture',\
       GetCaretBlinkTime,'GetCaretBlinkTime',\
       GetCaretPos,'GetCaretPos',\
       GetClassInfoA,'GetClassInfoA',\
       GetClassInfoW,'GetClassInfoW',\
       GetClassInfoExA,'GetClassInfoExA',\
       GetClassInfoExW,'GetClassInfoExW',\
       GetClassLongA,'GetClassLongA',\
       GetClassLongW,'GetClassLongW',\
       GetClassNameA,'GetClassNameA',\
       GetClassNameW,'GetClassNameW',\
       GetClassWord,'GetClassWord',\
       GetClientRect,'GetClientRect',\
       GetClipCursor,'GetClipCursor',\
       GetClipboardData,'GetClipboardData',\
       GetClipboardFormatNameA,'GetClipboardFormatNameA',\
       GetClipboardFormatNameW,'GetClipboardFormatNameW',\
       GetClipboardSequenceNumberA,'GetClipboardSequenceNumberA',\
       GetClipboardSequenceNumberW,'GetClipboardSequenceNumberW',\
       GetClipboardViewer,'GetClipboardViewer',\
       GetComboBoxInfo,'GetComboBoxInfo',\
       GetCursor,'GetCursor',\
       GetCursorInfo,'GetCursorInfo',\
       GetCursorPos,'GetCursorPos',\
       GetDC,'GetDC',\
       GetDCEx,'GetDCEx',\
       GetDesktopWindow,'GetDesktopWindow',\
       GetDialogBaseUnits,'GetDialogBaseUnits',\
       GetDlgCtrlID,'GetDlgCtrlID',\
       GetDlgItem,'GetDlgItem',\
       GetDlgItemInt,'GetDlgItemInt',\
       GetDlgItemTextA,'GetDlgItemTextA',\
       GetDlgItemTextW,'GetDlgItemTextW',\
       GetDoubleClickTime,'GetDoubleClickTime',\
       GetFocus,'GetFocus',\
       GetForegroundWindow,'GetForegroundWindow',\
       GetGUIThreadInfo,'GetGUIThreadInfo',\
       GetGuiResources,'GetGuiResources',\
       GetIconInfo,'GetIconInfo',\
       GetInputDesktop,'GetInputDesktop',\
       GetInputState,'GetInputState',\
       GetKBCodePage,'GetKBCodePage',\
       GetKeyNameTextA,'GetKeyNameTextA',\
       GetKeyNameTextW,'GetKeyNameTextW',\
       GetKeyState,'GetKeyState',\
       GetKeyboardLayout,'GetKeyboardLayout',\
       GetKeyboardLayoutList,'GetKeyboardLayoutList',\
       GetKeyboardLayoutNameA,'GetKeyboardLayoutNameA',\
       GetKeyboardLayoutNameW,'GetKeyboardLayoutNameW',\
       GetKeyboardState,'GetKeyboardState',\
       GetKeyboardType,'GetKeyboardType',\
       GetLastActivePopup,'GetLastActivePopup',\
       GetLastInputInfo,'GetLastInputInfo',\
       GetLayeredWindowAttributes,'GetLayeredWindowAttributes',\
       GetListBoxInfo,'GetListBoxInfo',\
       GetMenu,'GetMenu',\
       GetMenuBarInfo,'GetMenuBarInfo',\
       GetMenuCheckMarkDimensions,'GetMenuCheckMarkDimensions',\
       GetMenuContextHelpId,'GetMenuContextHelpId',\
       GetMenuDefaultItem,'GetMenuDefaultItem',\
       GetMenuInfo,'GetMenuInfo',\
       GetMenuItemCount,'GetMenuItemCount',\
       GetMenuItemID,'GetMenuItemID',\
       GetMenuItemInfoA,'GetMenuItemInfoA',\
       GetMenuItemInfoW,'GetMenuItemInfoW',\
       GetMenuItemRect,'GetMenuItemRect',\
       GetMenuState,'GetMenuState',\
       GetMenuStringA,'GetMenuStringA',\
       GetMenuStringW,'GetMenuStringW',\
       GetMessageA,'GetMessageA',\
       GetMessageW,'GetMessageW',\
       GetMessageExtraInfo,'GetMessageExtraInfo',\
       GetMessagePos,'GetMessagePos',\
       GetMessageTime,'GetMessageTime',\
       GetMonitorInfoA,'GetMonitorInfoA',\
       GetMonitorInfoW,'GetMonitorInfoW',\
       GetMouseMovePoints,'GetMouseMovePoints',\
       GetNextDlgGroupItem,'GetNextDlgGroupItem',\
       GetNextDlgTabItem,'GetNextDlgTabItem',\
       GetOpenClipboardWindow,'GetOpenClipboardWindow',\
       GetParent,'GetParent',\
       GetPriorityClipboardFormat,'GetPriorityClipboardFormat',\
       GetProcessWindowStation,'GetProcessWindowStation',\
       GetPropA,'GetPropA',\
       GetPropW,'GetPropW',\
       GetQueueStatus,'GetQueueStatus',\
       GetScrollBarInfo,'GetScrollBarInfo',\
       GetScrollInfo,'GetScrollInfo',\
       GetScrollPos,'GetScrollPos',\
       GetScrollRange,'GetScrollRange',\
       GetShellWindow,'GetShellWindow',\
       GetSubMenu,'GetSubMenu',\
       GetSysColor,'GetSysColor',\
       GetSysColorBrush,'GetSysColorBrush',\
       GetSystemMenu,'GetSystemMenu',\
       GetSystemMetrics,'GetSystemMetrics',\
       GetTabbedTextExtentA,'GetTabbedTextExtentA',\
       GetTabbedTextExtentW,'GetTabbedTextExtentW',\
       GetThreadDesktop,'GetThreadDesktop',\
       GetTitleBarInfo,'GetTitleBarInfo',\
       GetTopWindow,'GetTopWindow',\
       GetUpdateRect,'GetUpdateRect',\
       GetUpdateRgn,'GetUpdateRgn',\
       GetUserObjectInformationA,'GetUserObjectInformationA',\
       GetUserObjectInformationW,'GetUserObjectInformationW',\
       GetUserObjectSecurity,'GetUserObjectSecurity',\
       GetWindow,'GetWindow',\
       GetWindowContextHelpId,'GetWindowContextHelpId',\
       GetWindowDC,'GetWindowDC',\
       GetWindowInfo,'GetWindowInfo',\
       GetWindowLongA,'GetWindowLongA',\
       GetWindowLongW,'GetWindowLongW',\
       GetWindowModuleFileNameA,'GetWindowModuleFileNameA',\
       GetWindowModuleFileNameW,'GetWindowModuleFileNameW',\
       GetWindowPlacement,'GetWindowPlacement',\
       GetWindowRect,'GetWindowRect',\
       GetWindowRgn,'GetWindowRgn',\
       GetWindowTextA,'GetWindowTextA',\
       GetWindowTextW,'GetWindowTextW',\
       GetWindowTextLengthA,'GetWindowTextLengthA',\
       GetWindowTextLengthW,'GetWindowTextLengthW',\
       GetWindowThreadProcessId,'GetWindowThreadProcessId',\
       GetWindowWord,'GetWindowWord',\
       GrayStringA,'GrayStringA',\
       GrayStringW,'GrayStringW',\
       HideCaret,'HideCaret',\
       HiliteMenuItem,'HiliteMenuItem',\
       IMPGetIMEA,'IMPGetIMEA',\
       IMPGetIMEW,'IMPGetIMEW',\
       IMPQueryIMEA,'IMPQueryIMEA',\
       IMPQueryIMEW,'IMPQueryIMEW',\
       IMPSetIMEA,'IMPSetIMEA',\
       IMPSetIMEW,'IMPSetIMEW',\
       ImpersonateDdeClientWindow,'ImpersonateDdeClientWindow',\
       InSendMessage,'InSendMessage',\
       InSendMessageEx,'InSendMessageEx',\
       InflateRect,'InflateRect',\
       InsertMenuA,'InsertMenuA',\
       InsertMenuW,'InsertMenuW',\
       InsertMenuItemA,'InsertMenuItemA',\
       InsertMenuItemW,'InsertMenuItemW',\
       IntersectRect,'IntersectRect',\
       InvalidateRect,'InvalidateRect',\
       InvalidateRgn,'InvalidateRgn',\
       InvertRect,'InvertRect',\
       IsCharAlphaA,'IsCharAlphaA',\
       IsCharAlphaW,'IsCharAlphaW',\
       IsCharAlphaNumericA,'IsCharAlphaNumericA',\
       IsCharAlphaNumericW,'IsCharAlphaNumericW',\
       IsCharLowerA,'IsCharLowerA',\
       IsCharLowerW,'IsCharLowerW',\
       IsCharUpperA,'IsCharUpperA',\
       IsCharUpperW,'IsCharUpperW',\
       IsChild,'IsChild',\
       IsClipboardFormatAvailable,'IsClipboardFormatAvailable',\
       IsDialogMessageA,'IsDialogMessageA',\
       IsDialogMessageW,'IsDialogMessageW',\
       IsDlgButtonChecked,'IsDlgButtonChecked',\
       IsIconic,'IsIconic',\
       IsMenu,'IsMenu',\
       IsRectEmpty,'IsRectEmpty',\
       IsWindow,'IsWindow',\
       IsWindowEnabled,'IsWindowEnabled',\
       IsWindowUnicode,'IsWindowUnicode',\
       IsWindowVisible,'IsWindowVisible',\
       IsZoomed,'IsZoomed',\
       KillSystemTimer,'KillSystemTimer',\
       KillTimer,'KillTimer',\
       LoadAcceleratorsA,'LoadAcceleratorsA',\
       LoadAcceleratorsW,'LoadAcceleratorsW',\
       LoadBitmapA,'LoadBitmapA',\
       LoadBitmapW,'LoadBitmapW',\
       LoadCursorA,'LoadCursorA',\
       LoadCursorW,'LoadCursorW',\
       LoadCursorFromFileA,'LoadCursorFromFileA',\
       LoadCursorFromFileW,'LoadCursorFromFileW',\
       LoadIconA,'LoadIconA',\
       LoadIconW,'LoadIconW',\
       LoadImageA,'LoadImageA',\
       LoadImageW,'LoadImageW',\
       LoadKeyboardLayoutA,'LoadKeyboardLayoutA',\
       LoadKeyboardLayoutW,'LoadKeyboardLayoutW',\
       LoadMenuA,'LoadMenuA',\
       LoadMenuW,'LoadMenuW',\
       LoadMenuIndirectA,'LoadMenuIndirectA',\
       LoadMenuIndirectW,'LoadMenuIndirectW',\
       LoadStringA,'LoadStringA',\
       LoadStringW,'LoadStringW',\
       LockWindowUpdate,'LockWindowUpdate',\
       LockWorkStation,'LockWorkStation',\
       LookupIconIdFromDirectory,'LookupIconIdFromDirectory',\
       LookupIconIdFromDirectoryEx,'LookupIconIdFromDirectoryEx',\
       MapDialogRect,'MapDialogRect',\
       MapVirtualKeyA,'MapVirtualKeyA',\
       MapVirtualKeyW,'MapVirtualKeyW',\
       MapVirtualKeyExA,'MapVirtualKeyExA',\
       MapVirtualKeyExW,'MapVirtualKeyExW',\
       MapWindowPoints,'MapWindowPoints',\
       MenuItemFromPoint,'MenuItemFromPoint',\
       MessageBeep,'MessageBeep',\
       MessageBoxA,'MessageBoxA',\
       MessageBoxW,'MessageBoxW',\
       MessageBoxExA,'MessageBoxExA',\
       MessageBoxExW,'MessageBoxExW',\
       MessageBoxIndirectA,'MessageBoxIndirectA',\
       MessageBoxIndirectW,'MessageBoxIndirectW',\
       ModifyMenuA,'ModifyMenuA',\
       ModifyMenuW,'ModifyMenuW',\
       MonitorFromPoint,'MonitorFromPoint',\
       MonitorFromRect,'MonitorFromRect',\
       MonitorFromWindow,'MonitorFromWindow',\
       MoveWindow,'MoveWindow',\
       MsgWaitForMultipleObjects,'MsgWaitForMultipleObjects',\
       MsgWaitForMultipleObjectsEx,'MsgWaitForMultipleObjectsEx',\
       NotifyWinEvent,'NotifyWinEvent',\
       OemKeyScan,'OemKeyScan',\
       OemToCharA,'OemToCharA',\
       OemToCharW,'OemToCharW',\
       OemToCharBuffA,'OemToCharBuffA',\
       OemToCharBuffW,'OemToCharBuffW',\
       OffsetRect,'OffsetRect',\
       OpenClipboard,'OpenClipboard',\
       OpenDesktopA,'OpenDesktopA',\
       OpenDesktopW,'OpenDesktopW',\
       OpenIcon,'OpenIcon',\
       OpenInputDesktop,'OpenInputDesktop',\
       OpenWindowStationA,'OpenWindowStationA',\
       OpenWindowStationW,'OpenWindowStationW',\
       PackDDElParam,'PackDDElParam',\
       PaintDesktop,'PaintDesktop',\
       PeekMessageA,'PeekMessageA',\
       PeekMessageW,'PeekMessageW',\
       PostMessageA,'PostMessageA',\
       PostMessageW,'PostMessageW',\
       PostQuitMessage,'PostQuitMessage',\
       PostThreadMessageA,'PostThreadMessageA',\
       PostThreadMessageW,'PostThreadMessageW',\
       PtInRect,'PtInRect',\
       RealChildWindowFromPoint,'RealChildWindowFromPoint',\
       RealGetWindowClassA,'RealGetWindowClassA',\
       RealGetWindowClassW,'RealGetWindowClassW',\
       RedrawWindow,'RedrawWindow',\
       RegisterClassA,'RegisterClassA',\
       RegisterClassW,'RegisterClassW',\
       RegisterClassExA,'RegisterClassExA',\
       RegisterClassExW,'RegisterClassExW',\
       RegisterClipboardFormatA,'RegisterClipboardFormatA',\
       RegisterClipboardFormatW,'RegisterClipboardFormatW',\
       RegisterDeviceNotificationA,'RegisterDeviceNotificationA',\
       RegisterDeviceNotificationW,'RegisterDeviceNotificationW',\
       RegisterHotKey,'RegisterHotKey',\
       RegisterWindowMessageA,'RegisterWindowMessageA',\
       RegisterWindowMessageW,'RegisterWindowMessageW',\
       ReleaseCapture,'ReleaseCapture',\
       ReleaseDC,'ReleaseDC',\
       RemoveMenu,'RemoveMenu',\
       RemovePropA,'RemovePropA',\
       RemovePropW,'RemovePropW',\
       ReplyMessage,'ReplyMessage',\
       ReuseDDElParam,'ReuseDDElParam',\
       ScreenToClient,'ScreenToClient',\
       ScrollChildren,'ScrollChildren',\
       ScrollDC,'ScrollDC',\
       ScrollWindow,'ScrollWindow',\
       ScrollWindowEx,'ScrollWindowEx',\
       SendDlgItemMessageA,'SendDlgItemMessageA',\
       SendDlgItemMessageW,'SendDlgItemMessageW',\
       SendIMEMessageExA,'SendIMEMessageExA',\
       SendIMEMessageExW,'SendIMEMessageExW',\
       SendInput,'SendInput',\
       SendMessageA,'SendMessageA',\
       SendMessageW,'SendMessageW',\
       SendMessageCallbackA,'SendMessageCallbackA',\
       SendMessageCallbackW,'SendMessageCallbackW',\
       SendMessageTimeoutA,'SendMessageTimeoutA',\
       SendMessageTimeoutW,'SendMessageTimeoutW',\
       SendNotifyMessageA,'SendNotifyMessageA',\
       SendNotifyMessageW,'SendNotifyMessageW',\
       SetActiveWindow,'SetActiveWindow',\
       SetCapture,'SetCapture',\
       SetCaretBlinkTime,'SetCaretBlinkTime',\
       SetCaretPos,'SetCaretPos',\
       SetClassLongA,'SetClassLongA',\
       SetClassLongW,'SetClassLongW',\
       SetClassWord,'SetClassWord',\
       SetClipboardData,'SetClipboardData',\
       SetClipboardViewer,'SetClipboardViewer',\
       SetCursor,'SetCursor',\
       SetCursorPos,'SetCursorPos',\
       SetDebugErrorLevel,'SetDebugErrorLevel',\
       SetDeskWallpaper,'SetDeskWallpaper',\
       SetDlgItemInt,'SetDlgItemInt',\
       SetDlgItemTextA,'SetDlgItemTextA',\
       SetDlgItemTextW,'SetDlgItemTextW',\
       SetDoubleClickTime,'SetDoubleClickTime',\
       SetFocus,'SetFocus',\
       SetForegroundWindow,'SetForegroundWindow',\
       SetKeyboardState,'SetKeyboardState',\
       SetLastErrorEx,'SetLastErrorEx',\
       SetLayeredWindowAttributes,'SetLayeredWindowAttributes',\
       SetMenu,'SetMenu',\
       SetMenuContextHelpId,'SetMenuContextHelpId',\
       SetMenuDefaultItem,'SetMenuDefaultItem',\
       SetMenuInfo,'SetMenuInfo',\
       SetMenuItemBitmaps,'SetMenuItemBitmaps',\
       SetMenuItemInfoA,'SetMenuItemInfoA',\
       SetMenuItemInfoW,'SetMenuItemInfoW',\
       SetMessageExtraInfo,'SetMessageExtraInfo',\
       SetMessageQueue,'SetMessageQueue',\
       SetParent,'SetParent',\
       SetProcessWindowStation,'SetProcessWindowStation',\
       SetPropA,'SetPropA',\
       SetPropW,'SetPropW',\
       SetRect,'SetRect',\
       SetRectEmpty,'SetRectEmpty',\
       SetScrollInfo,'SetScrollInfo',\
       SetScrollPos,'SetScrollPos',\
       SetScrollRange,'SetScrollRange',\
       SetShellWindow,'SetShellWindow',\
       SetSysColors,'SetSysColors',\
       SetSystemCursor,'SetSystemCursor',\
       SetSystemMenu,'SetSystemMenu',\
       SetSystemTimer,'SetSystemTimer',\
       SetThreadDesktop,'SetThreadDesktop',\
       SetTimer,'SetTimer',\
       SetUserObjectInformationA,'SetUserObjectInformationA',\
       SetUserObjectInformationW,'SetUserObjectInformationW',\
       SetUserObjectSecurity,'SetUserObjectSecurity',\
       SetWinEventHook,'SetWinEventHook',\
       SetWindowContextHelpId,'SetWindowContextHelpId',\
       SetWindowLongA,'SetWindowLongA',\
       SetWindowLongW,'SetWindowLongW',\
       SetWindowPlacement,'SetWindowPlacement',\
       SetWindowPos,'SetWindowPos',\
       SetWindowRgn,'SetWindowRgn',\
       SetWindowTextA,'SetWindowTextA',\
       SetWindowTextW,'SetWindowTextW',\
       SetWindowWord,'SetWindowWord',\
       SetWindowsHookA,'SetWindowsHookA',\
       SetWindowsHookW,'SetWindowsHookW',\
       SetWindowsHookExA,'SetWindowsHookExA',\
       SetWindowsHookExW,'SetWindowsHookExW',\
       ShowCaret,'ShowCaret',\
       ShowCursor,'ShowCursor',\
       ShowOwnedPopups,'ShowOwnedPopups',\
       ShowScrollBar,'ShowScrollBar',\
       ShowWindow,'ShowWindow',\
       ShowWindowAsync,'ShowWindowAsync',\
       SubtractRect,'SubtractRect',\
       SwapMouseButton,'SwapMouseButton',\
       SwitchDesktop,'SwitchDesktop',\
       SystemParametersInfoA,'SystemParametersInfoA',\
       SystemParametersInfoW,'SystemParametersInfoW',\
       TabbedTextOutA,'TabbedTextOutA',\
       TabbedTextOutW,'TabbedTextOutW',\
       TileChildWindows,'TileChildWindows',\
       TileWindows,'TileWindows',\
       ToAscii,'ToAscii',\
       ToAsciiEx,'ToAsciiEx',\
       ToUnicode,'ToUnicode',\
       ToUnicodeEx,'ToUnicodeEx',\
       TrackMouseEvent,'TrackMouseEvent',\
       TrackPopupMenu,'TrackPopupMenu',\
       TrackPopupMenuEx,'TrackPopupMenuEx',\
       TranslateAcceleratorA,'TranslateAcceleratorA',\
       TranslateAcceleratorW,'TranslateAcceleratorW',\
       TranslateMDISysAccel,'TranslateMDISysAccel',\
       TranslateMessage,'TranslateMessage',\
       UnhookWinEvent,'UnhookWinEvent',\
       UnhookWindowsHook,'UnhookWindowsHook',\
       UnhookWindowsHookEx,'UnhookWindowsHookEx',\
       UnionRect,'UnionRect',\
       UnloadKeyboardLayout,'UnloadKeyboardLayout',\
       UnpackDDElParam,'UnpackDDElParam',\
       UnregisterClassA,'UnregisterClassA',\
       UnregisterClassW,'UnregisterClassW',\
       UnregisterDeviceNotification,'UnregisterDeviceNotification',\
       UnregisterHotKey,'UnregisterHotKey',\
       UpdateWindow,'UpdateWindow',\
       UserHandleGrantAccess,'UserHandleGrantAccess',\
       ValidateRect,'ValidateRect',\
       ValidateRgn,'ValidateRgn',\
       VkKeyScanA,'VkKeyScanA',\
       VkKeyScanW,'VkKeyScanW',\
       VkKeyScanExA,'VkKeyScanExA',\
       VkKeyScanExW,'VkKeyScanExW',\
       WINNLSEnableIME,'WINNLSEnableIME',\
       WINNLSGetEnableStatus,'WINNLSGetEnableStatus',\
       WINNLSGetIMEHotkey,'WINNLSGetIMEHotkey',\
       WaitForInputIdle,'WaitForInputIdle',\
       WaitMessage,'WaitMessage',\
       WinHelpA,'WinHelpA',\
       WinHelpW,'WinHelpW',\
       WindowFromDC,'WindowFromDC',\
       WindowFromPoint,'WindowFromPoint',\
       keybd_event,'keybd_event',\
       mouse_event,'mouse_event',\
       wsprintfA,'wsprintfA',\
       wsprintfW,'wsprintfW',\
       wvsprintfA,'wvsprintfA',\
       wvsprintfW,'wvsprintfW'
       
api AppendMenu,\
    BroadcastSystemMessage,\
    CallMsgFilter,\
    CallWindowProc,\
    ChangeDisplaySettings,\
    ChangeDisplaySettingsEx,\
    ChangeMenu,\
    CharLower,\
    CharLowerBuff,\
    CharNext,\
    CharNextEx,\
    CharPrev,\
    CharPrevEx,\
    CharToOem,\
    CharToOemBuff,\
    CharUpper,\
    CharUpperBuff,\
    CopyAcceleratorTable,\
    CreateAcceleratorTable,\
    CreateDesktop,\
    CreateDialogIndirectParam,\
    CreateDialogParam,\
    CreateMDIWindow,\
    CreateWindowEx,\
    CreateWindowStation,\
    DdeCreateStringHandle,\
    DdeInitialize,\
    DdeQueryString,\
    DefDlgProc,\
    DefFrameProc,\
    DefMDIChildProc,\
    DefWindowProc,\
    DialogBoxIndirectParam,\
    DialogBoxParam,\
    DispatchMessage,\
    DlgDirList,\
    DlgDirListComboBox,\
    DlgDirSelectComboBoxEx,\
    DlgDirSelectEx,\
    DrawState,\
    DrawText,\
    DrawTextEx,\
    EnumDesktops,\
    EnumDisplaySettings,\
    EnumDisplaySettingsEx,\
    EnumProps,\
    EnumPropsEx,\
    EnumWindowStations,\
    FindWindow,\
    FindWindowEx,\
    GetAltTabInfo,\
    GetClassInfo,\
    GetClassInfoEx,\
    GetClassLong,\
    GetClassName,\
    GetClipboardFormatName,\
    GetClipboardSequenceNumber,\
    GetDlgItemText,\
    GetKeyNameText,\
    GetKeyboardLayoutName,\
    GetMenuItemInfo,\
    GetMenuString,\
    GetMessage,\
    GetMonitorInfo,\
    GetProp,\
    GetTabbedTextExtent,\
    GetUserObjectInformation,\
    GetWindowLong,\
    GetWindowModuleFileName,\
    GetWindowText,\
    GetWindowTextLength,\
    GrayString,\
    IMPGetIME,\
    IMPQueryIME,\
    IMPSetIME,\
    InsertMenu,\
    InsertMenuItem,\
    IsCharAlpha,\
    IsCharAlphaNumeric,\
    IsCharLower,\
    IsCharUpper,\
    IsDialogMessage,\
    LoadAccelerators,\
    LoadBitmap,\
    LoadCursor,\
    LoadCursorFromFile,\
    LoadIcon,\
    LoadImage,\
    LoadKeyboardLayout,\
    LoadMenu,\
    LoadMenuIndirect,\
    LoadString,\
    MapVirtualKey,\
    MapVirtualKeyEx,\
    MessageBox,\
    MessageBoxEx,\
    MessageBoxIndirect,\
    ModifyMenu,\
    OemToChar,\
    OemToCharBuff,\
    OpenDesktop,\
    OpenWindowStation,\
    PeekMessage,\
    PostMessage,\
    PostThreadMessage,\
    RealGetWindowClass,\
    RegisterClass,\
    RegisterClassEx,\
    RegisterClipboardFormat,\
    RegisterDeviceNotification,\
    RegisterWindowMessage,\
    RemoveProp,\
    SendDlgItemMessage,\
    SendIMEMessageEx,\
    SendMessage,\
    SendMessageCallback,\
    SendMessageTimeout,\
    SendNotifyMessage,\
    SetClassLong,\
    SetDlgItemText,\
    SetMenuItemInfo,\
    SetProp,\
    SetUserObjectInformation,\
    SetWindowLong,\
    SetWindowText,\
    SetWindowsHook,\
    SetWindowsHookEx,\
    SystemParametersInfo,\
    TabbedTextOut,\
    TranslateAccelerator,\
    UnregisterClass,\
    VkKeyScan,\
    VkKeyScanEx,\
    WinHelp,\
    wsprintf,\
    wvsprintf


; GDI32 API calls

import gdi32,\
       AbortDoc,'AbortDoc',\
       AbortPath,'AbortPath',\
       AddFontMemResourceEx,'AddFontMemResourceEx',\
       AddFontResourceA,'AddFontResourceA',\
       AddFontResourceW,'AddFontResourceW',\
       AddFontResourceExA,'AddFontResourceExA',\
       AddFontResourceExW,'AddFontResourceExW',\
       AngleArc,'AngleArc',\
       AnimatePalette,'AnimatePalette',\
       Arc,'Arc',\
       ArcTo,'ArcTo',\
       BeginPath,'BeginPath',\
       BitBlt,'BitBlt',\
       CancelDC,'CancelDC',\
       CheckColorsInGamut,'CheckColorsInGamut',\
       ChoosePixelFormat,'ChoosePixelFormat',\
       Chord,'Chord',\
       CloseEnhMetaFile,'CloseEnhMetaFile',\
       CloseFigure,'CloseFigure',\
       CloseMetaFile,'CloseMetaFile',\
       ColorCorrectPalette,'ColorCorrectPalette',\
       ColorMatchToTarget,'ColorMatchToTarget',\
       CombineRgn,'CombineRgn',\
       CombineTransform,'CombineTransform',\
       CopyEnhMetaFileA,'CopyEnhMetaFileA',\
       CopyEnhMetaFileW,'CopyEnhMetaFileW',\
       CopyMetaFileA,'CopyMetaFileA',\
       CopyMetaFileW,'CopyMetaFileW',\
       CreateBitmap,'CreateBitmap',\
       CreateBitmapIndirect,'CreateBitmapIndirect',\
       CreateBrushIndirect,'CreateBrushIndirect',\
       CreateColorSpaceA,'CreateColorSpaceA',\
       CreateColorSpaceW,'CreateColorSpaceW',\
       CreateCompatibleBitmap,'CreateCompatibleBitmap',\
       CreateCompatibleDC,'CreateCompatibleDC',\
       CreateDCA,'CreateDCA',\
       CreateDCW,'CreateDCW',\
       CreateDIBPatternBrush,'CreateDIBPatternBrush',\
       CreateDIBPatternBrushPt,'CreateDIBPatternBrushPt',\
       CreateDIBSection,'CreateDIBSection',\
       CreateDIBitmap,'CreateDIBitmap',\
       CreateDiscardableBitmap,'CreateDiscardableBitmap',\
       CreateEllipticRgn,'CreateEllipticRgn',\
       CreateEllipticRgnIndirect,'CreateEllipticRgnIndirect',\
       CreateEnhMetaFileA,'CreateEnhMetaFileA',\
       CreateEnhMetaFileW,'CreateEnhMetaFileW',\
       CreateFontA,'CreateFontA',\
       CreateFontW,'CreateFontW',\
       CreateFontIndirectA,'CreateFontIndirectA',\
       CreateFontIndirectW,'CreateFontIndirectW',\
       CreateFontIndirectExA,'CreateFontIndirectExA',\
       CreateFontIndirectExW,'CreateFontIndirectExW',\
       CreateHalftonePalette,'CreateHalftonePalette',\
       CreateHatchBrush,'CreateHatchBrush',\
       CreateICA,'CreateICA',\
       CreateICW,'CreateICW',\
       CreateMetaFileA,'CreateMetaFileA',\
       CreateMetaFileW,'CreateMetaFileW',\
       CreatePalette,'CreatePalette',\
       CreatePatternBrush,'CreatePatternBrush',\
       CreatePen,'CreatePen',\
       CreatePenIndirect,'CreatePenIndirect',\
       CreatePolyPolygonRgn,'CreatePolyPolygonRgn',\
       CreatePolygonRgn,'CreatePolygonRgn',\
       CreateRectRgn,'CreateRectRgn',\
       CreateRectRgnIndirect,'CreateRectRgnIndirect',\
       CreateRoundRectRgn,'CreateRoundRectRgn',\
       CreateScalableFontResourceA,'CreateScalableFontResourceA',\
       CreateScalableFontResourceW,'CreateScalableFontResourceW',\
       CreateSolidBrush,'CreateSolidBrush',\
       DPtoLP,'DPtoLP',\
       DeleteColorSpace,'DeleteColorSpace',\
       DeleteDC,'DeleteDC',\
       DeleteEnhMetaFile,'DeleteEnhMetaFile',\
       DeleteMetaFile,'DeleteMetaFile',\
       DeleteObject,'DeleteObject',\
       DescribePixelFormat,'DescribePixelFormat',\
       DeviceCapabilitiesExA,'DeviceCapabilitiesExA',\
       DeviceCapabilitiesExW,'DeviceCapabilitiesExW',\
       DrawEscape,'DrawEscape',\
       Ellipse,'Ellipse',\
       EnableEUDC,'EnableEUDC',\
       EndDoc,'EndDoc',\
       EndPage,'EndPage',\
       EndPath,'EndPath',\
       EnumEnhMetaFile,'EnumEnhMetaFile',\
       EnumFontFamiliesA,'EnumFontFamiliesA',\
       EnumFontFamiliesW,'EnumFontFamiliesW',\
       EnumFontFamiliesExA,'EnumFontFamiliesExA',\
       EnumFontFamiliesExW,'EnumFontFamiliesExW',\
       EnumFontsA,'EnumFontsA',\
       EnumFontsW,'EnumFontsW',\
       EnumICMProfilesA,'EnumICMProfilesA',\
       EnumICMProfilesW,'EnumICMProfilesW',\
       EnumMetaFile,'EnumMetaFile',\
       EnumObjects,'EnumObjects',\
       EqualRgn,'EqualRgn',\
       Escape,'Escape',\
       ExcludeClipRect,'ExcludeClipRect',\
       ExtCreatePen,'ExtCreatePen',\
       ExtCreateRegion,'ExtCreateRegion',\
       ExtEscape,'ExtEscape',\
       ExtFloodFill,'ExtFloodFill',\
       ExtSelectClipRgn,'ExtSelectClipRgn',\
       ExtTextOutA,'ExtTextOutA',\
       ExtTextOutW,'ExtTextOutW',\
       FillPath,'FillPath',\
       FillRgn,'FillRgn',\
       FixBrushOrgEx,'FixBrushOrgEx',\
       FlattenPath,'FlattenPath',\
       FloodFill,'FloodFill',\
       FrameRgn,'FrameRgn',\
       GdiComment,'GdiComment',\
       GdiDeleteSpoolFileHandle,'GdiDeleteSpoolFileHandle',\
       GdiEndDocEMF,'GdiEndDocEMF',\
       GdiEndPageEMF,'GdiEndPageEMF',\
       GdiFlush,'GdiFlush',\
       GdiGetBatchLimit,'GdiGetBatchLimit',\
       GdiGetDC,'GdiGetDC',\
       GdiGetDevmodeForPage,'GdiGetDevmodeForPage',\
       GdiGetPageCount,'GdiGetPageCount',\
       GdiGetPageHandle,'GdiGetPageHandle',\
       GdiGetSpoolFileHandle,'GdiGetSpoolFileHandle',\
       GdiPlayDCScript,'GdiPlayDCScript',\
       GdiPlayEMF,'GdiPlayEMF',\
       GdiPlayJournal,'GdiPlayJournal',\
       GdiPlayPageEMF,'GdiPlayPageEMF',\
       GdiPlayPrivatePageEMF,'GdiPlayPrivatePageEMF',\
       GdiPlayScript,'GdiPlayScript',\
       GdiResetDCEMF,'GdiResetDCEMF',\
       GdiSetBatchLimit,'GdiSetBatchLimit',\
       GdiStartDocEMF,'GdiStartDocEMF',\
       GdiStartPageEMF,'GdiStartPageEMF',\
       GetArcDirection,'GetArcDirection',\
       GetAspectRatioFilterEx,'GetAspectRatioFilterEx',\
       GetBitmapBits,'GetBitmapBits',\
       GetBitmapDimensionEx,'GetBitmapDimensionEx',\
       GetBkColor,'GetBkColor',\
       GetBkMode,'GetBkMode',\
       GetBoundsRect,'GetBoundsRect',\
       GetBrushOrgEx,'GetBrushOrgEx',\
       GetCharABCWidthsA,'GetCharABCWidthsA',\
       GetCharABCWidthsW,'GetCharABCWidthsW',\
       GetCharABCWidthsFloatA,'GetCharABCWidthsFloatA',\
       GetCharABCWidthsFloatW,'GetCharABCWidthsFloatW',\
       GetCharABCWidthsI,'GetCharABCWidthsI',\
       GetCharWidth32A,'GetCharWidth32A',\
       GetCharWidth32W,'GetCharWidth32W',\
       GetCharWidthA,'GetCharWidthA',\
       GetCharWidthW,'GetCharWidthW',\
       GetCharWidthFloatA,'GetCharWidthFloatA',\
       GetCharWidthFloatW,'GetCharWidthFloatW',\
       GetCharWidthI,'GetCharWidthI',\
       GetCharacterPlacementA,'GetCharacterPlacementA',\
       GetCharacterPlacementW,'GetCharacterPlacementW',\
       GetClipBox,'GetClipBox',\
       GetClipRgn,'GetClipRgn',\
       GetColorAdjustment,'GetColorAdjustment',\
       GetColorSpace,'GetColorSpace',\
       GetCurrentObject,'GetCurrentObject',\
       GetCurrentPositionEx,'GetCurrentPositionEx',\
       GetDCBrushColor,'GetDCBrushColor',\
       GetDCOrgEx,'GetDCOrgEx',\
       GetDCPenColor,'GetDCPenColor',\
       GetDIBColorTable,'GetDIBColorTable',\
       GetDIBits,'GetDIBits',\
       GetDeviceCaps,'GetDeviceCaps',\
       GetDeviceGammaRamp,'GetDeviceGammaRamp',\
       GetEnhMetaFileA,'GetEnhMetaFileA',\
       GetEnhMetaFileW,'GetEnhMetaFileW',\
       GetEnhMetaFileBits,'GetEnhMetaFileBits',\
       GetEnhMetaFileDescriptionA,'GetEnhMetaFileDescriptionA',\
       GetEnhMetaFileDescriptionW,'GetEnhMetaFileDescriptionW',\
       GetEnhMetaFileHeader,'GetEnhMetaFileHeader',\
       GetEnhMetaFilePaletteEntries,'GetEnhMetaFilePaletteEntries',\
       GetEnhMetaFilePixelFormat,'GetEnhMetaFilePixelFormat',\
       GetFontAssocStatus,'GetFontAssocStatus',\
       GetFontData,'GetFontData',\
       GetFontLanguageInfo,'GetFontLanguageInfo',\
       GetFontUnicodeRanges,'GetFontUnicodeRanges',\
       GetGlyphIndicesA,'GetGlyphIndicesA',\
       GetGlyphIndicesW,'GetGlyphIndicesW',\
       GetGlyphOutlineA,'GetGlyphOutlineA',\
       GetGlyphOutlineW,'GetGlyphOutlineW',\
       GetGraphicsMode,'GetGraphicsMode',\
       GetICMProfileA,'GetICMProfileA',\
       GetICMProfileW,'GetICMProfileW',\
       GetKerningPairsA,'GetKerningPairsA',\
       GetKerningPairsW,'GetKerningPairsW',\
       GetLogColorSpaceA,'GetLogColorSpaceA',\
       GetLogColorSpaceW,'GetLogColorSpaceW',\
       GetMapMode,'GetMapMode',\
       GetMetaFileA,'GetMetaFileA',\
       GetMetaFileW,'GetMetaFileW',\
       GetMetaFileBitsEx,'GetMetaFileBitsEx',\
       GetMetaRgn,'GetMetaRgn',\
       GetMiterLimit,'GetMiterLimit',\
       GetNearestColor,'GetNearestColor',\
       GetNearestPaletteIndex,'GetNearestPaletteIndex',\
       GetObjectA,'GetObjectA',\
       GetObjectW,'GetObjectW',\
       GetObjectType,'GetObjectType',\
       GetOutlineTextMetricsA,'GetOutlineTextMetricsA',\
       GetOutlineTextMetricsW,'GetOutlineTextMetricsW',\
       GetPaletteEntries,'GetPaletteEntries',\
       GetPath,'GetPath',\
       GetPixel,'GetPixel',\
       GetPixelFormat,'GetPixelFormat',\
       GetPolyFillMode,'GetPolyFillMode',\
       GetROP2,'GetROP2',\
       GetRandomRgn,'GetRandomRgn',\
       GetRasterizerCaps,'GetRasterizerCaps',\
       GetRegionData,'GetRegionData',\
       GetRelAbs,'GetRelAbs',\
       GetRgnBox,'GetRgnBox',\
       GetStockObject,'GetStockObject',\
       GetStretchBltMode,'GetStretchBltMode',\
       GetSystemPaletteEntries,'GetSystemPaletteEntries',\
       GetSystemPaletteUse,'GetSystemPaletteUse',\
       GetTextAlign,'GetTextAlign',\
       GetTextCharacterExtra,'GetTextCharacterExtra',\
       GetTextCharset,'GetTextCharset',\
       GetTextCharsetInfo,'GetTextCharsetInfo',\
       GetTextColor,'GetTextColor',\
       GetTextExtentExPointA,'GetTextExtentExPointA',\
       GetTextExtentExPointW,'GetTextExtentExPointW',\
       GetTextExtentExPointI,'GetTextExtentExPointI',\
       GetTextExtentPoint32A,'GetTextExtentPoint32A',\
       GetTextExtentPoint32W,'GetTextExtentPoint32W',\
       GetTextExtentPointA,'GetTextExtentPointA',\
       GetTextExtentPointW,'GetTextExtentPointW',\
       GetTextExtentPointI,'GetTextExtentPointI',\
       GetTextFaceA,'GetTextFaceA',\
       GetTextFaceW,'GetTextFaceW',\
       GetTextMetricsA,'GetTextMetricsA',\
       GetTextMetricsW,'GetTextMetricsW',\
       GetViewportExtEx,'GetViewportExtEx',\
       GetViewportOrgEx,'GetViewportOrgEx',\
       GetWinMetaFileBits,'GetWinMetaFileBits',\
       GetWindowExtEx,'GetWindowExtEx',\
       GetWindowOrgEx,'GetWindowOrgEx',\
       GetWorldTransform,'GetWorldTransform',\
       IntersectClipRect,'IntersectClipRect',\
       InvertRgn,'InvertRgn',\
       LPtoDP,'LPtoDP',\
       LineDDA,'LineDDA',\
       LineDDW,'LineDDW',\
       LineTo,'LineTo',\
       MaskBlt,'MaskBlt',\
       ModifyWorldTransform,'ModifyWorldTransform',\
       MoveToEx,'MoveToEx',\
       OffsetClipRgn,'OffsetClipRgn',\
       OffsetRgn,'OffsetRgn',\
       OffsetViewportOrgEx,'OffsetViewportOrgEx',\
       OffsetWindowOrgEx,'OffsetWindowOrgEx',\
       PaintRgn,'PaintRgn',\
       PatBlt,'PatBlt',\
       PathToRegion,'PathToRegion',\
       Pie,'Pie',\
       PlayEnhMetaFile,'PlayEnhMetaFile',\
       PlayEnhMetaFileRecord,'PlayEnhMetaFileRecord',\
       PlayMetaFile,'PlayMetaFile',\
       PlayMetaFileRecord,'PlayMetaFileRecord',\
       PlgBlt,'PlgBlt',\
       PolyBezier,'PolyBezier',\
       PolyBezierTo,'PolyBezierTo',\
       PolyDraw,'PolyDraw',\
       PolyPatBlt,'PolyPatBlt',\
       PolyPolygon,'PolyPolygon',\
       PolyPolyline,'PolyPolyline',\
       PolyTextOutA,'PolyTextOutA',\
       PolyTextOutW,'PolyTextOutW',\
       Polygon,'Polygon',\
       Polyline,'Polyline',\
       PolylineTo,'PolylineTo',\
       PtInRegion,'PtInRegion',\
       PtVisible,'PtVisible',\
       RealizePalette,'RealizePalette',\
       RectInRegion,'RectInRegion',\
       RectVisible,'RectVisible',\
       Rectangle,'Rectangle',\
       RemoveFontMemResourceEx,'RemoveFontMemResourceEx',\
       RemoveFontResourceA,'RemoveFontResourceA',\
       RemoveFontResourceW,'RemoveFontResourceW',\
       RemoveFontResourceExA,'RemoveFontResourceExA',\
       RemoveFontResourceExW,'RemoveFontResourceExW',\
       ResetDCA,'ResetDCA',\
       ResetDCW,'ResetDCW',\
       ResizePalette,'ResizePalette',\
       RestoreDC,'RestoreDC',\
       RoundRect,'RoundRect',\
       SaveDC,'SaveDC',\
       ScaleViewportExtEx,'ScaleViewportExtEx',\
       ScaleWindowExtEx,'ScaleWindowExtEx',\
       SelectBrushLocal,'SelectBrushLocal',\
       SelectClipPath,'SelectClipPath',\
       SelectClipRgn,'SelectClipRgn',\
       SelectFontLocal,'SelectFontLocal',\
       SelectObject,'SelectObject',\
       SelectPalette,'SelectPalette',\
       SetAbortProc,'SetAbortProc',\
       SetArcDirection,'SetArcDirection',\
       SetBitmapBits,'SetBitmapBits',\
       SetBitmapDimensionEx,'SetBitmapDimensionEx',\
       SetBkColor,'SetBkColor',\
       SetBkMode,'SetBkMode',\
       SetBoundsRect,'SetBoundsRect',\
       SetBrushOrgEx,'SetBrushOrgEx',\
       SetColorAdjustment,'SetColorAdjustment',\
       SetColorSpace,'SetColorSpace',\
       SetDCBrushColor,'SetDCBrushColor',\
       SetDCPenColor,'SetDCPenColor',\
       SetDIBColorTable,'SetDIBColorTable',\
       SetDIBits,'SetDIBits',\
       SetDIBitsToDevice,'SetDIBitsToDevice',\
       SetDeviceGammaRamp,'SetDeviceGammaRamp',\
       SetEnhMetaFileBits,'SetEnhMetaFileBits',\
       SetFontEnumeration,'SetFontEnumeration',\
       SetGraphicsMode,'SetGraphicsMode',\
       SetICMMode,'SetICMMode',\
       SetICMProfileA,'SetICMProfileA',\
       SetICMProfileW,'SetICMProfileW',\
       SetMagicColors,'SetMagicColors',\
       SetMapMode,'SetMapMode',\
       SetMapperFlags,'SetMapperFlags',\
       SetMetaFileBitsEx,'SetMetaFileBitsEx',\
       SetMetaRgn,'SetMetaRgn',\
       SetMiterLimit,'SetMiterLimit',\
       SetPaletteEntries,'SetPaletteEntries',\
       SetPixel,'SetPixel',\
       SetPixelFormat,'SetPixelFormat',\
       SetPixelV,'SetPixelV',\
       SetPolyFillMode,'SetPolyFillMode',\
       SetROP2,'SetROP2',\
       SetRectRgn,'SetRectRgn',\
       SetRelAbs,'SetRelAbs',\
       SetStretchBltMode,'SetStretchBltMode',\
       SetSystemPaletteUse,'SetSystemPaletteUse',\
       SetTextAlign,'SetTextAlign',\
       SetTextCharacterExtra,'SetTextCharacterExtra',\
       SetTextColor,'SetTextColor',\
       SetTextJustification,'SetTextJustification',\
       SetViewportExtEx,'SetViewportExtEx',\
       SetViewportOrgEx,'SetViewportOrgEx',\
       SetWinMetaFileBits,'SetWinMetaFileBits',\
       SetWindowExtEx,'SetWindowExtEx',\
       SetWindowOrgEx,'SetWindowOrgEx',\
       SetWorldTransform,'SetWorldTransform',\
       StartDocA,'StartDocA',\
       StartDocW,'StartDocW',\
       StartPage,'StartPage',\
       StretchBlt,'StretchBlt',\
       StretchDIBits,'StretchDIBits',\
       StrokeAndFillPath,'StrokeAndFillPath',\
       StrokePath,'StrokePath',\
       SwapBuffers,'SwapBuffers',\
       TextOutA,'TextOutA',\
       TextOutW,'TextOutW',\
       TranslateCharsetInfo,'TranslateCharsetInfo',\
       UnrealizeObject,'UnrealizeObject',\
       UpdateColors,'UpdateColors',\
       UpdateICMRegKeyA,'UpdateICMRegKeyA',\
       UpdateICMRegKeyW,'UpdateICMRegKeyW',\
       WidenPath,'WidenPath',\
       gdiPlaySpoolStream,'gdiPlaySpoolStream'

api AddFontResource,\
    AddFontResourceEx,\
    CopyEnhMetaFile,\
    CopyMetaFile,\
    CreateColorSpace,\
    CreateDC,\
    CreateEnhMetaFile,\
    CreateFont,\
    CreateFontIndirect,\
    CreateFontIndirectEx,\
    CreateIC,\
    CreateMetaFile,\
    CreateScalableFontResource,\
    DeviceCapabilitiesEx,\
    EnumFontFamilies,\
    EnumFontFamiliesEx,\
    EnumFonts,\
    EnumICMProfiles,\
    ExtTextOut,\
    GetCharABCWidths,\
    GetCharABCWidthsFloat,\
    GetCharWidth32,\
    GetCharWidth,\
    GetCharWidthFloat,\
    GetCharacterPlacement,\
    GetEnhMetaFile,\
    GetEnhMetaFileDescription,\
    GetGlyphIndices,\
    GetGlyphOutline,\
    GetICMProfile,\
    GetKerningPairs,\
    GetLogColorSpace,\
    GetMetaFile,\
    GetObject,\
    GetOutlineTextMetrics,\
    GetTextExtentExPoint,\
    GetTextExtentPoint32,\
    GetTextExtentPoint,\
    GetTextFace,\
    GetTextMetrics,\
    LineDD,\
    PolyTextOut,\
    RemoveFontResource,\
    RemoveFontResourceEx,\
    ResetDC,\
    SetICMProfile,\
    StartDoc,\
    TextOut,\
    UpdateICMRegKey


; ADVAPI32 API calls

import advapi32,\
       AbortSystemShutdownA,'AbortSystemShutdownA',\
       AbortSystemShutdownW,'AbortSystemShutdownW',\
       AccessCheck,'AccessCheck',\
       AccessCheckAndAuditAlarmA,'AccessCheckAndAuditAlarmA',\
       AccessCheckAndAuditAlarmW,'AccessCheckAndAuditAlarmW',\
       AccessCheckByType,'AccessCheckByType',\
       AccessCheckByTypeAndAuditAlarmA,'AccessCheckByTypeAndAuditAlarmA',\
       AccessCheckByTypeAndAuditAlarmW,'AccessCheckByTypeAndAuditAlarmW',\
       AccessCheckByTypeResultList,'AccessCheckByTypeResultList',\
       AccessCheckByTypeResultListAndAuditAlarmA,'AccessCheckByTypeResultListAndAuditAlarmA',\
       AccessCheckByTypeResultListAndAuditAlarmW,'AccessCheckByTypeResultListAndAuditAlarmW',\
       AddAccessAllowedAce,'AddAccessAllowedAce',\
       AddAccessAllowedAceEx,'AddAccessAllowedAceEx',\
       AddAccessAllowedObjectAce,'AddAccessAllowedObjectAce',\
       AddAccessDeniedAce,'AddAccessDeniedAce',\
       AddAccessDeniedAceEx,'AddAccessDeniedAceEx',\
       AddAccessDeniedObjectAce,'AddAccessDeniedObjectAce',\
       AddAce,'AddAce',\
       AddAuditAccessAce,'AddAuditAccessAce',\
       AddAuditAccessAceEx,'AddAuditAccessAceEx',\
       AddAuditAccessObjectAce,'AddAuditAccessObjectAce',\
       AdjustTokenGroups,'AdjustTokenGroups',\
       AdjustTokenPrivileges,'AdjustTokenPrivileges',\
       AllocateAndInitializeSid,'AllocateAndInitializeSid',\
       AllocateLocallyUniqueId,'AllocateLocallyUniqueId',\
       AreAllAccessesGranted,'AreAllAccessesGranted',\
       AreAnyAccessesGranted,'AreAnyAccessesGranted',\
       BackupEventLogA,'BackupEventLogA',\
       BackupEventLogW,'BackupEventLogW',\
       BuildExplicitAccessWithNameA,'BuildExplicitAccessWithNameA',\
       BuildExplicitAccessWithNameW,'BuildExplicitAccessWithNameW',\
       BuildImpersonateExplicitAccessWithNameA,'BuildImpersonateExplicitAccessWithNameA',\
       BuildImpersonateExplicitAccessWithNameW,'BuildImpersonateExplicitAccessWithNameW',\
       BuildImpersonateTrusteeA,'BuildImpersonateTrusteeA',\
       BuildImpersonateTrusteeW,'BuildImpersonateTrusteeW',\
       BuildSecurityDescriptorA,'BuildSecurityDescriptorA',\
       BuildSecurityDescriptorW,'BuildSecurityDescriptorW',\
       BuildTrusteeWithNameA,'BuildTrusteeWithNameA',\
       BuildTrusteeWithNameW,'BuildTrusteeWithNameW',\
       BuildTrusteeWithSidA,'BuildTrusteeWithSidA',\
       BuildTrusteeWithSidW,'BuildTrusteeWithSidW',\
       CancelOverlappedAccess,'CancelOverlappedAccess',\
       ChangeServiceConfig2A,'ChangeServiceConfig2A',\
       ChangeServiceConfig2W,'ChangeServiceConfig2W',\
       ChangeServiceConfigA,'ChangeServiceConfigA',\
       ChangeServiceConfigW,'ChangeServiceConfigW',\
       ClearEventLogA,'ClearEventLogA',\
       ClearEventLogW,'ClearEventLogW',\
       CloseEventLog,'CloseEventLog',\
       CloseRaw,'CloseRaw',\
       CloseServiceHandle,'CloseServiceHandle',\
       ControlService,'ControlService',\
       ConvertAccessToSecurityDescriptorA,'ConvertAccessToSecurityDescriptorA',\
       ConvertAccessToSecurityDescriptorW,'ConvertAccessToSecurityDescriptorW',\
       ConvertSecurityDescriptorToAccessA,'ConvertSecurityDescriptorToAccessA',\
       ConvertSecurityDescriptorToAccessW,'ConvertSecurityDescriptorToAccessW',\
       ConvertSecurityDescriptorToAccessNamedA,'ConvertSecurityDescriptorToAccessNamedA',\
       ConvertSecurityDescriptorToAccessNamedW,'ConvertSecurityDescriptorToAccessNamedW',\
       ConvertToAutoInheritPrivateObjectSecurity,'ConvertToAutoInheritPrivateObjectSecurity',\
       CopySid,'CopySid',\
       CreatePrivateObjectSecurity,'CreatePrivateObjectSecurity',\
       CreatePrivateObjectSecurityEx,'CreatePrivateObjectSecurityEx',\
       CreateProcessAsUserA,'CreateProcessAsUserA',\
       CreateProcessAsUserW,'CreateProcessAsUserW',\
       CreateRestrictedToken,'CreateRestrictedToken',\
       CreateServiceA,'CreateServiceA',\
       CreateServiceW,'CreateServiceW',\
       CryptAcquireContextA,'CryptAcquireContextA',\
       CryptAcquireContextW,'CryptAcquireContextW',\
       CryptContextAddRef,'CryptContextAddRef',\
       CryptCreateHash,'CryptCreateHash',\
       CryptDecrypt,'CryptDecrypt',\
       CryptDeriveKey,'CryptDeriveKey',\
       CryptDestroyHash,'CryptDestroyHash',\
       CryptDestroyKey,'CryptDestroyKey',\
       CryptDuplicateHash,'CryptDuplicateHash',\
       CryptDuplicateKey,'CryptDuplicateKey',\
       CryptEncrypt,'CryptEncrypt',\
       CryptEnumProviderTypesA,'CryptEnumProviderTypesA',\
       CryptEnumProviderTypesW,'CryptEnumProviderTypesW',\
       CryptEnumProvidersA,'CryptEnumProvidersA',\
       CryptEnumProvidersW,'CryptEnumProvidersW',\
       CryptExportKey,'CryptExportKey',\
       CryptGenKey,'CryptGenKey',\
       CryptGenRandom,'CryptGenRandom',\
       CryptGetDefaultProviderA,'CryptGetDefaultProviderA',\
       CryptGetDefaultProviderW,'CryptGetDefaultProviderW',\
       CryptGetHashParam,'CryptGetHashParam',\
       CryptGetKeyParam,'CryptGetKeyParam',\
       CryptGetProvParam,'CryptGetProvParam',\
       CryptGetUserKey,'CryptGetUserKey',\
       CryptHashData,'CryptHashData',\
       CryptHashSessionKey,'CryptHashSessionKey',\
       CryptImportKey,'CryptImportKey',\
       CryptReleaseContext,'CryptReleaseContext',\
       CryptSetHashParam,'CryptSetHashParam',\
       CryptSetKeyParam,'CryptSetKeyParam',\
       CryptSetProvParam,'CryptSetProvParam',\
       CryptSetProviderA,'CryptSetProviderA',\
       CryptSetProviderW,'CryptSetProviderW',\
       CryptSetProviderExA,'CryptSetProviderExA',\
       CryptSetProviderExW,'CryptSetProviderExW',\
       CryptSignHashA,'CryptSignHashA',\
       CryptSignHashW,'CryptSignHashW',\
       CryptVerifySignatureA,'CryptVerifySignatureA',\
       CryptVerifySignatureW,'CryptVerifySignatureW',\
       DecryptFileA,'DecryptFileA',\
       DecryptFileW,'DecryptFileW',\
       DeleteAce,'DeleteAce',\
       DeleteService,'DeleteService',\
       DeregisterEventSource,'DeregisterEventSource',\
       DestroyPrivateObjectSecurity,'DestroyPrivateObjectSecurity',\
       DuplicateToken,'DuplicateToken',\
       DuplicateTokenEx,'DuplicateTokenEx',\
       ElfBackupEventLogFileA,'ElfBackupEventLogFileA',\
       ElfBackupEventLogFileW,'ElfBackupEventLogFileW',\
       ElfChangeNotify,'ElfChangeNotify',\
       ElfClearEventLogFileA,'ElfClearEventLogFileA',\
       ElfClearEventLogFileW,'ElfClearEventLogFileW',\
       ElfCloseEventLog,'ElfCloseEventLog',\
       ElfDeregisterEventSource,'ElfDeregisterEventSource',\
       ElfNumberOfRecords,'ElfNumberOfRecords',\
       ElfOldestRecord,'ElfOldestRecord',\
       ElfOpenBackupEventLogA,'ElfOpenBackupEventLogA',\
       ElfOpenBackupEventLogW,'ElfOpenBackupEventLogW',\
       ElfOpenEventLogA,'ElfOpenEventLogA',\
       ElfOpenEventLogW,'ElfOpenEventLogW',\
       ElfReadEventLogA,'ElfReadEventLogA',\
       ElfReadEventLogW,'ElfReadEventLogW',\
       ElfRegisterEventSourceA,'ElfRegisterEventSourceA',\
       ElfRegisterEventSourceW,'ElfRegisterEventSourceW',\
       ElfReportEventA,'ElfReportEventA',\
       ElfReportEventW,'ElfReportEventW',\
       EncryptFileA,'EncryptFileA',\
       EncryptFileW,'EncryptFileW',\
       EnumDependentServicesA,'EnumDependentServicesA',\
       EnumDependentServicesW,'EnumDependentServicesW',\
       EnumServicesStatusA,'EnumServicesStatusA',\
       EnumServicesStatusW,'EnumServicesStatusW',\
       EqualPrefixSid,'EqualPrefixSid',\
       EqualSid,'EqualSid',\
       FindFirstFreeAce,'FindFirstFreeAce',\
       FreeSid,'FreeSid',\
       GetAccessPermissionsForObjectA,'GetAccessPermissionsForObjectA',\
       GetAccessPermissionsForObjectW,'GetAccessPermissionsForObjectW',\
       GetAce,'GetAce',\
       GetAclInformation,'GetAclInformation',\
       GetAuditedPermissionsFromAclA,'GetAuditedPermissionsFromAclA',\
       GetAuditedPermissionsFromAclW,'GetAuditedPermissionsFromAclW',\
       GetCurrentHwProfileA,'GetCurrentHwProfileA',\
       GetCurrentHwProfileW,'GetCurrentHwProfileW',\
       GetEffectiveRightsFromAclA,'GetEffectiveRightsFromAclA',\
       GetEffectiveRightsFromAclW,'GetEffectiveRightsFromAclW',\
       GetExplicitEntriesFromAclA,'GetExplicitEntriesFromAclA',\
       GetExplicitEntriesFromAclW,'GetExplicitEntriesFromAclW',\
       GetFileSecurityA,'GetFileSecurityA',\
       GetFileSecurityW,'GetFileSecurityW',\
       GetKernelObjectSecurity,'GetKernelObjectSecurity',\
       GetLengthSid,'GetLengthSid',\
       GetMultipleTrusteeA,'GetMultipleTrusteeA',\
       GetMultipleTrusteeW,'GetMultipleTrusteeW',\
       GetMultipleTrusteeOperationA,'GetMultipleTrusteeOperationA',\
       GetMultipleTrusteeOperationW,'GetMultipleTrusteeOperationW',\
       GetNamedSecurityInfoA,'GetNamedSecurityInfoA',\
       GetNamedSecurityInfoW,'GetNamedSecurityInfoW',\
       GetNamedSecurityInfoExA,'GetNamedSecurityInfoExA',\
       GetNamedSecurityInfoExW,'GetNamedSecurityInfoExW',\
       GetNumberOfEventLogRecords,'GetNumberOfEventLogRecords',\
       GetOldestEventLogRecord,'GetOldestEventLogRecord',\
       GetOverlappedAccessResults,'GetOverlappedAccessResults',\
       GetPrivateObjectSecurity,'GetPrivateObjectSecurity',\
       GetSecurityDescriptorControl,'GetSecurityDescriptorControl',\
       GetSecurityDescriptorDacl,'GetSecurityDescriptorDacl',\
       GetSecurityDescriptorGroup,'GetSecurityDescriptorGroup',\
       GetSecurityDescriptorLength,'GetSecurityDescriptorLength',\
       GetSecurityDescriptorOwner,'GetSecurityDescriptorOwner',\
       GetSecurityDescriptorSacl,'GetSecurityDescriptorSacl',\
       GetSecurityInfo,'GetSecurityInfo',\
       GetSecurityInfoExA,'GetSecurityInfoExA',\
       GetSecurityInfoExW,'GetSecurityInfoExW',\
       GetServiceDisplayNameA,'GetServiceDisplayNameA',\
       GetServiceDisplayNameW,'GetServiceDisplayNameW',\
       GetServiceKeyNameA,'GetServiceKeyNameA',\
       GetServiceKeyNameW,'GetServiceKeyNameW',\
       GetSidLengthRequiredA,'GetSidLengthRequiredA',\
       GetSidLengthRequiredW,'GetSidLengthRequiredW',\
       GetSidSubAuthority,'GetSidSubAuthority',\
       GetSidSubAuthorityCount,'GetSidSubAuthorityCount',\
       GetTokenInformation,'GetTokenInformation',\
       GetTrusteeNameA,'GetTrusteeNameA',\
       GetTrusteeNameW,'GetTrusteeNameW',\
       GetTrusteeTypeA,'GetTrusteeTypeA',\
       GetTrusteeTypeW,'GetTrusteeTypeW',\
       GetUserNameA,'GetUserNameA',\
       GetUserNameW,'GetUserNameW',\
       I_ScSetServiceBitsA,'I_ScSetServiceBitsA',\
       I_ScSetServiceBitsW,'I_ScSetServiceBitsW',\
       ImpersonateLoggedOnUser,'ImpersonateLoggedOnUser',\
       ImpersonateNamedPipeClient,'ImpersonateNamedPipeClient',\
       ImpersonateSelf,'ImpersonateSelf',\
       InitializeAcl,'InitializeAcl',\
       InitializeSecurityDescriptor,'InitializeSecurityDescriptor',\
       InitializeSid,'InitializeSid',\
       InitiateSystemShutdownA,'InitiateSystemShutdownA',\
       InitiateSystemShutdownW,'InitiateSystemShutdownW',\
       IsTextUnicode,'IsTextUnicode',\
       IsTokenRestricted,'IsTokenRestricted',\
       IsValidAcl,'IsValidAcl',\
       IsValidSecurityDescriptor,'IsValidSecurityDescriptor',\
       IsValidSid,'IsValidSid',\
       LockServiceDatabase,'LockServiceDatabase',\
       LogonUserA,'LogonUserA',\
       LogonUserW,'LogonUserW',\
       LookupAccountNameA,'LookupAccountNameA',\
       LookupAccountNameW,'LookupAccountNameW',\
       LookupAccountSidA,'LookupAccountSidA',\
       LookupAccountSidW,'LookupAccountSidW',\
       LookupPrivilegeDisplayNameA,'LookupPrivilegeDisplayNameA',\
       LookupPrivilegeDisplayNameW,'LookupPrivilegeDisplayNameW',\
       LookupPrivilegeNameA,'LookupPrivilegeNameA',\
       LookupPrivilegeNameW,'LookupPrivilegeNameW',\
       LookupPrivilegeValueA,'LookupPrivilegeValueA',\
       LookupPrivilegeValueW,'LookupPrivilegeValueW',\
       LookupSecurityDescriptorPartsA,'LookupSecurityDescriptorPartsA',\
       LookupSecurityDescriptorPartsW,'LookupSecurityDescriptorPartsW',\
       LsaAddAccountRights,'LsaAddAccountRights',\
       LsaAddPrivilegesToAccount,'LsaAddPrivilegesToAccount',\
       LsaClearAuditLog,'LsaClearAuditLog',\
       LsaClose,'LsaClose',\
       LsaCreateAccount,'LsaCreateAccount',\
       LsaCreateSecret,'LsaCreateSecret',\
       LsaCreateTrustedDomain,'LsaCreateTrustedDomain',\
       LsaCreateTrustedDomainEx,'LsaCreateTrustedDomainEx',\
       LsaDelete,'LsaDelete',\
       LsaDeleteTrustedDomain,'LsaDeleteTrustedDomain',\
       LsaEnumerateAccountRights,'LsaEnumerateAccountRights',\
       LsaEnumerateAccounts,'LsaEnumerateAccounts',\
       LsaEnumerateAccountsWithUserRight,'LsaEnumerateAccountsWithUserRight',\
       LsaEnumeratePrivileges,'LsaEnumeratePrivileges',\
       LsaEnumeratePrivilegesOfAccount,'LsaEnumeratePrivilegesOfAccount',\
       LsaEnumerateTrustedDomains,'LsaEnumerateTrustedDomains',\
       LsaEnumerateTrustedDomainsEx,'LsaEnumerateTrustedDomainsEx',\
       LsaFreeMemory,'LsaFreeMemory',\
       LsaGetQuotasForAccount,'LsaGetQuotasForAccount',\
       LsaGetSystemAccessAccount,'LsaGetSystemAccessAccount',\
       LsaGetUserName,'LsaGetUserName',\
       LsaICLookupNames,'LsaICLookupNames',\
       LsaICLookupSids,'LsaICLookupSids',\
       LsaIGetTrustedDomainAuthInfoBlobs,'LsaIGetTrustedDomainAuthInfoBlobs',\
       LsaISetTrustedDomainAuthInfoBlobs,'LsaISetTrustedDomainAuthInfoBlobs',\
       LsaLookupNames,'LsaLookupNames',\
       LsaLookupPrivilegeDisplayName,'LsaLookupPrivilegeDisplayName',\
       LsaLookupPrivilegeName,'LsaLookupPrivilegeName',\
       LsaLookupPrivilegeValue,'LsaLookupPrivilegeValue',\
       LsaLookupSids,'LsaLookupSids',\
       LsaNtStatusToWinError,'LsaNtStatusToWinError',\
       LsaOpenAccount,'LsaOpenAccount',\
       LsaOpenPolicy,'LsaOpenPolicy',\
       LsaOpenSecret,'LsaOpenSecret',\
       LsaOpenTrustedDomain,'LsaOpenTrustedDomain',\
       LsaQueryDomainInformationPolicy,'LsaQueryDomainInformationPolicy',\
       LsaQueryInfoTrustedDomain,'LsaQueryInfoTrustedDomain',\
       LsaQueryInformationPolicy,'LsaQueryInformationPolicy',\
       LsaQueryLocalInformationPolicy,'LsaQueryLocalInformationPolicy',\
       LsaQuerySecret,'LsaQuerySecret',\
       LsaQuerySecurityObject,'LsaQuerySecurityObject',\
       LsaQueryTrustedDomainInfo,'LsaQueryTrustedDomainInfo',\
       LsaQueryTrustedDomainInfoByName,'LsaQueryTrustedDomainInfoByName',\
       LsaRemoveAccountRights,'LsaRemoveAccountRights',\
       LsaRemovePrivilegesFromAccount,'LsaRemovePrivilegesFromAccount',\
       LsaRetrievePrivateData,'LsaRetrievePrivateData',\
       LsaSetDomainInformationPolicy,'LsaSetDomainInformationPolicy',\
       LsaSetInformationPolicy,'LsaSetInformationPolicy',\
       LsaSetInformationTrustedDomain,'LsaSetInformationTrustedDomain',\
       LsaSetLocalInformationPolicy,'LsaSetLocalInformationPolicy',\
       LsaSetQuotasForAccount,'LsaSetQuotasForAccount',\
       LsaSetSecret,'LsaSetSecret',\
       LsaSetSecurityObject,'LsaSetSecurityObject',\
       LsaSetSystemAccessAccount,'LsaSetSystemAccessAccount',\
       LsaSetTrustedDomainInfoByName,'LsaSetTrustedDomainInfoByName',\
       LsaSetTrustedDomainInformation,'LsaSetTrustedDomainInformation',\
       LsaStorePrivateData,'LsaStorePrivateData',\
       MakeAbsoluteSD,'MakeAbsoluteSD',\
       MakeSelfRelativeSD,'MakeSelfRelativeSD',\
       MapGenericMask,'MapGenericMask',\
       NotifyBootConfigStatus,'NotifyBootConfigStatus',\
       NotifyChangeEventLog,'NotifyChangeEventLog',\
       ObjectCloseAuditAlarmA,'ObjectCloseAuditAlarmA',\
       ObjectCloseAuditAlarmW,'ObjectCloseAuditAlarmW',\
       ObjectDeleteAuditAlarmA,'ObjectDeleteAuditAlarmA',\
       ObjectDeleteAuditAlarmW,'ObjectDeleteAuditAlarmW',\
       ObjectOpenAuditAlarmA,'ObjectOpenAuditAlarmA',\
       ObjectOpenAuditAlarmW,'ObjectOpenAuditAlarmW',\
       ObjectPrivilegeAuditAlarmA,'ObjectPrivilegeAuditAlarmA',\
       ObjectPrivilegeAuditAlarmW,'ObjectPrivilegeAuditAlarmW',\
       OpenBackupEventLogA,'OpenBackupEventLogA',\
       OpenBackupEventLogW,'OpenBackupEventLogW',\
       OpenEventLogA,'OpenEventLogA',\
       OpenEventLogW,'OpenEventLogW',\
       OpenProcessToken,'OpenProcessToken',\
       OpenRawA,'OpenRawA',\
       OpenRawW,'OpenRawW',\
       OpenSCManagerA,'OpenSCManagerA',\
       OpenSCManagerW,'OpenSCManagerW',\
       OpenServiceA,'OpenServiceA',\
       OpenServiceW,'OpenServiceW',\
       OpenThreadToken,'OpenThreadToken',\
       PrivilegeCheck,'PrivilegeCheck',\
       PrivilegedServiceAuditAlarmA,'PrivilegedServiceAuditAlarmA',\
       PrivilegedServiceAuditAlarmW,'PrivilegedServiceAuditAlarmW',\
       QueryRecoveryAgentsA,'QueryRecoveryAgentsA',\
       QueryRecoveryAgentsW,'QueryRecoveryAgentsW',\
       QueryServiceConfig2A,'QueryServiceConfig2A',\
       QueryServiceConfig2W,'QueryServiceConfig2W',\
       QueryServiceConfigA,'QueryServiceConfigA',\
       QueryServiceConfigW,'QueryServiceConfigW',\
       QueryServiceLockStatusA,'QueryServiceLockStatusA',\
       QueryServiceLockStatusW,'QueryServiceLockStatusW',\
       QueryServiceObjectSecurity,'QueryServiceObjectSecurity',\
       QueryServiceStatus,'QueryServiceStatus',\
       QueryWindows31FilesMigration,'QueryWindows31FilesMigration',\
       ReadEventLogA,'ReadEventLogA',\
       ReadEventLogW,'ReadEventLogW',\
       ReadRaw,'ReadRaw',\
       RegCloseKey,'RegCloseKey',\
       RegConnectRegistryA,'RegConnectRegistryA',\
       RegConnectRegistryW,'RegConnectRegistryW',\
       RegCreateKeyA,'RegCreateKeyA',\
       RegCreateKeyW,'RegCreateKeyW',\
       RegCreateKeyExA,'RegCreateKeyExA',\
       RegCreateKeyExW,'RegCreateKeyExW',\
       RegDeleteKeyA,'RegDeleteKeyA',\
       RegDeleteKeyW,'RegDeleteKeyW',\
       RegDeleteValueA,'RegDeleteValueA',\
       RegDeleteValueW,'RegDeleteValueW',\
       RegEnumKeyA,'RegEnumKeyA',\
       RegEnumKeyW,'RegEnumKeyW',\
       RegEnumKeyExA,'RegEnumKeyExA',\
       RegEnumKeyExW,'RegEnumKeyExW',\
       RegEnumValueA,'RegEnumValueA',\
       RegEnumValueW,'RegEnumValueW',\
       RegFlushKey,'RegFlushKey',\
       RegGetKeySecurity,'RegGetKeySecurity',\
       RegLoadKeyA,'RegLoadKeyA',\
       RegLoadKeyW,'RegLoadKeyW',\
       RegNotifyChangeKeyValue,'RegNotifyChangeKeyValue',\
       RegOpenKeyA,'RegOpenKeyA',\
       RegOpenKeyW,'RegOpenKeyW',\
       RegOpenKeyExA,'RegOpenKeyExA',\
       RegOpenKeyExW,'RegOpenKeyExW',\
       RegOverridePredefKey,'RegOverridePredefKey',\
       RegQueryInfoKeyA,'RegQueryInfoKeyA',\
       RegQueryInfoKeyW,'RegQueryInfoKeyW',\
       RegQueryMultipleValuesA,'RegQueryMultipleValuesA',\
       RegQueryMultipleValuesW,'RegQueryMultipleValuesW',\
       RegQueryValueA,'RegQueryValueA',\
       RegQueryValueW,'RegQueryValueW',\
       RegQueryValueExA,'RegQueryValueExA',\
       RegQueryValueExW,'RegQueryValueExW',\
       RegReplaceKeyA,'RegReplaceKeyA',\
       RegReplaceKeyW,'RegReplaceKeyW',\
       RegRestoreKeyA,'RegRestoreKeyA',\
       RegRestoreKeyW,'RegRestoreKeyW',\
       RegSaveKeyA,'RegSaveKeyA',\
       RegSaveKeyW,'RegSaveKeyW',\
       RegSetKeySecurity,'RegSetKeySecurity',\
       RegSetValueA,'RegSetValueA',\
       RegSetValueW,'RegSetValueW',\
       RegSetValueExA,'RegSetValueExA',\
       RegSetValueExW,'RegSetValueExW',\
       RegUnLoadKeyA,'RegUnLoadKeyA',\
       RegUnLoadKeyW,'RegUnLoadKeyW',\
       RegisterEventSourceA,'RegisterEventSourceA',\
       RegisterEventSourceW,'RegisterEventSourceW',\
       RegisterServiceCtrlHandlerA,'RegisterServiceCtrlHandlerA',\
       RegisterServiceCtrlHandlerW,'RegisterServiceCtrlHandlerW',\
       ReportEventA,'ReportEventA',\
       ReportEventW,'ReportEventW',\
       RevertToSelf,'RevertToSelf',\
       SetAclInformation,'SetAclInformation',\
       SetEntriesInAccessListA,'SetEntriesInAccessListA',\
       SetEntriesInAccessListW,'SetEntriesInAccessListW',\
       SetEntriesInAclA,'SetEntriesInAclA',\
       SetEntriesInAclW,'SetEntriesInAclW',\
       SetEntriesInAuditListA,'SetEntriesInAuditListA',\
       SetEntriesInAuditListW,'SetEntriesInAuditListW',\
       SetFileSecurityA,'SetFileSecurityA',\
       SetFileSecurityW,'SetFileSecurityW',\
       SetKernelObjectSecurity,'SetKernelObjectSecurity',\
       SetNamedSecurityInfoA,'SetNamedSecurityInfoA',\
       SetNamedSecurityInfoW,'SetNamedSecurityInfoW',\
       SetNamedSecurityInfoExA,'SetNamedSecurityInfoExA',\
       SetNamedSecurityInfoExW,'SetNamedSecurityInfoExW',\
       SetPrivateObjectSecurity,'SetPrivateObjectSecurity',\
       SetPrivateObjectSecurityEx,'SetPrivateObjectSecurityEx',\
       SetSecurityDescriptorControl,'SetSecurityDescriptorControl',\
       SetSecurityDescriptorDacl,'SetSecurityDescriptorDacl',\
       SetSecurityDescriptorGroup,'SetSecurityDescriptorGroup',\
       SetSecurityDescriptorOwner,'SetSecurityDescriptorOwner',\
       SetSecurityDescriptorSacl,'SetSecurityDescriptorSacl',\
       SetSecurityInfo,'SetSecurityInfo',\
       SetSecurityInfoExA,'SetSecurityInfoExA',\
       SetSecurityInfoExW,'SetSecurityInfoExW',\
       SetServiceBits,'SetServiceBits',\
       SetServiceObjectSecurity,'SetServiceObjectSecurity',\
       SetServiceStatus,'SetServiceStatus',\
       SetThreadToken,'SetThreadToken',\
       SetTokenInformation,'SetTokenInformation',\
       StartServiceA,'StartServiceA',\
       StartServiceW,'StartServiceW',\
       StartServiceCtrlDispatcherA,'StartServiceCtrlDispatcherA',\
       StartServiceCtrlDispatcherW,'StartServiceCtrlDispatcherW',\
       SynchronizeWindows31FilesAndWindowsNTRegistry,'SynchronizeWindows31FilesAndWindowsNTRegistry',\
       TrusteeAccessToObjectA,'TrusteeAccessToObjectA',\
       TrusteeAccessToObjectW,'TrusteeAccessToObjectW',\
       UnlockServiceDatabase,'UnlockServiceDatabase',\
       WriteRaw,'WriteRaw'

api AbortSystemShutdown,\
    AccessCheckAndAuditAlarm,\
    AccessCheckByTypeAndAuditAlarm,\
    AccessCheckByTypeResultListAndAuditAlarm,\
    BackupEventLog,\
    BuildExplicitAccessWithName,\
    BuildImpersonateExplicitAccessWithName,\
    BuildImpersonateTrustee,\
    BuildSecurityDescriptor,\
    BuildTrusteeWithName,\
    BuildTrusteeWithSid,\
    ChangeServiceConfig2,\
    ChangeServiceConfig,\
    ClearEventLog,\
    ConvertAccessToSecurityDescriptor,\
    ConvertSecurityDescriptorToAccess,\
    ConvertSecurityDescriptorToAccessNamed,\
    CreateProcessAsUser,\
    CreateService,\
    CryptAcquireContext,\
    CryptEnumProviderTypes,\
    CryptEnumProviders,\
    CryptGetDefaultProvider,\
    CryptSetProvider,\
    CryptSetProviderEx,\
    CryptSignHash,\
    CryptVerifySignature,\
    DecryptFile,\
    ElfBackupEventLogFile,\
    ElfClearEventLogFile,\
    ElfOpenBackupEventLog,\
    ElfOpenEventLog,\
    ElfReadEventLog,\
    ElfRegisterEventSource,\
    ElfReportEvent,\
    EncryptFile,\
    EnumDependentServices,\
    EnumServicesStatus,\
    GetAccessPermissionsForObject,\
    GetAuditedPermissionsFromAcl,\
    GetCurrentHwProfile,\
    GetEffectiveRightsFromAcl,\
    GetExplicitEntriesFromAcl,\
    GetFileSecurity,\
    GetMultipleTrustee,\
    GetMultipleTrusteeOperation,\
    GetNamedSecurityInfo,\
    GetNamedSecurityInfoEx,\
    GetSecurityInfoEx,\
    GetServiceDisplayName,\
    GetServiceKeyName,\
    GetSidLengthRequired,\
    GetTrusteeName,\
    GetTrusteeType,\
    GetUserName,\
    I_ScSetServiceBits,\
    InitiateSystemShutdown,\
    LogonUser,\
    LookupAccountName,\
    LookupAccountSid,\
    LookupPrivilegeDisplayName,\
    LookupPrivilegeName,\
    LookupPrivilegeValue,\
    LookupSecurityDescriptorParts,\
    ObjectCloseAuditAlarm,\
    ObjectDeleteAuditAlarm,\
    ObjectOpenAuditAlarm,\
    ObjectPrivilegeAuditAlarm,\
    OpenBackupEventLog,\
    OpenEventLog,\
    OpenRaw,\
    OpenSCManager,\
    OpenService,\
    PrivilegedServiceAuditAlarm,\
    QueryRecoveryAgents,\
    QueryServiceConfig2,\
    QueryServiceConfig,\
    QueryServiceLockStatus,\
    ReadEventLog,\
    RegConnectRegistry,\
    RegCreateKey,\
    RegCreateKeyEx,\
    RegDeleteKey,\
    RegDeleteValue,\
    RegEnumKey,\
    RegEnumKeyEx,\
    RegEnumValue,\
    RegLoadKey,\
    RegOpenKey,\
    RegOpenKeyEx,\
    RegQueryInfoKey,\
    RegQueryMultipleValues,\
    RegQueryValue,\
    RegQueryValueEx,\
    RegReplaceKey,\
    RegRestoreKey,\
    RegSaveKey,\
    RegSetValue,\
    RegSetValueEx,\
    RegUnLoadKey,\
    RegisterEventSource,\
    RegisterServiceCtrlHandler,\
    ReportEvent,\
    SetEntriesInAccessList,\
    SetEntriesInAcl,\
    SetEntriesInAuditList,\
    SetFileSecurity,\
    SetNamedSecurityInfo,\
    SetNamedSecurityInfoEx,\
    SetSecurityInfoEx,\
    StartService,\
    StartServiceCtrlDispatcher,\
    TrusteeAccessToObject


; COMCTL32 API calls

import comctl32,\
       CreateMappedBitmap,'CreateMappedBitmap',\
       CreatePropertySheetPageA,'CreatePropertySheetPageA',\
       CreatePropertySheetPageW,'CreatePropertySheetPageW',\
       CreateStatusWindowA,'CreateStatusWindowA',\
       CreateStatusWindowW,'CreateStatusWindowW',\
       CreateToolbar,'CreateToolbar',\
       CreateToolbarEx,'CreateToolbarEx',\
       CreateUpDownControl,'CreateUpDownControl',\
       DestroyPropertySheetPage,'DestroyPropertySheetPage',\
       DrawInsert,'DrawInsert',\
       DrawStatusTextA,'DrawStatusTextA',\
       DrawStatusTextW,'DrawStatusTextW',\
       FlatSB_EnableScrollBar,'FlatSB_EnableScrollBar',\
       FlatSB_GetScrollInfo,'FlatSB_GetScrollInfo',\
       FlatSB_GetScrollPos,'FlatSB_GetScrollPos',\
       FlatSB_GetScrollProp,'FlatSB_GetScrollProp',\
       FlatSB_GetScrollRange,'FlatSB_GetScrollRange',\
       FlatSB_SetScrollInfo,'FlatSB_SetScrollInfo',\
       FlatSB_SetScrollPos,'FlatSB_SetScrollPos',\
       FlatSB_SetScrollProp,'FlatSB_SetScrollProp',\
       FlatSB_SetScrollRange,'FlatSB_SetScrollRange',\
       FlatSB_ShowScrollBar,'FlatSB_ShowScrollBar',\
       GetEffectiveClientRect,'GetEffectiveClientRect',\
       ImageList_Add,'ImageList_Add',\
       ImageList_AddIcon,'ImageList_AddIcon',\
       ImageList_AddMasked,'ImageList_AddMasked',\
       ImageList_BeginDrag,'ImageList_BeginDrag',\
       ImageList_Copy,'ImageList_Copy',\
       ImageList_Create,'ImageList_Create',\
       ImageList_Destroy,'ImageList_Destroy',\
       ImageList_DragEnter,'ImageList_DragEnter',\
       ImageList_DragLeave,'ImageList_DragLeave',\
       ImageList_DragMove,'ImageList_DragMove',\
       ImageList_DragShowNolock,'ImageList_DragShowNolock',\
       ImageList_Draw,'ImageList_Draw',\
       ImageList_DrawEx,'ImageList_DrawEx',\
       ImageList_DrawIndirect,'ImageList_DrawIndirect',\
       ImageList_Duplicate,'ImageList_Duplicate',\
       ImageList_EndDrag,'ImageList_EndDrag',\
       ImageList_GetBkColor,'ImageList_GetBkColor',\
       ImageList_GetDragImage,'ImageList_GetDragImage',\
       ImageList_GetIcon,'ImageList_GetIcon',\
       ImageList_GetIconSize,'ImageList_GetIconSize',\
       ImageList_GetImageCount,'ImageList_GetImageCount',\
       ImageList_GetImageInfo,'ImageList_GetImageInfo',\
       ImageList_GetImageRect,'ImageList_GetImageRect',\
       ImageList_LoadImageA,'ImageList_LoadImageA',\
       ImageList_LoadImageW,'ImageList_LoadImageW',\
       ImageList_Merge,'ImageList_Merge',\
       ImageList_Read,'ImageList_Read',\
       ImageList_Remove,'ImageList_Remove',\
       ImageList_Replace,'ImageList_Replace',\
       ImageList_ReplaceIcon,'ImageList_ReplaceIcon',\
       ImageList_SetBkColor,'ImageList_SetBkColor',\
       ImageList_SetDragCursorImage,'ImageList_SetDragCursorImage',\
       ImageList_SetFilter,'ImageList_SetFilter',\
       ImageList_SetIconSize,'ImageList_SetIconSize',\
       ImageList_SetImageCount,'ImageList_SetImageCount',\
       ImageList_SetOverlayImage,'ImageList_SetOverlayImage',\
       ImageList_Write,'ImageList_Write',\
       InitCommonControls,'InitCommonControls',\
       InitCommonControlsEx,'InitCommonControlsEx',\
       InitializeFlatSB,'InitializeFlatSB',\
       LBItemFromPt,'LBItemFromPt',\
       MakeDragList,'MakeDragList',\
       MenuHelp,'MenuHelp',\
       PropertySheetA,'PropertySheetA',\
       PropertySheetW,'PropertySheetW',\
       ShowHideMenuCtl,'ShowHideMenuCtl',\
       UninitializeFlatSB,'UninitializeFlatSB',\
       _TrackMouseEvent,'_TrackMouseEvent'

api CreatePropertySheetPage,\
    CreateStatusWindow,\
    DrawStatusText,\
    ImageList_LoadImage,\
    PropertySheet


; COMDLG32 API calls

import comdlg32,\
       ChooseColorA,'ChooseColorA',\
       ChooseColorW,'ChooseColorW',\
       ChooseFontA,'ChooseFontA',\
       ChooseFontW,'ChooseFontW',\
       CommDlgExtendedError,'CommDlgExtendedError',\
       FindTextA,'FindTextA',\
       FindTextW,'FindTextW',\
       FormatCharDlgProc,'FormatCharDlgProc',\
       GetFileTitleA,'GetFileTitleA',\
       GetFileTitleW,'GetFileTitleW',\
       GetOpenFileNameA,'GetOpenFileNameA',\
       GetOpenFileNameW,'GetOpenFileNameW',\
       GetSaveFileNameA,'GetSaveFileNameA',\
       GetSaveFileNameW,'GetSaveFileNameW',\
       LoadAlterBitmap,'LoadAlterBitmap',\
       PageSetupDlgA,'PageSetupDlgA',\
       PageSetupDlgW,'PageSetupDlgW',\
       PrintDlgA,'PrintDlgA',\
       PrintDlgW,'PrintDlgW',\
       ReplaceTextA,'ReplaceTextA',\
       ReplaceTextW,'ReplaceTextW',\
       WantArrows,'WantArrows',\
       dwLBSubclass,'dwLBSubclass',\
       dwOKSubclass,'dwOKSubclass'

api ChooseColor,\
    ChooseFont,\
    FindText,\
    GetFileTitle,\
    GetOpenFileName,\
    GetSaveFileName,\
    PageSetupDlg,\
    PrintDlg,\
    ReplaceText


; SHELL32 API calls

import shell32,\
       CheckEscapesA,'CheckEscapesA',\
       CheckEscapesW,'CheckEscapesW',\
       DoEnvironmentSubstA,'DoEnvironmentSubstA',\
       DoEnvironmentSubstW,'DoEnvironmentSubstW',\
       DragAcceptFiles,'DragAcceptFiles',\
       DragFinish,'DragFinish',\
       DragQueryFileA,'DragQueryFileA',\
       DragQueryFileW,'DragQueryFileW',\
       DragQueryPoint,'DragQueryPoint',\
       DuplicateIcon,'DuplicateIcon',\
       ExtractAssociatedIconA,'ExtractAssociatedIconA',\
       ExtractAssociatedIconW,'ExtractAssociatedIconW',\
       ExtractAssociatedIconExA,'ExtractAssociatedIconExA',\
       ExtractAssociatedIconExW,'ExtractAssociatedIconExW',\
       ExtractIconA,'ExtractIconA',\
       ExtractIconW,'ExtractIconW',\
       ExtractIconExA,'ExtractIconExA',\
       ExtractIconExW,'ExtractIconExW',\
       ExtractIconResInfoA,'ExtractIconResInfoA',\
       ExtractIconResInfoW,'ExtractIconResInfoW',\
       FindExeDlgProc,'FindExeDlgProc',\
       FindExecutableA,'FindExecutableA',\
       FindExecutableW,'FindExecutableW',\
       FreeIconList,'FreeIconList',\
       InternalExtractIconListA,'InternalExtractIconListA',\
       InternalExtractIconListW,'InternalExtractIconListW',\
       RealShellExecuteA,'RealShellExecuteA',\
       RealShellExecuteW,'RealShellExecuteW',\
       RealShellExecuteExA,'RealShellExecuteExA',\
       RealShellExecuteExW,'RealShellExecuteExW',\
       RegenerateUserEnvironment,'RegenerateUserEnvironment',\
       SHAddToRecentDocs,'SHAddToRecentDocs',\
       SHAppBarMessage,'SHAppBarMessage',\
       SHBrowseForFolderA,'SHBrowseForFolderA',\
       SHBrowseForFolderW,'SHBrowseForFolderW',\
       SHChangeNotify,'SHChangeNotify',\
       SHEmptyRecycleBinA,'SHEmptyRecycleBinA',\
       SHEmptyRecycleBinW,'SHEmptyRecycleBinW',\
       SHFileOperationA,'SHFileOperationA',\
       SHFileOperationW,'SHFileOperationW',\
       SHFormatDrive,'SHFormatDrive',\
       SHFreeNameMappings,'SHFreeNameMappings',\
       SHGetDataFromIDListA,'SHGetDataFromIDListA',\
       SHGetDataFromIDListW,'SHGetDataFromIDListW',\
       SHGetDesktopFolder,'SHGetDesktopFolder',\
       SHGetDiskFreeSpaceA,'SHGetDiskFreeSpaceA',\
       SHGetDiskFreeSpaceW,'SHGetDiskFreeSpaceW',\
       SHGetFileInfoA,'SHGetFileInfoA',\
       SHGetFileInfoW,'SHGetFileInfoW',\
       SHGetInstanceExplorer,'SHGetInstanceExplorer',\
       SHGetMalloc,'SHGetMalloc',\
       SHGetNewLinkInfo,'SHGetNewLinkInfo',\
       SHGetPathFromIDListA,'SHGetPathFromIDListA',\
       SHGetPathFromIDListW,'SHGetPathFromIDListW',\
       SHGetSettings,'SHGetSettings',\
       SHGetSpecialFolderLocation,'SHGetSpecialFolderLocation',\
       SHGetSpecialFolderPathA,'SHGetSpecialFolderPathA',\
       SHGetSpecialFolderPathW,'SHGetSpecialFolderPathW',\
       SHInvokePrinterCommandA,'SHInvokePrinterCommandA',\
       SHInvokePrinterCommandW,'SHInvokePrinterCommandW',\
       SHLoadInProc,'SHLoadInProc',\
       SHQueryRecycleBinA,'SHQueryRecycleBinA',\
       SHQueryRecycleBinW,'SHQueryRecycleBinW',\
       SHUpdateRecycleBinIcon,'SHUpdateRecycleBinIcon',\
       SheChangeDirA,'SheChangeDirA',\
       SheChangeDirW,'SheChangeDirW',\
       SheChangeDirExA,'SheChangeDirExA',\
       SheChangeDirExW,'SheChangeDirExW',\
       SheFullPathA,'SheFullPathA',\
       SheFullPathW,'SheFullPathW',\
       SheGetCurDrive,'SheGetCurDrive',\
       SheGetDirA,'SheGetDirA',\
       SheGetDirW,'SheGetDirW',\
       SheRemoveQuotesA,'SheRemoveQuotesA',\
       SheRemoveQuotesW,'SheRemoveQuotesW',\
       SheSetCurDrive,'SheSetCurDrive',\
       SheShortenPathA,'SheShortenPathA',\
       SheShortenPathW,'SheShortenPathW',\
       ShellAboutA,'ShellAboutA',\
       ShellAboutW,'ShellAboutW',\
       ShellExecuteA,'ShellExecuteA',\
       ShellExecuteW,'ShellExecuteW',\
       ShellExecuteExA,'ShellExecuteExA',\
       ShellExecuteExW,'ShellExecuteExW',\
       ShellHookProc,'ShellHookProc',\
       Shell_NotifyIconA,'Shell_NotifyIconA',\
       Shell_NotifyIconW,'Shell_NotifyIconW',\
       StrChrA,'StrChrA',\
       StrChrW,'StrChrW',\
       StrChrIA,'StrChrIA',\
       StrChrIW,'StrChrIW',\
       StrCmpNA,'StrCmpNA',\
       StrCmpNW,'StrCmpNW',\
       StrCmpNIA,'StrCmpNIA',\
       StrCmpNIW,'StrCmpNIW',\
       StrCpyNA,'StrCpyNA',\
       StrCpyNW,'StrCpyNW',\
       StrNCmpA,'StrNCmpA',\
       StrNCmpW,'StrNCmpW',\
       StrNCmpIA,'StrNCmpIA',\
       StrNCmpIW,'StrNCmpIW',\
       StrNCpyA,'StrNCpyA',\
       StrNCpyW,'StrNCpyW',\
       StrRChrA,'StrRChrA',\
       StrRChrW,'StrRChrW',\
       StrRChrIA,'StrRChrIA',\
       StrRChrIW,'StrRChrIW',\
       StrRStrA,'StrRStrA',\
       StrRStrW,'StrRStrW',\
       StrRStrIA,'StrRStrIA',\
       StrRStrIW,'StrRStrIW',\
       WOWShellExecute,'WOWShellExecute'

api CheckEscapes,\
    DoEnvironmentSubst,\
    DragQueryFile,\
    ExtractAssociatedIcon,\
    ExtractAssociatedIconEx,\
    ExtractIcon,\
    ExtractIconEx,\
    ExtractIconResInfo,\
    FindExecutable,\
    InternalExtractIconList,\
    RealShellExecute,\
    RealShellExecuteEx,\
    SHBrowseForFolder,\
    SHEmptyRecycleBin,\
    SHFileOperation,\
    SHGetDataFromIDList,\
    SHGetDiskFreeSpace,\
    SHGetFileInfo,\
    SHGetPathFromIDList,\
    SHGetSpecialFolderPath,\
    SHInvokePrinterCommand,\
    SHQueryRecycleBin,\
    SheChangeDir,\
    SheChangeDirEx,\
    SheFullPath,\
    SheGetDir,\
    SheRemoveQuotes,\
    SheShortenPath,\
    ShellAbout,\
    ShellExecute,\
    ShellExecuteEx,\
    Shell_NotifyIcon,\
    StrChr,\
    StrChrI,\
    StrCmpN,\
    StrCmpNI,\
    StrCpyN,\
    StrNCmp,\
    StrNCmpI,\
    StrNCpy,\
    StrRChr,\
    StrRChrI,\
    StrRStr,\
    StrRStrI,\
    StrStr,\
    StrStrI


; WSOCK32 API calls
	      
import wsock32,\
       AcceptEx,'AcceptEx',\
       EnumProtocolsA,'EnumProtocolsA',\
       EnumProtocolsW,'EnumProtocolsW',\
       GetAcceptExSockaddrs,'GetAcceptExSockaddrs',\
       GetAddressByNameA,'GetAddressByNameA',\
       GetAddressByNameW,'GetAddressByNameW',\
       GetNameByTypeA,'GetNameByTypeA',\
       GetNameByTypeW,'GetNameByTypeW',\
       GetServiceA,'GetServiceA',\
       GetServiceW,'GetServiceW',\
       GetTypeByNameA,'GetTypeByNameA',\
       GetTypeByNameW,'GetTypeByNameW',\
       MigrateWinsockConfiguration,'MigrateWinsockConfiguration',\
       NPLoadNameSpaces,'NPLoadNameSpaces',\
       SetServiceA,'SetServiceA',\
       SetServiceW,'SetServiceW',\
       TransmitFile,'TransmitFile',\
       WEP,'WEP',\
       WSAAsyncGetHostByAddr,'WSAAsyncGetHostByAddr',\
       WSAAsyncGetHostByName,'WSAAsyncGetHostByName',\
       WSAAsyncGetProtoByName,'WSAAsyncGetProtoByName',\
       WSAAsyncGetProtoByNumber,'WSAAsyncGetProtoByNumber',\
       WSAAsyncGetServByName,'WSAAsyncGetServByName',\
       WSAAsyncGetServByPort,'WSAAsyncGetServByPort',\
       WSAAsyncSelect,'WSAAsyncSelect',\
       WSACancelAsyncRequestA,'WSACancelAsyncRequestA',\
       WSACancelAsyncRequestW,'WSACancelAsyncRequestW',\
       WSACancelBlockingCall,'WSACancelBlockingCall',\
       WSACleanup,'WSACleanup',\
       WSAGetLastError,'WSAGetLastError',\
       WSAIsBlocking,'WSAIsBlocking',\
       WSARecvEx,'WSARecvEx',\
       WSASetBlockingHook,'WSASetBlockingHook',\
       WSASetLastError,'WSASetLastError',\
       WSAStartup,'WSAStartup',\
       WSAUnhookBlockingHook,'WSAUnhookBlockingHook',\
       __WSAFDIsSet,'__WSAFDIsSet',\
       accept,'accept',\
       bind,'bind',\
       closesocket,'closesocket',\
       connect,'connect',\
       dn_expand,'dn_expand',\
       gethostbyaddr,'gethostbyaddr',\
       gethostbyname,'gethostbyname',\
       gethostname,'gethostname',\
       getnetbyname,'getnetbyname',\
       getpeername,'getpeername',\
       getprotobyname,'getprotobyname',\
       getprotobynumber,'getprotobynumber',\
       getservbyname,'getservbyname',\
       getservbyport,'getservbyport',\
       getsockname,'getsockname',\
       getsockopt,'getsockopt',\
       htonl,'htonl',\
       htons,'htons',\
       inet_addr,'inet_addr',\
       inet_network,'inet_network',\
       inet_ntoa,'inet_ntoa',\
       ioctlsocket,'ioctlsocket',\
       listen,'listen',\
       ntohl,'ntohl',\
       ntohs,'ntohs',\
       rcmd,'rcmd',\
       recv,'recv',\
       recvfrom,'recvfrom',\
       rexec,'rexec',\
       rresvport,'rresvport',\
       s_perror,'s_perror',\
       select,'select',\
       send,'send',\
       sendto,'sendto',\
       sethostname,'sethostname',\
       setsockopt,'setsockopt',\
       shutdown,'shutdown',\
       socket,'socket'

api EnumProtocols,\
    GetAddressByName,\
    GetNameByType,\
    GetService,\
    GetTypeByName,\	
    SetService,\		
    WSACancelAsyncRequest


purge import,api

macro .data { section '.data' data readable writeable }

macro .code { section '.text' code readable executable }

macro .end label
{
   entry label

   section '.idata' import data readable writeable

     library kernel32,'KERNEL32.DLL',\
	     user32,'USER32.DLL',\
	     gdi32,'GDI32.DLL',\
	     advapi32,'ADVAPI32.DLL',\
	     comctl32,'COMCTL32.DLL',\
	     comdlg32,'COMDLG32.DLL',\
	     shell32,'SHELL32.DLL',\
	     wsock32,'WSOCK32.DLL'

     import_kernel32
     import_user32
     import_gdi32
     import_advapi32
     import_comctl32
     import_comdlg32
     import_shell32
     import_wsock32

     all_api
}

virtual at 0
 xchg eax,eax
 detected_16bit = $-1
end virtual

if detected_16bit
 format PE GUI 4.0
end if
