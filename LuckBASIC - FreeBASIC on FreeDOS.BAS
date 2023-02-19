'---script created on 11-08-2022 14:51:59 by 
' LuckBasic
' This is my personal project to build a Basic interpreter.
' It's a high level project to prove to myself that i understand how to build an interpreter.
' I'm intentionaly using very simple algorithms and data structures. Readability outweighs performance.
' 
' Gary Luckenbaugh - fall of '22

'print "start"
dim t as integer
'input t
dim shared buff as string
buff = ""
dim shared buff_after_parse_or as string
dim shared pc as integer ' Program counter for interpreter
const ops = 10
dim shared op(ops) as string 

data "dummy", "+", "-", "*", "/", "(", ")", "=", "^", "@", ","
for t = 0 to ops
    read op(t)
next t

'op(0) = "dummy"
'op(1) = "+"
'op(2) = "-"
'op(3) = "*"
'op(4) = "/"
'op(5) = "("
'op(6) = ")"
'op(7) = "="
'op(8) = "^"
'op(9) = "@"
op(10) = "," 

' dim pTree as long
dim shared statement(5000) as string
dim shared at_array(5000) as integer
dim shared i as integer
dim shared vars(26) as string

declare function do_if(cmd as string) as integer
declare function next_pc(pc as integer) as integer
declare function do_goto(cmd as string) as integer
declare function do_gosub(cmd as string) as integer
declare function do_return() as integer
declare Function Eval_rpn(rpn as string) as string
declare function Expr_eval(e as string) as string
declare Function mypop() as string
declare Function rid_eol(inpval as string) as string
declare Function Get_variable(key as string) as string
declare function gettok() as string
declare function parse_factor() as string
declare function parse_exponent() as string
declare function parse_unary_minus() as string
declare function parse_term ()as string
declare function parse_expr () as string
declare function parse_condition () as string
declare function parse_and () as string
declare function parse_or () as string

declare sub edit_delete(linenum as integer)
declare sub edit_replace(linenum as integer, buff as string)
declare sub process_line(i as string)
declare sub process_tok(tok as string)
declare sub process_op(op as string)
declare sub process_num_or_var(num as string)
declare Sub mypush(tok as string)
declare sub Start_variable_store() 
declare sub Set_variable(key as string, item as string)
declare sub Release_variable_store()
declare sub do_input_at_array(e as string)
declare Sub clearstack()
declare sub do_command(cmd as string)
declare Sub do_list()
declare sub do_print(cmd as string)
declare sub do_let(cmd as string)
declare sub do_new()
declare sub do_load(fname as string)
declare sub do_save(fname as string)
declare sub do_run()
declare sub do_input(cmd as string)
declare sub do_for(cmd as string)
declare sub do_next(cmd as string)
declare sub do_let_at_array(e as string)
declare sub Main()

Main()

function myucase(s as string) as string
    dim c(100) as string
    dim flag as integer
    
    dim i as integer
    dim collect as string
    flag = true
    collect = ""
    for i = 1 to len(s)
        c(i) = mid(s, i, 1)
'        Console.WriteLine("character and code = " + c(i) + "  code = ", asc(c(i)))
        if asc(c(i)) = 34 then ' toggle flag
'            Console.writeline("Toggling flag at "+i)
            
            if flag = true then
                flag = false
            else
                flag = true
            end if
        end if
        if flag = true then
            c(i) = Ucase(c(i))
        end if
'       Console.WriteLine("character and code = " + c(i) + "  code = ", asc(c(i)))
    next i
    
    for i = 1 to len(s)
        
        collect = collect + c(i)
    next i
    
    myucase = collect
    
End function
  
Sub Main()
    'input t
  'print "I'm here"
  'dim ug as integer
  'input up
  dim k as integer

  for k = 1 to 5000
        at_array(k) = 2*k  ' test pattern
  next k

  for k = 1 to 5000
        statement(k) = ""
  next k

  

  'op = new string(ops) { "dummy for 0" , "+", "-", "*", "/", "(", ")", "=", "^", "@", "," }

  dim i as string

  dim cont as integer
  
  dim tok as string
  
  ' start_variable_store()
  
  cont = true
  
  do while cont = true
      line input "basic> ", i
      rid_eol(i)
      i = myucase(i)
      ' print i
      
      buff = i
      
      tok = gettok()

      if left(tok, 1) >= "0" and left(tok, 1) <= "9" then
  
         process_line(i)

      elseif tok <> "BYE" then
      
         do_command(i)
          
      elseif tok = "BYE" then
          cont = false
          
      end if
      
   loop
 
   ' Release_variable_store()


end sub

sub edit_delete(linenum as integer)
  statement(linenum) = ""
end sub

sub edit_replace(linenum as integer, buff as string)
    statement(linenum) = buff
end sub

sub process_line(i as string)
    dim line_tok as string
    dim nexttok as string
    buff = i
    
    line_tok = gettok()
    nexttok = gettok()
    if nexttok = "~" then
        edit_delete(val(line_tok))
    else
       edit_replace(val(line_tok), nexttok + buff)
'       print "edit_replace(val(line_tok), nexttok + buff)", "next ",nexttok+buff

    end if
    
end sub

sub do_command(cmd as string)

    if cmd = "LIST" then
   
      do_list()
    
    elseif left(cmd, 5) = "PRINT" then

      do_print(cmd)

    elseif left(cmd, 3) = "LET" then
  
       do_let(cmd)
       buff = buff_after_parse_or ' look for extraneous stuff
       if gettok() <> "~" then
           print "warning: extra stuff after expression"
       end if
       
    elseif cmd = "NEW"then
    
       do_new()
       
    elseif left(cmd, 4) = "LOAD" then
    
        do_load(trim(mid(cmd, 5)))
        
    elseif left(cmd, 4) = "SAVE" then
  
        do_save(trim(mid(cmd, 5)))
  
    elseif cmd = "RUN" then
  
        do_run()
        
    else
    
        do_let("LET "+cmd)
        buff = buff_after_parse_or ' look for extraneous stuff
        if gettok() <> "~" then
            print "warning: extra stuff after expression"
        end if
       
    end if
  
end sub

sub do_load(fname as string)
   dim ff as Ubyte
   dim inbuff as string
   dim myline as string

   do_new()
   ff = FreeFile
   Open fname For Input As #ff
        do while not eof(ff)
            line input #ff, myline
            process_line(myline)
        loop
    Close #ff
   'For Each line As String In File.ReadAllLines(fname)
    '   process_line(line)
   'next
   
end sub

sub do_save(fname as string)

   dim ff as Ubyte

   ff = FreeFile
   Open fname For output As #ff
        dim i as integer
        for i = 1 to 5000
            if statement(i) <> "" then
                print #ff, str(i) + " " +statement(i)
            end if
        next i
      
    Close #ff
 
end sub
    

sub do_new()
    dim i as integer 
    for i = 1 to 5000
       statement(i) = ""
    next i

end sub

Sub do_list()
  dim i as integer
  for i = 1 to 5000
    
      if statement(i) <> "" then
        print str(i) + " " +statement(i)
      end if
      
  next i
  
End Sub

dim shared acvar(100) as string    ' For loop variable
dim shared aistart(100) as integer ' start
dim shared ailast(100) as integer  ' last
dim shared aistep(100) as integer  ' step
dim shared ailine(100) as integer  ' line number of For
dim shared for_stackp as integer   ' For loop stack pointer

dim shared gosub_stack(100) as integer
dim shared gosub_stack_ptr as integer


sub do_run()

   dim startpc as integer
   dim stoprun as integer
   dim cmd as string
   dim target as integer
   dim l as integer
   
   startpc = 0
   for_stackp = 0
   
   gosub_stack_ptr = 0
   
   stoprun = false
   

   
   for l = 1 to 5000
   
      if statement(l) <> "" then
          startpc = l
          exit for
      end if
      
   next l
  
   if startpc = 0 then
      print "No statements in buffer!"
      exit sub
   end if
  
  pc = startpc
   
  do while not stoprun and pc <> 0
  'print "top of run loop, pc = ";pc
      cmd = statement(pc)
      ' print "cmd is ";cmd;" at ";pc
      
      if left(cmd, 3) = "LET" then
         do_let(cmd)
         buff = buff_after_parse_or ' look for extraneous stuff
         if gettok() <> "~" then
            print "warning: extra stuff after LET expression on line " + str(pc)
             
          end if
 
      elseif left(cmd, 5) = "PRINT" then
          do_print(cmd)
      elseif left(cmd, 4) = "GOTO" then
          pc = do_goto(cmd) - 1
      elseif left(cmd, 5) = "GOSUB" then
          pc = do_gosub(cmd) -1
      elseif left(cmd, 7) = "RETURN" then
          pc = do_return()
      elseif cmd = "END" or cmd = "STOP" then
          stoprun = true
      elseif left(cmd, 2) = "IF" then
          target = do_if(cmd)

          if target <> 0 then 
              pc = target - 1
          end if
      elseif left(cmd, 5) = "INPUT" then
          do_input(cmd)
      Elseif left(cmd, 3) = "FOR" then
          do_for(cmd)
      elseif left(cmd, 4) = "NEXT" then
          do_next(cmd)
      else
         cmd = "LET " + cmd
         do_let(cmd)
      end if
'       print "pc is before next check is ";pc; " cmd is ";cmd

      if left(cmd, 4) <> "NEXT" then
          'PRINT "WHAT THE HELL IS cmd when <> NEXT";cmd
          pc = next_pc(pc)
      end if
'print "pc after NEXT check is ";pc

  loop
  
end sub

sub do_print(cmd as string)
' print "do_print ";cmd
    dim tok as string
    dim comma_last as integer
    
    comma_last = false

    buff = mid(cmd, 6)
    
    tok = gettok()

    if tok = "~" then
         print ""
         Exit sub
    end if
    
    do while tok<> "~" 

      comma_last = false

      if lefT(tok, 1) = chr(34) then
          tok = mid(tok, 2, len(tok) - 2)
          print tok + " ";
          
          tok = gettok()
          
          if tok = "," then
             tok = gettok()
             comma_last = true
          end if
          
      else
          print Expr_eval(tok + buff) + " ";
          buff = buff_after_parse_or: 'print "buff_after_parse_or", buff_after_parse_or
 
          tok = gettok()
          
          if tok = "," then
              tok = gettok()
              comma_last = true
          end if
        end if  
    loop
    
    if not comma_last then
        print ""
    end if
  
end sub

function do_if(cmd as string) as integer
  Dim b as integer
  dim linenum as integer
  
  b = val(Expr_eval(mid(cmd, 3)))
  buff = buff_after_parse_or ' look for extraneous stuff
  if gettok() <> "THEN" then
         print "warning: THEN not found right after expression end on line "; pc
  end if

  i = instr(1, cmd, "THEN")
  
  if i <> 0 then
      linenum = val(mid(cmd, i + 4))
  else
      print "malformed command is  :" + cmd + ":   missing THEN"
      linenum = 0
  end if
  
  if b then
     do_if = linenum
  else 
     do_if = 0 
  end if
  
end function



sub do_for(cmd as string)

    cmd = mid(cmd, 4)
    buff = cmd
    
    dim cvar as string
    
    cvar = gettok()
    
    if gettok() <> "=" then
        print "Missing = in FOR statment on line "+str(pc)
        exit sub
    end if
    
    dim istart as integer
    dim ilast as integer
    dim istep as integer
    
    istart = Val(Expr_eval(buff))
    
    buff = buff_after_parse_or
    
    if gettok() <> "TO" then
        print "Missing TO in FOR on line "+str(pc)
        exit sub
    end if
    
    ilast = Val(Expr_eval(buff))
    
    buff = buff_after_parse_or
    
    istep = 1
    
    if gettok() = "STEP" then
    
       istep = Val(Expr_eval(buff))
       
    end if
    
    buff = buff_after_parse_or
    
    if gettok() <> "~" then
    
         print "Extraneous stuff after STEP value on line " +str(pc)
        
    end if
    
    for_stackp = for_stackp + 1     ' Put For info on stack
    
    acvar(for_stackp) = cvar: ' print "acvar is ";cvar
    aistart(for_stackp) = istart: 'print "istart is ";istart
    ailast(for_stackp) = ilast: 'print "ilast is "; ilast
    aistep(for_stackp) = istep: 'print "istep is "; istep
    ailine(for_stackp) = pc: 'print "pc is ";pc 
   ' input i
    
    Set_variable(cvar, str(istart))
    
end sub
  
sub do_next(cmd as string)

  if for_stackp >= 1 then
'    Console.WriteLine("NEXT "+acvar(for_stackp))

  else
    print "stack underflow NEXT without FOR on line "+str(pc)
    pc = next_pc(pc)
    exit sub
  end if
  buff = mid(cmd, 5)
    
 
    
  dim controlvariable as string
    
    controlvariable = gettok()
    
    if controlvariable <> acvar(for_stackp) then
        print "mismatched next on line "+str(pc)
    end if
    
    
    dim cvar as string
    dim start as integer
    dim last as integer
    dim istep as integer
    dim iline as integer
    
    

    start = aistart(for_stackp): 'print "next start ";start
    last = ailast(for_stackp): 'print "next last ";last
    istep = aistep(for_stackp): 'print "next istep ";istep
    iline = ailine(for_stackp): 'print "next iline ";iline

    
    cvar = acvar(for_stackp) 
    dim v as integer
    v = Val(Get_variable(cvar))
    
    v = v + istep
    'print "v is ";v
    if istep >= 0 then
        if v > last then
            'print "******* v > last so now pc is ";pc
            pc = next_pc(pc)
            'print "******** and now pc is ";pc
            for_stackp = for_stackp - 1
        else
            pc = next_pc(iline)
        end if
    else
        if v < last then
            pc = next_pc(pc)
        else
            pc = next_pc(iline)
        end if
    end if
    
    'print "next pc is ";pc
    
    Set_variable(cvar, str(v)): 'print "set ";cvar;" to ";str(v)
    dim mwait as integer
   ' input mwait

end sub

function next_pc(pc as integer) as integer
  dim i as integer
  
  for i = pc + 1 to 5000
  
    if statement(i) <> "" then
        next_pc = i
        exit function
    end if
  
  next i
  
  next_pc = 0
    
end Function

function do_goto(cmd as string) as integer
  do_goto = val(mid(cmd, 5))
end function

function do_gosub(cmd as string) as integer
   do_gosub = val(mid(cmd, 6))
   gosub_stack_ptr = gosub_stack_ptr + 1
   gosub_stack(gosub_stack_ptr)= pc
end function

function do_return() as integer
    if gosub_stack_ptr = 0 then
        print "return without matching GOSUB at line ";pc
        return 0
    end if
    do_return = gosub_stack(gosub_stack_ptr) 
    gosub_stack_ptr = gosub_stack_ptr - 1
end function

sub do_let(cmd as string)

     dim cvar as string
     dim firstchar as string

      buff = mid(cmd, 4)
      cvar = gettok()

      if cvar = "@" then
          do_let_at_array(buff)
          exit sub
      end if
      
      firstchar = left(cvar, 1)
      if not (firstchar >= "A" and firstchar <= "Z") then
      
          print "Variable name must follow LET"
          exit sub
          
      elseif gettok() <> "=" then
      
          print "Missing '=' in LET  " + cmd
          exit sub
          
      end if
  
      Set_variable(cvar, Expr_eval(buff))

end sub

sub do_let_at_array(e as string)

  dim leftval as integer
  dim rightval as integer
  
  dim rpn as string

  buff = e
  if gettok() <> "(" then
      print "an open left paren ( must follow @ in let"
      exit sub
  end if
  
  rpn = parse_or()
  
  if gettok() <> ")" then
      print "missing right paren ) in LET @"
      exit sub
  end if
  
  buff_after_parse_or = buff

  leftval = Val(Eval_rpn(rpn))
  
  buff = buff_after_parse_or
  
  If gettok() <> "=" then
      print "missing = in LET @"
      exit sub
  end if
  
  rightval = Val(Eval_rpn(parse_or()))
  
  if not (leftval >= 1 and leftval <= 5000) then
      print "subscript out of bounds on left side of assignment is "+str(leftval)
      exit sub
  end if

  at_array(leftval) = rightval
  
end sub

sub do_input(cmd as string)

     dim cvar as string
     dim firstchar as string

      buff = mid(cmd, 6)
      cvar = gettok()

      if cvar = "@" then
          do_input_at_array(buff)
          exit sub
      end if
      
      firstchar = left(cvar, 1)
      if not (firstchar >= "A" and firstchar <= "Z") then
          print"Variable name must follow INPUT"
          exit sub
      end if
          
      dim user_input as string
      
       input user_input
  
    user_input = user_input
      user_input = trim(user_input)
      Set_variable(cvar, Expr_eval(user_input))

end sub

sub do_input_at_array(e as string)

  dim user_input as string
  
  dim subval as integer
  dim inpval as integer
  
  dim rpn as string

  buff = e
  if gettok() <> "(" then
      print "an open left paren ( must follow @ in INPUT"
      exit sub
  end if
  
  rpn = parse_or()
  
  if gettok() <> ")" then
      print "missing right paren ) in INPUT @"
      exit sub
  end if
  
  subval = val(Eval_rpn(rpn))
  
  line input user_input
  
  inpval = Val(Expr_eval(user_input))
  
  if not (subval >= 1 and subval <= 5000) then
'      Console.WriteLine("subscript out of bounds on left side of INPUT @ is " + subval)
      exit sub
  end if

  at_array(subval) = inpval
  
end sub

Function Eval_rpn(rpn as string) as string

  buff = rpn
  dim tok as string
  
  clearstack()
  tok = gettok()
  
  if tok = "~" then
    Eval_rpn = "0"
    exit function
  end if
  
  do while tok<> "~"
      process_tok(tok)
    tok = gettok()
  loop
  
  dim r as string
  r = mypop()

  Eval_rpn = r
  
End Function
  
function Expr_eval(e as string) as string

  clearstack()

  buff = e
  buff = parse_or()

  dim tok as string

  tok = gettok()
  
  if tok = "~" then
    Expr_eval = "0"
    exit function
  end if
  
  do While tok<> "~"
    process_tok(tok)
    tok = gettok()
  loop
  dim r as integer
  r = val(mypop())

  Expr_eval = str(r)

End function

sub process_tok(tok as string)
'    dim logical_op() as string = new string(8) { "dummy", "<", "<=", "=", ">", ">=", "<>", "AND", "OR" }
    dim logical_op(9) as string = { "dummy", "<", "<=", "=", ">", ">=", "<>", "AND", "OR" } 
    dim i as integer

    if tok = "UNM" then
        mypush(str(-val(mypop())))
        exit sub
    end if
    
    if tok = "GETAT" then

        i = val(mypop())
        if i < 1 or i > 5000 then
            print "Subscript out of range 1..5000 for " + str(i)
            mypush(str(0))
            Exit Sub
        end if
        
        mypush(str(at_array(i)))
        
        exit sub
    end if
    
    for i = 1 to ops
        if op(i) = tok then
          process_op(op(i))
          exit sub
        end if
    next i
 
    for i = 1 to 8

        if logical_op(i) = tok then

          process_op(logical_op(i))
          exit sub
        end if
    next i
 
    process_num_or_var(tok)
    
end sub

sub process_op(op as string)
   dim leftval as integer
   Dim rightval as integer
   dim res as integer
   
   rightval = Val(mypop())
   leftval = Val(mypop())
   
   If op = "+" then
      res = leftval + rightval
   elseif op = "-" then
      res = leftval - rightval
   elseif op = "*" then
      res = leftval*rightval
   elseif op = "/" then
      res = leftval/rightval
   elseif op = "^" then
      res = leftval ^ rightval
   elseif op = "<" then
      res = leftval < rightval
   elseif op = "<=" then
      res = leftval <= rightval
   elseif op = "=" then
'         Console.WriteLine("= "+leftval+" "+rightval)
      res = leftval = rightval
   elseif op = ">" then
      res = leftval > rightval
   elseif op = ">=" then
      res = leftval >= rightval
   elseif op = "AND" then
'      Console.WriteLine("and "+leftval+" "+rightval)
      res = leftval and rightval
   elseif op = "OR" then
      res = leftval or rightval
    else
       print "Not an operator " + op
       exit sub
    End if
    
    mypush(str(res))
    
end sub

sub process_num_or_var(num as string)
    if left(num, 1) >= "0" and left(num,1) <= "9" then

      mypush(num)
    else
      mypush(Get_variable(num))
    end if
end sub

dim shared mystack(50) as string
dim shared msp as integer

Sub clearstack()
    msp = 1
end sub

Sub mypush(tok as string)
  ' Console.WriteLine("push " + tok)
   mystack(msp) = tok
   msp = msp + 1
end sub

Function mypop() as string
  msp = msp - 1
  ' Console.WriteLine("pop sp = "+sp + "tok = " + mystack(sp))
  mypop = mystack(msp)
end function 

' There must be an API for this, but for now this will work
' Found the API, but my function calls the API

Function rid_eol(inpval as string) as string
  rid_eol = inpval
end function

sub Start_variable_store() 
'   pTree = Tree_New
End sub

sub Set_variable(key as string, item as string)
    'print "Set *************** ";key;" to ";item
    if len(key) <> 1 then
        print "variable name " + key + " is too long"
        exit sub
    end if

    dim nkey as integer

    nkey = asc(key) - asc("A") + 1

    vars(nkey) = item
    'print "var ";nkey;" "; chr(nkey + asc("A") - 1); " in set_var";" is ";item

End sub

Function Get_variable(key as string) as string  
 '   Get_variable = Tree_Get(pTree, key)
    if len(key) <> 1 then
        'print "variable name " + key + " is too long"
        Get_variable = "dummy"
        exit function
    end if
    dim nkey as integer

    nkey = asc(key) - asc("A") + 1

    Get_variable = vars(nkey)
    'print "Get *************** ";nkey;" to ";vars(nkey); " nkey is ";nkey

end function

sub Release_variable_store()
 '   Tree_Free(pTree)
End sub


' here we go, the gettok function is the so-called lexical scanner.

function gettok() as string
    dim collect as string
    dim b as string
    dim b1 as string
    dim i as integer
    
    do while left(buff, 1) = " "
        buff = mid(buff, 2)
    loop
    
    if buff = "" then
        ' console.writeline("~")
        gettok = "~"
        exit function
    end if
    
    collect = ""

    b = left(buff,1)
    b1 = mid(buff, 2, 1)
    
    if b = "<" and b1 = "=" then
        buff = mid(buff, 3)
        ' console.writeline(b + b1)
        gettok = b + b1
        exit function
    end if
    
    if b = "<" and b1 = ">" then
        buff = mid(buff, 3)
        ' console.writeline(b + b1)
        gettok = b + b1
        exit function
    end if
    
    if b = "<" then
        buff = mid(buff, 2)
        ' console.writeline(b)
        gettok = b
        exit function
    end if
    
    if b = ">" and b1 = "=" then
        buff = mid(buff, 3)
        ' console.writeline(b + b1)
        gettok = b + b1
        exit function
    end if
    
    if b = ">" then
        buff = mid(buff, 2)
        ' console.writeline(b)
        gettok = b
        exit function
    end if
    
    for i = 1 to ops
            ' console.writeline("checking for :" + b + ": against :" + op(i)+":")
        if b = op(i) then
        ' console.writeline("found " + b)
           ' console.writeline(b)
            gettok = b
            buff = mid(buff, 2)
            exit function
        end if
    next i

    ' console.writeline(b + " is not an op")
    
    if b >="A" and b <= "Z" then
        do while b >= "A" and b <= "Z" or b >= "0" and b <= "9"
            collect = collect + b
            buff = mid(buff, 2)
            b = left(buff, 1)
        loop
        ' console.writeline(collect)
        gettok = collect
        exit function
    end if
    
    'print "processing "; "b is ";b; "  buff is ";buff;"  collect is ";collect
    if b >= "0" and b <= "9" then
        do while b >= "0" and b <= "9"
            collect = collect + b
            buff = mid(buff, 2)
            b = left(buff, 1)
        loop
        'print "numeric token ";collect
        ' console.writeline(collect)
        gettok = collect
'        Console.WriteLine("number token is " + collect)
        exit function
    end if
    
    if asc(b) = 34 then ' quote mark
        collect = b
        buff = mid(buff, 2)
        b = left(buff, 1)
        do while Asc(b) <> 34
            collect = collect + b
            buff = mid(buff, 2)
            b = left(buff, 1)
        loop
        ' console.writeline(collect + chr(34))
        gettok = collect + chr(34)
        buff = mid(buff, 2)
        
'        Console.WriteLine("text token is " + collect + chr(0x22))
        
        exit function
    end if
    
    gettok = "~":  rem end of file token
    
end function

' The remainder of the program constitutes a recursive descent parser.  Each function can be
' viewed as a production in a context free grammar.  A CFG is a grammar where you can predict what
' is coming based on looking at the next symbol.  High precedence operators appear first in the function
' list.  as you read down the code, you'll see lower and lower precedence operators.

' A factor is a high precedence bundle of tokens. An identier, a number, a parenthesized expression,
' or a unary minus followed by an expression. you will see tok, and rpn in all the following functions. 
' each function returns the rpn of the converted expression fragment.  


function parse_factor() as string
    dim tok as string
    dim rpn as string
'    Console.WriteLine("Enter parse_factor with buff = '" + buff +"'")
    tok = gettok()

    if tok= "(" then
        
        rpn = parse_or()
        tok = gettok()
        
        if tok <> ")" then
            print "Missing ) before "+buff
            buff = ""
            parse_factor = "~"
            exit function
        end if
        parse_factor = rpn
        exit function
    end if
    
    if tok = "@" then
    
        tok = gettok()
    
        if tok <> "(" then
            print "Error in let atsign"
            buff = ""
            parse_factor = "~"
            exit function
        end if
        
        rpn = parse_or()
        
        tok = gettok()
        
        if tok <> ")" then
            print "Missing ) before "+buff
            buff = ""
            parse_factor = "~"
            exit function
        end if
  ' Console.WriteLine("@ evaluated to "+rpn+"GETAT")
        parse_factor = rpn + "GETAT "
        Exit Function
        

 
    end if
    
    if tok = "-" then ' I think this is uneccesary after review
        rpn = parse_or()
        parse_factor = rpn + "UNM "
        exit function
    end if
    
'    Console.WriteLine("token added to rpn is " + tok + " ")
    rpn = tok + " "
    parse_factor = rpn
'    Console.WriteLine("Exit parse_factor with buff = '" + buff +"'")
end function

function parse_exponent() as string
    dim tok as string
    dim rpn as string
    
    rpn = parse_factor()
    tok = gettok()

    do while tok = "^"
        rpn = rpn + parse_exponent () + tok
        tok = gettok()
    loop

    buff = tok + " " + buff
    parse_exponent = rpn    
end function

' parse a unary minus which has less precedence than exponentiation
' that's why unary - is checked here before exponential expressions above
'
' in the rpn we use a separate designation for unary minus (UNM)
' this makes it easy for the vm to understand the different
' interpretations


function parse_unary_minus() as string
    dim tok as string
    dim rpn as string
    dim neg as integer
    
    neg = false
    
    tok = gettok()
    do while tok = "-" or tok = "+"
        if tok = "-" then
            if neg = false then
                neg = true
            elseif neg = true then
                neg = false
            end if
        end if
        tok = gettok()
    loop
    
    buff = tok + " " + buff
    
    rpn = parse_exponent()
    
    if neg = true then
        rpn = rpn + "UNM "
    end if
    
    parse_unary_minus = rpn
    
end function

function parse_term ()as string
    dim tok as string
    dim rpn as string
    
    rpn = parse_unary_minus()
    tok = gettok()
    
    do while tok = "*" or tok = "/"
        rpn = rpn + parse_unary_minus() + tok
        tok = gettok()
    loop
    
    buff = tok + " " + buff
    parse_term = rpn
    
end function



function parse_expr () as string
    dim tok as string
    dim rpn as string
    
    rpn = parse_term()
    tok = gettok()
    
    do while tok = "+" or tok = "-"
        rpn = rpn + parse_term() + tok
        tok = gettok()
    loop
    
    buff = tok + " " + buff
    
    parse_expr = rpn
    
end function

function parse_condition () as string
    dim tok as string
    dim rpn as string
    
    rpn = parse_expr()
    tok = gettok()
    
    if left(tok, 1) = "=" or left(tok, 1) = ">" or left(tok, 1) = "<" then
        rpn = rpn + parse_expr() + tok
        tok = gettok()
    end if
    
    buff = tok + " " + buff
    parse_condition = rpn
    
end function

function parse_and () as string
    dim tok as string
    dim rpn as string
    
    rpn = parse_condition()
    tok = gettok()
    
    do while tok = "AND"
        rpn = rpn + parse_condition() + tok + " "
        tok = gettok()
    loop
    
    buff = tok + " " + buff
    parse_and = rpn
    
end function

function parse_or () as string
    dim tok as string
    dim rpn as string
'    Console.WriteLine("Entering parse_or with buff = " + buff)
    rpn = parse_and()

    tok = gettok()
    
    do while tok = "OR"
        rpn = rpn + parse_and() + tok + " "
        tok = gettok()
    loop
    
    buff = tok + " " + buff
    parse_or = rpn
    
    buff_after_parse_or = buff
    
'    Console.WriteLine("Leaving parse_or with buff = " + buff)
    
end function

