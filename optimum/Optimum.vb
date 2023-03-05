Imports system
Imports System.io

module trips

dim buff as string = ""

const ops as integer = 10
dim op() as string
dim temp_num as integer
  
Sub Main()

dim rpn as string
 
  op = new string(ops) { "dummy for 0" , "+", "-", "*", "/", "(", ")", "=", "^", "@", "," }
  
  while true

      Console.Write("expr> ")
      
      buff = Console.readline()
 '     vartbl_ptr = 0
    outp = ""
        temp_num = 0  
      rpn = parse_or()

      process(rpn)
      Console.Write(outp)
      Console.WriteLine("Result = " + pop())
   end while
 
end sub

function gettok() as string
    dim collect as string
    dim b as string
    dim b1 as string
    dim i as integer
    
    while left(buff, 1) = " "
        buff = mid(buff, 2)
    end while
    
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
    
    if b >="a" and b <= "z" then
        while b >= "a" and b <= "z" or b >= "0" and b <= "9"
            collect = collect + b
            buff = mid(buff, 2)
            b = left(buff, 1)
        end while
        ' console.writeline(collect)
        gettok = collect
        exit function
    end if
    
    if b >= "0" and b <= "9" then
        while b >= "0" and b <= "9"
            collect = collect + b
            buff = mid(buff, 2)
            b = left(buff, 1)
        end while
        ' console.writeline(collect)
        gettok = collect
'        Console.WriteLine("number token is " + collect)
        exit function
    end if
    
    if asc(b) = 34 then ' quote mark
        collect = b
        buff = mid(buff, 2)
        b = left(buff, 1)
        while Asc(b) <> 34
            collect = collect + b
            buff = mid(buff, 2)
            b = left(buff, 1)
        end while
        ' console.writeline(collect + chr$(34))
        gettok = collect + chr(34)
        buff = mid(buff, 2)
        
'        Console.WriteLine("text token is " + collect + chr$(0x22))
        
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
            Console.WriteLine("Missing ) before "+buff)
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
            Console.WriteLine("Error in let atsign")
            buff = ""
            parse_factor = "~"
            exit function
        end if
        
        rpn = parse_or()
        
        tok = gettok()
        
        if tok <> ")" then
            Console.WriteLine("Missing ) before "+buff)
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
    
'    Console.WriteLine("Leaving parse_or with buff = " + buff)
    
end function

    dim eval_stack(100) as string
    dim eval_ptr as integer = 1

    Sub push(item as string)
        ' console.WriteLine("item push " + item + " at eval_ptr" + str$(eval_ptr) )
        eval_stack(eval_ptr) = item
        eval_ptr = eval_ptr + 1
    end sub

    Function pop() as string
        eval_ptr = eval_ptr - 1
        ' Console.Write("Item pop "+ eval_stack(eval_ptr))
        return eval_stack(eval_ptr)
    end function

    'Sub Main()

 '       dim rpn as string

'	    Console.Write("Enter expression: ")
 '       rpn = Console.readline()
  '      rpn = Trim(rpn)
   '     process(rpn)

    'End Sub

    dim outp as string = ""

    Sub process(rpn as string)
        dim token as string
        while len(rpn) <> 0
            
            while left(rpn, 1) = " "
                rpn = mid(rpn, 2)
            end while

            token = left(rpn, 1)

            rpn = mid$(rpn, 2)

            process_token(token)

        end while

        ' dim i as integer
        ' console.writeline("eval_ptr is "+str$(eval_ptr))
        ' for i = 1 to eval_ptr - 1
        '    console.writeline("i is"+str$(i))
        '     Console.Write(eval_stack(i))
        ' next i

        ' while eval_ptr >= 1
        '     console.write("poping " + pop())
        ' end while

 

    End Sub

    function next_temp() as string

        temp_num = temp_num + 1
        dim result = "t" + mid$(str$(temp_num),2)
        return result

    end function

    function use_temp(num as integer)
        Dim result = "t" + mid$(str$(num),2)
        return result
    end function

    Sub process_token(token as string)
        if token = "+" or token = "-" or token = "*" or token = "/" then
            process_op(token)
        else
            process_var(token)
        end if
    End Sub

    Sub process_op(op as string)
        ' dim second as string = use_temp(temp_num - 1) + " " + op " " + use_temp(temp_num)
        ' dim first as string = 
        ' dim result as string
        dim temp as string

        temp = next_temp()

        dim second as string = pop()
        dim first as string = pop()
        outp = outp + temp + " = " + first + " " + op + " " + second + vbCrlf
        push(temp)
    end Sub

    'structure var_record 
     '   dim t as integer
      '  dim vname as string
    ' end structure 

    'dim vartbl(100) as var_record
    ' dim vartbl_ptr as integer 

    sub process_var(var as string)
        ' Console.WriteLine("var " + var)
        ' console.WriteLine(next_temp())

        ' temp = next_temp()
        ' result = temp + " = " + var
    '    vartbl_ptr = vartbl_ptr + 1
     '   vartbl(vartbl_ptr).t = temp_num
     '   temp_num = temp_num + 1

      '  vartbl(vartbl_ptr).vname = var
         
        push(var)
    end sub






end module
