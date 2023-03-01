Imports System

Module Program
    
    dim temp_num as integer = 0

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

    Sub Main()

        dim rpn as string

	    Console.Write("Enter RPN string: ")
        rpn = Console.readline()
        rpn = Trim(rpn)
        process(rpn)

    End Sub

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

        Console.WriteLine("Result = " + pop())

        if eval_ptr <> 1 then
            Console.WriteLine("Stack is not empty")
        end if

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
        Console.WriteLine(temp + " = " + first + " " + op + " " + second)
        push(temp)
    end Sub

    sub process_var(var as string)
        ' Console.WriteLine("var " + var)
        ' console.WriteLine(next_temp())
        dim result as String
        dim temp as string
        temp = next_temp()
        result = temp + " = " + var
        Console.WriteLine(result)
        push(temp)
    end sub



    End Module
