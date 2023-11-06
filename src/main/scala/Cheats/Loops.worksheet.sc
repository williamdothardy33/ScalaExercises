def While(condition: => Boolean)(continuation: => Unit): Unit =
    if condition then 
        continuation
        While(condition)(continuation)
    else ()

def repeatUntil(command: => Unit)(condition: => Boolean): Unit = While(!condition)(command)

var i = 0
repeatUntil {
    i = i + 1
    println(i)
}(i >= 10)
i

class Until(body: => Unit):
    infix def until(condition: => Boolean): Unit =
        if !condition then 
            body
            until(condition)

def repeat(body: => Unit) = Until(body)
var j = 0
repeat {
    j = j + 1
} until (j >= 10)

j