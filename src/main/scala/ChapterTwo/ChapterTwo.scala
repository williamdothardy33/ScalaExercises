package ChapterTwo
import scala.annotation.tailrec
object ChapterTwoExercises:
    def fib(index: Int): Int =
        @tailrec def go(index: Int, currentFibonacci: Int, nextFibonacci: Int): Int =
            if index <= 0 then currentFibonacci
            else go(index - 1, nextFibonacci, currentFibonacci + nextFibonacci)
        go(index, 0, 1)

    def formatResult(name: String, input: Int, f: Int => Int): String =
        val msg = "The %s of %d is %d."
        msg.format(name, input, f(input))

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
        @tailrec def go(index: Int): Boolean =
            if index > as.length - 1 then true
            else if !ordered(as(index - 1), as(index)) then false
            else go(index + 1)
        go(1)
    /*I need an a to partially apply to f*/
    def partial1[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

    /*I think the type of my lambda function's parameters on L.H.S are inferred from the types of the parameters on R.H.S*/
    def partial2[A, B, C, D](a: A, f: (A, B, C) => D): (B, C) => D = (b, c) => f(a, b, c)
    /* *If I get an a partially apply it to f*/
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => partial1(a, f)

    def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = (a, b) => f(a)(b)

    def compose[A, B, C](g: B => C, f: A => B): A => C = a => g(f(a))

object TestFib:          
    import ChapterTwoExercises.fib
    @main def printFib: Unit =
        println("Expected: 0, 1, 1, 2, 3, 5, 8")
        println("Actual:   %d, %d, %d, %d, %d, %d, %d".format(fib(0), fib(1), fib(2), fib(3), fib(4), fib(5), fib(6)))

object TestIsOrdered:
    import ChapterTwoExercises.isSorted
    @main def printIsSorted: Unit =
        val isIncreasingInt: (Int, Int) => Boolean = (x: Int, y: Int) => x < y
        val isIncreasingString: (String, String) => Boolean = (s1: String, s2: String) => s1.length() < s2.length()
        val isDecreasingInt: (Int, Int) => Boolean = (x: Int, y: Int) => x > y
        val res1 = isSorted[Int](Array(1,2), isIncreasingInt) //true
        val res2 = isSorted[Int](Array(1,2,3,4,5), isDecreasingInt) //false
        val res3 = isSorted[String](Array("ab","bcd", "cdef", "defgh"), isIncreasingString) //true
        val res4 = isSorted[String](Array("de","ab"), isIncreasingString) //false
        println("Expected: true, false, true, false")
        println("Actual: %b, %b, %b, %b".format(res1, res2, res3, res4))