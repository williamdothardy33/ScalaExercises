import Show.getShow
import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
/**
    * anything that hits the else branch has work to do after the base case gets done and popped off the call stack
    *
    * @param n
    */
def test(n: Int): Unit =
    if n == 1 then println(1)
    else 
        println(n)
        test(n - 1)
        println(n)
/**
    * calling test1 after printing basically makes it repeat all the steps to get back to the stack frame before it can get popped
    */

def test1(n: Int): Unit =
    if n == 1 then println(1)
    else
        test1(n - 1)
        println(n)
        test1(n - 1)

test(3)
test1(4)

def mergeSort(l: Vector[Int]): Vector[Int] =
    if l.isEmpty || l.tail.isEmpty then l
    else
        val mid = l.length / 2
        val split = l.splitAt(mid)
        val left = mergeSort(split._1)
        val right = mergeSort(split._2)
        merge(left, right)

def merge(l1: Vector[Int], l2: Vector[Int]): Vector[Int] =
    def go(l1Left: Vector[Int], l2Left: Vector[Int], result: Vector[Int]): Vector[Int] =
        (l1Left, l2Left) match
            case (as:+a, bs:+b) if (a > b) => go(as, l2Left, a+:result)
            case (as:+a, bs:+b) if (b > a) => go(l1Left, bs, b+:result)
            case (as:+a, bs:+b) if (b == a) => go(as, bs, a+:b+:result)
            case (l1, l2) if l2.isEmpty => l1 ++: result
            case (l1, l2) if l1.isEmpty => l2 ++: result
            case _ => result
    go(l1, l2, Vector())

merge(Vector(8,9,10, 15), Vector(1,3,11))
            
mergeSort(Vector(8, 11, 2, 100, 27, 3, 8, 5,9))

enum Tree[+A]:
    case Leaf(value: A)
    case Branch(value: A, left: Tree[A], right: Tree[A])

/*object Tree:
    Apply()*/

val l1 = Tree.Leaf(1)
val l2 = Tree.Leaf(2)
val l3 = Tree.Leaf(6)
val l4 = Tree.Leaf(7)

val b1 = Tree.Branch(3, l1, l2)
val b2 = Tree.Branch(5, l3, l4)
val r = Tree.Branch(4, b1, b2)

def printPostOrder(t: Tree[Int]): Unit =
    t match
        case Tree.Leaf(value) => println(value)
        case Tree.Branch(value, left, right) => {
            printPostOrder(left)
            printPostOrder(right)
            println(value)
        }

printPostOrder(r)
    

def dfs(t: Tree[Int]): Unit =
    t match
        case Tree.Leaf(value) => println(value)
        case Tree.Branch(value, left, right) => {
            println(value)
            dfs(left)
            dfs(right)
        }

dfs(r)
    
//to figure out a recursive solution you need to figure out what you want to have at the stack frame right above the base case and the base case
def treeSum(t: Tree[Int]): Int =
    t match
        case Tree.Leaf(value) => value
        case Tree.Branch(value, left, right) => {
            val leftSum = treeSum(left)
            val rightSum = treeSum(right)
            value + leftSum + rightSum
        }
    
treeSum(r)


def steps(n: Int, atATime: List[Int], callStack: Stack[String]): Int =
    callStack.push(s"$n function call with parameters of n: $n and atATime: $atATime")
    println()
    println(callStack)
    println()
    println()
    var count = 0
    if n == 0 then 1
    else
        var nAtATime = atATime
        while !nAtATime.isEmpty
        do
            val k = nAtATime.head
            if n - k >= 0 then
                count += steps(n - k, atATime, callStack)
            nAtATime = nAtATime.tail
        val popped = callStack.pop()
        println(s"$n popped: $popped")
        println()
        println(callStack)
        println()
        println()
        count
            
    
steps(10, List(2, 4, 5, 8), Stack())


def steps1(n: Int): String =
    var count = " "
    if n == 0 then " cha ching!"
    else
        var k = n
        while k >= 0
        do
            count += steps1(n - 1)
            k -= 1
        count
            
steps1(4)

case class RecursionTree[A](var calledWith: String = "", var returnedValue: Option[A] = None, var madeCallsTo: ListBuffer[RecursionTree[A]] = ListBuffer[RecursionTree[A]]())

def steps2(n: Int, atATime: List[Int], tree: RecursionTree[Int]): Int =
    tree.calledWith = s"steps($n)"
    var count = 0
    if n == 0 then 1
    else
        var nAtATime = atATime
        while !nAtATime.isEmpty
        do
            val k = nAtATime.head
            if n - k >= 0 then
                var branch: RecursionTree[Int] = RecursionTree()
                tree.madeCallsTo += branch
                count += steps2(n - k, atATime, branch)
            nAtATime = nAtATime.tail
            
        tree.returnedValue = Some(count)
        count

def steps3(n: Int, atATime: List[Int], cache: HashMap[Int, Int]): Int =
    var count = 0
    if cache.get(n).isDefined then {
        println(s"retrieved ${cache.get(n)} from cache with key $n")
        cache.get(n).getOrElse(0)  
    } 
    else if n == 0 then {
        println(s"add value 1 to cache with key $n")
        cache += (n -> 1)
        1
    }
    else
        var nAtATime = atATime
        while !nAtATime.isEmpty
        do
            val k = nAtATime.head
            if n - k >= 0 then
                count += steps3(n - k, atATime, cache)
            nAtATime = nAtATime.tail
        cache += (n -> count)
        println(s"add value $count to cache with key $n")
        count

steps3(10, List(2, 4, 5, 8), HashMap())



def printTree(tree: RecursionTree[Int], indent: String = ""): Unit =
    val INDENT_SIZE = 4
    if tree.madeCallsTo.isEmpty then
        println(s"${tree.calledWith} returned ${tree.returnedValue}")
    else
        println(s"${tree.calledWith} returned ${tree.returnedValue}")
        val last = tree.madeCallsTo.last
        tree.madeCallsTo.init.foreach {
            case child => {
                print(s"${indent}|${"-" * INDENT_SIZE}")
                printTree(child, s"${indent}|${" " * INDENT_SIZE}")
            }
        }
        print(s"${indent}L${"-" * INDENT_SIZE}")
        printTree(last, s"${indent}${"   " * INDENT_SIZE}")

def search(as: Vector[Int], searchValue: Int, leftEndpoint: Int, rightEndpoint: Int): Int =
    val mid = (leftEndpoint + rightEndpoint) / 2
    val midValue = as(mid)
    if leftEndpoint > rightEndpoint then -1
    else if searchValue == midValue then mid
    else if searchValue > midValue then search(as, searchValue, mid + 1, rightEndpoint)
    else search(as, searchValue, leftEndpoint, mid - 1)

def binarySearch(sorted: Vector[Int], searchValue: Int): Int =
    search(sorted, searchValue, 0, sorted.length - 1)

binarySearch(Vector(5,8,11, 19, 20, 25, 32, 55, 72, 73, 74, 75, 76, 77, 78, 79, 80), 73)

def pascal(row: Int, col: Int): Int =
    if row == col then 1
    else if row >= col && row >= 0 && col >= 0 then
        pascal(row -1, col - 1) + pascal(row - 1, col)
    else 0


pascal(3, 1)
pascal(3, 3)
pascal(0, 2)
pascal(2, 2)

/*def isBalanced(chars: List[Char]): Boolean =
    if chars.isEmpty then true*/


def countChange(money: Int, coins: List[Int]): Int =
    var count = 0
    if money == 0 then 1
    else
        var coinsLeft = coins
        while !coinsLeft.isEmpty
        do
            val coin = coinsLeft.head
            if money - coin >= 0 then
            count += countChange(money - coin, coinsLeft)
            coinsLeft = coinsLeft.tail
        count


countChange(4,List(1,2))
countChange(300,List(5,10,20,50,100,200,500))

def balance(chars: List[Char]): Boolean =
    def go(opened: List[Char], left: List[Char]): Boolean =
        if left.isEmpty then opened.isEmpty
        else if left.head == '(' then go(left.head +: opened, left.tail)
        else if left.head == ')' && !opened.isEmpty then go(opened.tail, left.tail)
        else go(opened, left.tail)
    go(List(), chars)

balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList)
balance("())(".toList)
balance("(((((((()))())))))".toList)

def insert(x: Int, sorted: List[Int]): List[Int] =
    sorted match
        case Nil => List(x) 
        case head :: next => {
            if head < x then head :: insert(x, next)
            else x :: sorted
        }

def iSort(as: List[Int]): List[Int] =
    as match
        case head :: next => insert(head, iSort(next))
        case Nil => Nil
iSort(List(5, 3, 1,2, 8, 9, 3))

def isPrime(n: Int): Boolean = (2 to Math.sqrt(n).toInt)  forall { x => n % x != 0 }
/*isPrime(32)
isPrime(119023749)
isPrime(1234324989)
isPrime(7)*/

/*def  primePairs(n: Int): Seq[(Int, Int)] =
    (1 until n).flatMap(i => (1 until i).map(j => (i, j))).filter((x, y) => isPrime(x + y)).filter((x, y) => x + y <= n)*/

def  primePairs(n: Int): Seq[(Int, Int)] =
    for i <- (1 until n)
        j <- (1 until i)
        if isPrime(i + j)
    yield (i, j)

primePairs(7)

def scalarProduct(xs: List[Int], ys: List[Int]): Int =
    val product = for (i, j) <- xs.zip(ys)
        yield i * j
    product.sum

def isSafe(col: Int, cols: List[Int]): Boolean =
    def safeAcc(distance: Int, colsLeft: List[Int]): Boolean =
        colsLeft match
            case Nil => true
            case currentCol :: colsLeft => {
                val leftDiag = currentCol - distance
                val rightDiag = currentCol + distance
                if col != currentCol && col != leftDiag && col != rightDiag then safeAcc(distance + 1, colsLeft)
                else false
            } 
    safeAcc(1, cols)


def queens(boardSize: Int): Set[List[Int]] =
    def placeQueen(k: Int): Set[List[Int]] =
        if k == 0 then Set(List())
        else
            for soln <- placeQueen(k - 1)
                col <- 0 until boardSize
                if isSafe(col, soln)
            yield col :: soln
    placeQueen(boardSize)

val test = queens(4)
/**
    * every function/method automatically takes in the type that invokes it (similar to javascript)
    */
/**
    * although we do not explicitly say that Show type constructor is contravariant in T it's method show uses it
    * "in a contravariant position" (I think) and if we would like to impart the structure bolted-on to T on another type V we need to
    * "pull-back" the structure using f: V => T using contraMap so we do this by bolting-on the Contravariant Structure onto Show[T]. now
    * Show[T] has the "tools" to send its structure back to V from T
    */
trait Show[T]:
    extension (t: T)
        def show: String
        /*def test(i: Int): String*/

object Show:
    def getShow[T](st: Show[T]): T => String =
        st.show 
    /*def getTest[T](st: Show[T]): T => Int => String =
        st.test*/
        
    def create[T](f: T => String): Show[T] = new Show {
        extension (t: T) def show: String = f(t)
    }
/**
    * contravariant typeclass can be be thought of as a an abstraction of the  
    * "pull back" mechanism via f: B => A of some functionality that you have
    * bolted/embellished onto type A to type B (use F[A] with contraMap to get F[B]) 
    * analogously Functor can be thought of as an abstraction of the "push forward" mechanism via
    * f: A => B of some functionality that you have bolted/embellished onto type A to type B (use F[A] with
    * map to get F[B]). Going further if type A has been endowed with some structure that makes it convenient/easy to work with for my purposes
    * then map/contraMap are ways to leverage that structure of type A using
    *  f: A => B and g: C => A respectively on other types. (from reading) the type parameter of a type constructor either tells you information
    * about what methods on that type either produce (covariant type/read from method) or consume (contravariant/write to parameters) so if a type
    * constructors type param is covariant and the types produced by the type constructor is a functor then methods on those types produce the type param
    * and F[A]'s methods can be viewed as a source of A. we can use map with f: A => B to get F[B] a source of B
    * similarly if F[A]'s type argument is contravariant then the Functors produced by the type constructor are a "sink" of A or the methods of F[A] consume
    * A type. we can use contraMap along with f: B => A to get a sink of B F[B] (this is where the terminology "mapping over the output/input" comes
    * from F[+A] "outputs" A and F[A] map A => B takes the output and turns it into a B whereas F[-A] "inputs" A and F[A] contraMap B => A takes the
    * type B converts it to an A which F[A] can accept) so I think these are the general guidelines of when you can use the "push-forward" mechanism
    * vs the "pull-back" mechanism based on the "structure" of your type map f can be thought of as "push-forward with f" if F[A] is a source of A to
    * create F[B], a source of B and contraMap f can be thought of as "pull-back" with f if F[A] is a sink of A to create F[B], a sink of B, I personally
    * prefer to think of map/contraMap as a way of imparting a structure on another type based on the structure of the original type
    */
trait Contravariant[F[_]]:
    extension [A](contravariant: F[A])
        def contraMap[B](f: B => A): F[B]

case class Money(amount: Int)

given ShowMoney: Show[Money] with
    extension (s: Money)
        def show = s"$$${s.amount}"

given ContravariantShow: Contravariant[Show] with
    extension [A](cs: Show[A])
        def contraMap[B](f: B => A): Show[B] = Show.create(b => Show.getShow(cs)(f(b)))

case class Salary(size: Money)

implicit val ShowSalary: Show[Salary] = ShowMoney.contraMap(s => s.size)

val money = Money(100)
money.show

val salary = Salary(Money(1000))
salary.show
val ageOfPeople =
    Map("Alice" -> 7, "Bob" -> 11, "Carol" -> 13)

val ageOfPeople2 = ageOfPeople ++ Map("Ted" -> 10, "Alice" -> 8)

val ageOfPeople3 = ageOfPeople2.map((key, value) => (key, value + 1))

ageOfPeople3.get("Ted")

ageOfPeople3("Ted")

val dictionary = List("scala", "is", "fun")

/*def encode(phoneNumber: Char*): Either[String, Map[String, Seq[Char]]] =
    def nextEncode(left: Seq[Char], result: Map[String, Int]): Map[String, Seq[Char]] =
        left match
            case Seq(nextNum) =>
                val code = mnemonics.get(nextNum)
                code match
                    case None => Left("could not find code for number!")
                    case Some(alphaCode) =>
                        result.filter {
                            (phrase, num)
                        }*/
                
def generateWords(index: Int, code: String, dictionary: List[String]): Set[String] =
    dictionary.filter(word => code.toLowerCase().contains(word.toLowerCase().charAt(index))).toSet

val phoneNumber = Seq(7,2,2,5,2,4,7,3,8,6)
        
val testG = generateWords(3, "JKL", dictionary.filter(word => word.length() >= 4))

class Coder(words: List[String]):
    opaque type Digits = String
    opaque type Digit = Char
    opaque type Word = String
    opaque type Letter = Char
    val mnemonics = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

    private val CharCode: Map[Char, Char] = mnemonics.foldLeft(Map.empty[Char, Char]) {
        case (acc, (digit, code)) => acc ++ code.map(letter => letter -> digit)
    }

    private def wordCode(word: String): Digits = word.toUpperCase().map(CharCode)

    private val wordsForNum: Map[String, List[String]] = words.groupBy(wordCode).withDefaultValue(Nil)

    def encode(number: String): Set[List[String]] =
        val numberLength = number.length()
        if number.isEmpty() then Set(Nil)
        else
            for
                splitPoint <- (1 to numberLength).toSet
                word <- wordsForNum(number.take(splitPoint))
                rest <- encode(number.drop(splitPoint))
            yield word :: rest

    val coder = Coder(dictionary)
    val result = coder.encode("7225247386")




case class Book(title: String, rating: Double)

val books = List(
    Book("abc", 123),
    Book("abc", 1234),
    Book("ab", 12345)
)

books.sortBy(book => (book.title, book.rating))