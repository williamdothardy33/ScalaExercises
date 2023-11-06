for
    case Some(x) <- List(Some(1),Some(2),None,Some(4))
yield 2 * x

trait Generator[+T]:
    self =>
        def generate: T
        def map[V](f: T => V): Generator[V] = 
            new Generator[V]:
                def generate: V = f(self.generate)

        def flatMap[V](f: T => Generator[V]): Generator[V] =
            new Generator[V]:
                def generate: V = f(self.generate).generate

object Generator:
    def apply[A](f: => A) =
        new Generator[A]:
            def generate: A = f

    def batched[A](gs: List[Generator[A]]): Generator[List[A]] =
        new Generator[List[A]]:
            def generate: List[A] = gs.map(g => g.generate)

val integers = Generator(scala.util.Random.nextInt())

def pairs[U, V](genU: Generator[U], genV: Generator[V]): Generator[(U, V)] =
    for 
        u <- genU
        v <- genV
    yield (u, v)

def single[A](a: A): Generator[A] =
    new Generator[A]:
        def generate: A = a

def interval(startInclusive: Int, endExclusive: Int): Generator[Int] =
    for 
        int <- integers
    yield
        startInclusive + Math.abs(int) % (endExclusive - startInclusive)

def oneOf[A](as: A*): Generator[A] =
    for 
        index <- interval(0, as.length)
    yield
        as(index)

val booleans =
    for 
        int <- integers
    yield
        int >= 0

/*def nonEmptyIntList(maxSize: Int): Generator[List[Int]] =
    val size = interval(1, maxSize).generate
    Generator.batched(List.fill(size)(integers))*/

def nonEmptyIntListGen: Generator[List[Int]] =
    for 
        head <- integers
        tail <- list
    yield
        head :: tail

//val smallIntListGen = nonEmptyIntList(10)

val emptyListGen = single(List.empty)

def list: Generator[List[Int]] =
    for 
        threshold <- interval(1, 100) 
        list <- if 50 >= threshold then emptyListGen else nonEmptyIntListGen
    yield list



val l = list.generate

val intPairs = pairs(integers, integers)

intPairs.generate

var x = 0;
val updateX = {x = x + 1; println(x)}
val runCondition = x < 10

enum IntTree:
    case Inner(left: IntTree, right: IntTree)
    case Leaf(x: Int)

def intLeaf: Generator[IntTree] =
    for 
        x <- integers
    yield
        IntTree.Leaf(x)

def intTree: Generator[IntTree] =
    for 
        isLeaf <- booleans
        tree <- if isLeaf then intLeaf else intInner
    yield
        tree

def intInner: Generator[IntTree] =
    for 
        subtree1 <- intTree
        subtree2 <- intTree
    yield
        IntTree.Inner(subtree1, subtree2)

val t = intTree.generate

def While(condition: => Boolean)(continuation: => Unit): Unit =
    if condition then 
        continuation
        While(condition)(continuation)
    else ()

enum JSON:
    case Null
    case JObject(bindings: Map[String, JSON])
    case JArray(elements: List[JSON])
    case JString(value: String)
    case JNumber(value: Double)
    case JBoolean(value: Boolean)

def quoted(jString: String): String = {
        jString match
            case str => s"\"${str}\""
}
    

val jsonExample =
    """{"firstName" : "John",
        "lastName" : "Smith",
        "address" : {
            "streetAddress" : "21 2nd Street",
            "state", "NY",
            "postalCode" : 10021
        },
        "phoneNumbers" : [
            {"type" : "home",
            "number" : "212 555-1234"},
            {"type" : "fax",
            "number" : "646 555-4567"}
        ]
    }"""

val jsonData = JSON.JObject(Map(
    "firstName" -> JSON.JString("John"),
    "lastName" -> JSON.JString("Smith"),
    "address" -> JSON.JObject(
        Map("streetAddress" -> JSON.JString("21 2nd Street"),
        "state" -> JSON.JString("NY"),
        "postalCode" -> JSON.JNumber(10021))
    ),
    "phoneNumbers" -> JSON.JArray(List(
        JSON.JObject(Map(
            "type" -> JSON.JString("home"),
            "number" -> JSON.JString("212 555-1234")
        )),
        JSON.JObject(Map(
            "type" -> JSON.JString("fax"),
            "number" -> JSON.JString("646 555-4567")
        )))
    )
))    

def show(json: JSON): String =
    json match
        case JSON.Null => "null" 
        case JSON.JObject(bindings) =>
            bindings.toList.map {
                (k, v) => s"${quoted(k)} : ${show(v)}"
            }.mkString("{\n", ", \n", "\n}")
        case JSON.JArray(elements) =>
            elements.map(show).mkString("[\n", ",\n", "\n]")
        case JSON.JString(value) => quoted(value.toString())
        case JSON.JNumber(value) => value.toString()
        case JSON.JBoolean(value) => value.toString()
    
show(jsonData)

val phoneNumberRegex = "^212"

def bindingsOf(jSon: JSON): List[(String, JSON)] =
    jSon match
        case JSON.JObject(bindings) => bindings.toList
        case _ => Nil

def findPhoneNumbers(jSon: JSON): List[JSON] =
    for 
        case (phoneNumberRegex, json) <- bindingsOf(jSon)
    yield json
    



def numsStartWith(prefix: String, jSon: JSON): List[String] =
    for
        case ("phoneNumbers", JSON.JArray(numberInfoObjs)) <- bindingsOf(jSon)
        numberInfoObj <- numberInfoObjs
        case ("number", JSON.JString(phoneNumber)) <- bindingsOf(numberInfoObj)
        if phoneNumber.startsWith(prefix)
    yield phoneNumber

/*bindingsOf(jsonData).filter(binding =>  binding._1 == "phoneNumbers")
.map(binding => binding._2).map(ja => ja match
    case JSON.JArray(elements) => elements
    case _ => Nil).flatten.map(jo => bindingsOf(jo)).flatten.filter(t => t._1 == "number").filter(t => t match
        case (_, JSON.JString(phoneNumber)) => phoneNumber.startsWith("212") 
    )*/


numsStartWith("212", jsonData)

case class Book(title: String, authors: List[String])

val books: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs",
    authors = List("Abelson, Harald", "Sussman, Gerald J.")),

    Book(title = "Introduction to Functional Programming",
    authors = List("Bird, Richard", "Wadler, Phil")),

    Book(title = "Effective Java",
    authors = List("Bloch, Joshua")),

    Book(title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Neal")),

    Book(title = "Programming in Scala",
    authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
)

for 
    case Book(title, authors) <- books
    case author <- authors
    if author.contains("Bird")
yield title

for 
    book <- books
    if book.title.contains("Programming")
yield book

val items = List(List(1,2) -> "a", List(1,5) -> "b", List(3,8,5,10) -> "c")

val ProductofitemsXitems1: List[(String, String)] =
    for first <- items
        second <- items
    yield (first._2, second._2)

val ProductofitemsXitems2: List[List[String]] =
    for first <- items
        second <- items
    yield List(first._2, second._2)

val ProductofitemsXitems3: Set[List[String]] =
    for first <- items.toSet
        second <- items
    yield List(first._2, second._2)

/**
    * the inner set collapses the tuple so the product doesn't contain duplicates
    * the outer set collapses the sets so that permutations of the tuple are eliminated
    */

val ProductofitemsXitemsNoDuplicates3: Set[Set[String]] =
    for first <- items.toSet
        second <- items
    yield Set(first._2, second._2)

val sameKey =
    for 
        first <- items.toSet
        second <- items
        key1 <- first._1
        key2 <- second._1
        if first != second && key1 == key2
    yield
        key1

for
    b1 <- books
    b2 <- books
    a1 <- b1.authors
    a2 <- b2.authors 
yield
    List(a1, a2)

for
    b1 <- books.toSet
    b2 <- books
    a1 <- b1.authors
    a2 <- b2.authors
    if b1 != b2 && a1 == a2
yield
    a1

def mapFun1[A, B](as: List[A])(f: A => B): List[B] =
    for 
        a <- `as`
    yield f(a)
mapFun1(List(1,2,3))(x => 2 * x)

/**
    * should never do this in practice but need to get handle on for comprehensions
    */

def mapFun2[A, B](as: List[A])(f: A => B): List[B] =
    as match
        case head :: next => 
            for 
                case b <- f(head) :: mapFun2(next)(f)
            yield b
        case Nil => Nil
    
mapFun2(List(1,2,3))(x => 2 * x)

/*def mapFun2[A, B](as: List[A])(f: A => B): List[B] =
    as match
        case head :: next => f(head) :: mapFun2(next)(f).map(x => x)
        case Nil => Nil
    
mapFun2(List(1,2,3))(x => 2 * x)*/

def flatMapFun1[A, B](as: List[A])(f: A => List[B]): List[B] =
    for 
        a <- `as`
        b <- f(a)
    yield b

flatMapFun1(List(1,3))(x => List(x, x + 1))

def flatMapFun2[A, B](as: List[A])(f: A => List[B]): List[B] =
    as match
        case head :: next => 
            for 
                b <- f(head) ::: flatMapFun2(next)(f)
            yield b
        case Nil => Nil
    

def filterFun[A](as: List[A])(p: A => Boolean): List[A] =
    for 
        a <- `as`
        if p(a)
    yield a

for
    b <- books
    a <- b.authors
    if (a.contains("Bird"))
yield b.title

books.filter(b => b.authors.exists(a => a.startsWith("Bird"))).map(b => b.title)

val m = Some(1)
val f = (x: Int) =>
    if x <= 1 then Some(x + 1)
    else None

val g = (x: Int) =>
    if x <= 2 then Some(x * 2)
    else None


for
    fxValue <- for x <- m; fx <- f(x) yield fx
    gfxValue <- g(fxValue)
yield gfxValue

for 
    x <- m
    fx <- f(x)
    gfx <- g(fx)
yield
    gfx

for
    x <- m
yield x

def sieve(start: LazyList[Int]): LazyList[Int] =
    start.head #:: sieve(start.tail.filter(_ % start.head != 0))

sieve(LazyList.from(2)).take(10).toList

/**
    * y/x = x => (y/x + x) /2 quit when (x * x - y) / y < .0001
    */

def closeEnough(guess: Double, target: Double): Boolean =
    Math.abs((guess * guess - target) / target) < .0001

/**
the technique here is instead of trying to compute the solution
recursively/iteratively by passing our "improved guess" into a helper/
mutating some variable with successive guesses and then testing that
guess against some criteria to stop, we instead build a list of guesses
lazily and filter that list using the criteria
    */
def sqrtGuesses(target: Double): LazyList[Double] =
    val improve = (guess: Double) => ((target / guess) + guess) / 2
    lazy val guesses: LazyList[Double] = 1 #:: guesses.map(improve)
    guesses

val tenGuesses = sqrtGuesses(2).take(10).toList

val sqrtTwo = sqrtGuesses(2).filter(closeEnough(_, 2)).head
