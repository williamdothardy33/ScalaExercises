package TypeClassPractice
import ChapterTwo.ChapterTwoExercises.{compose}

trait Functor[F[_]]:
    extension [A](functor: F[A])
        def fmap[B](f: A => B): F[B]

/*given ListFunctor: Functor[List] with
    extension[A](xs: List[A])
        def fmap[B](f: A => B): List[B] = xs.map(f)*/

case class Const[C, A](c: C)

given ConstFunctor[C]: Functor[[T] =>> Const[C, T]] with
    extension[A](cf: Const[C, A])
        def fmap[B](f: A => B): Const[C, B] =
            Const[C, B](cf.c)

val myConst: Const[Int, String] = Const(3)

val myConst1: Const[Int, Boolean] = myConst.fmap(s => s.length() < 3)

val myLength: [A] => List[A] => Const[Int, A] = [A] =>
    (l: List[A]) => l match
        case head :: next => Const(1 + (myLength(next) match
            case Const(c) => c
        ))
        case Nil => Const(0)

val myList = List(1,2,3,4,5,6,7)

val testLength = myLength(myList)


def polyCompose[A, B](f: [R] => (R => A), g: A => B): [R] => (R => B) =
    [R] => (r: R) => g(f(r))

val xtoString: [R] => (R => String) = [R] => (r: R) => r.toString()

val strLength: String => Int = (s: String) => s.length()

val composed = polyCompose(xtoString, strLength)
val listStringLength = composed[List[Int]]
val myListStringLength = listStringLength(myList)

given ReaderFunctor: Functor[[T] =>> [R] => R => T] with
    extension[A](f: [R] => R => A)
        def fmap[B](g: A => B): [R] => R => B =
            polyCompose(f, g)

val testReader: [R] => (R => Int) = [R] => (r: R) => {
    println(r)
    100
}

val newTestReader = testReader.fmap(a => a * 2)
val readerImpl = newTestReader[List[Int]]
val readerValue = readerImpl(myList)
/*case class Reader[R, A](f: R => A)

given ReaderFunctor[R]: Functor[[T] =>> Reader[R, T]] with
    extension[A](rf: Reader[R, A])
        def fmap[B](f: A => B): Reader[R, B] =
            Reader(f compose rf.f)*/
/**
  * difference between type lambda and polymorphic lambda function. type lambdas are called within type expressions
  * by passing in a type (level zero in this case and returning value types I think aka types that can be assigned values)
  * while polymorphic lambda functions "fix" our function to work on a specific type when a type is passed in. So when we pass
  * in a type to a polymorphic lambda function we get a "function value" back I think. type lambdas are function on types
  * i.e type lambda takes a type and gives back a type, while polymorphic lambda functions are function on values i.e polymorphic lambda function
  * takes a type and gives back a "function value" (a regular lambda function)
  */

trait Reversible:
    type L = [T] =>> List[T]
    def reverse[T](xs: L[T]): L[T]

class Reverse extends Reversible:
    type IntList = L[Int]
    def reverse[T](xs: List[T]): List[T] = xs.reverse


//val ra: Reader[Int, Int] = Reader(x => 2 * x)
/**
  * fully curried function from Type and List[T] to List[T] (if you give it a type it returns function partially applied to type)
  */
val reverseListOf: [T] => List[T] => List[T] = [T] => (xs: List[T]) => xs.reverse 
val reverseInts = reverseListOf[Int]
val reversed = reverseInts(List(1,2,3))

//val rh = ra.fmap(_.toString())

trait Monoid[M]:
    def mempty: M
    extension (m: M)
        def mappend(n: M): M

trait Monad[M[_]]:
    def unit[A](a: A): M[A]
    extension [A](m: M[A])
        def combine[B](f: A => M[B]): M[B]
        


case class Writer[A, S](a: A, s: S)


given WriterMonad[S]: Monad[[A] =>> Writer[A, S]] with
    def unit[A](a: A): Writer[A, S] = ???
    extension [A](m: Writer[A, S]) def combine[B](f: A => Writer[B, S]): Writer[B, S] = ???




