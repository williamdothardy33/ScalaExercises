abstract class IntSet:
    def contains(e: Int): Boolean
    def incl(e: Int): IntSet
    def union(other: IntSet): IntSet

case object Empty extends IntSet:
    def contains(e: Int): Boolean = false

    def incl(e: Int): IntSet = NonEmpty(e, Empty, Empty)

    def union(other: IntSet): IntSet = other

case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:
    def contains(e: Int): Boolean =
        if e < elem then left.contains(e)
        else if e > elem then right.contains(e)
        else true

    def incl(e: Int): IntSet =
        if e < elem then NonEmpty(elem, left.incl(e), right)
        else if e > elem then NonEmpty(elem, left, right.incl(e))
        else this

    /**
        * this strategy is based on dfs i include element at each left subtree in the other IntSet then at each previous recursive call
        * I take right union with the computed left union and return it (when I first call right.union other will be the entire computed left of each left
        * subtree and next will start including values in the right subtrees) right.union of the next subtree will be leftUnion of the previous subtree
        * and then will will execute right.union of the previous subtree.
        *
        * @param other
        * @return
        */
    def union(other: IntSet): IntSet =
        val next = other.incl(elem)
        val leftUnion = left.union(next)
        right.union(leftUnion)
        
/**
    * if it's too hard to think about the tree of recursive calls don't do it! try to find a simpler way
    */
            

val newSet1: IntSet = Empty
val newSet2 = newSet1.incl(5)
val newSet3 = newSet2.incl(8)
val newSet4 = newSet3.incl(2)
val newSet5 = newSet4.incl(3)
val newSet6 = newSet5.incl(7)
val newSetThis = newSet6.incl(0)

val newSet7: IntSet = Empty
val newSet8 = newSet7.incl(15)
val newSet9 = newSet8.incl(18)
val newSet10 = newSet9.incl(12)
val newSet11 = newSet10.incl(13)
val newSet12 = newSet11.incl(17)
val newSetThat = newSet12.incl(10)

newSetThis.union(newSetThat)

def foo: String = throw Exception()

trait List[+T]:
    def head: T
    def tail: List[T]
    def isEmpty: Boolean
    //def nth(n: Int): T

case class Cons[T](head: T, tail: List[T]) extends List[T]:
    def isEmpty: Boolean = false

    /*def nth(n: Int): T =
        if n == 0 then this.head
        else tail.nth(n - 1)*/


case class Nil[T]() extends List[T]:
    def isEmpty = true
    def head: T = throw Exception("Empty List!")
    def tail: List[T] = throw Exception("Empty List!")
    
    /*def nth(n: Int): T = throw IndexOutOfBoundsException()*/

def nth[T](xs: List[T], n: Int): T =
    if xs.isEmpty then throw IndexOutOfBoundsException()
    else if n == 0 then xs.head
    else nth(xs.tail, n - 1)

abstract class Bool extends AnyRef:
    /**
    * basicall ifThenElse will on evaluate the t
    * by-name parameter if its ran on a True object
    */
    def ifThenElse[T](t: => T, e: => T): T
    /**
        * && methods only evaluates its by-name parameter
        * x on a true object. (on a false instance it
        * immediately returns false)
        */

    def &&(x: => Bool): Bool = ifThenElse(x, False)
    /**
        * || only evaluates is by-name parameter if the
        * method is run on a false instance
        * (on a true instance it immediately returns true)
        *
        * @param x
        * @return
        */
    def ||(x: => Bool): Bool = ifThenElse(True, x)



object True extends Bool:
    def ifThenElse[T](t: => T, e: => T): T = t

object False extends Bool:
    def ifThenElse[T](t: => T, e: => T): T = e    

True.&&(True)

trait Expr

case class Number(n: Int) extends Expr
case class Sum(leftOp: Expr, rightOp: Expr) extends Expr
case class Product(leftOp: Expr, rightOp: Expr) extends Expr
case class Var(varName: String) extends Expr

def show(expr: Expr): String =
    expr match
        case Number(n) => n.toString 
        case Product(Sum(e1, e2), rightOp) => s"(${show(e1)} + ${show(e2)}) * ${show(rightOp)}"
        case Product(leftOp, Sum(e1, e2)) => s"${show(leftOp)} * (${show(e1)} + ${show(e2)})" 
        case Product(leftOp, rightOp) => s"${show(leftOp)} * ${show(rightOp)}"
        case Sum(leftOp, rightOp) => s"${show(leftOp)} + ${show(rightOp)}"
        case Var(varName) => varName
    
val test1 = Product(Sum(Product(Number(2), Var("x")), Var("y")), Number(3))
show(test1)

val test2 = Product(Sum(Number(2), Var("x")), Var("y"))
show(test2)

/**
    * motivating example for aliasing this (documentation calls this self aliasing) consider a function that turns an A to an Option[B] (endoFunctor?) and
    * the composition of 2 such function (Kleisli Composition)
    * 
    */

def myTry[A](a: => A): Option[A] =
    try
        Some(a)
    catch
        case e: Exception => None
/**
    * The below andThen method definition has a compile time error because
    * This refers to the anonymous object conforming to the trait OptionFunction we wanted "this"" to refer to the object that we invoke
    * and then on, but "this"" is referring to the object returned from andThen. So we need to alias "this" to the object that is calling andThen 
    */
/*trait OptionFunction[-A, +B]:
    def apply(a: A): Option[B]


    def andThen[B, C](that: OptionFunction[B, C]): OptionFunction[A, C] =
        new OptionFunction[A, C] {
            def apply(a: A): Option[C] = ???
                this.apply(a) match
                    case None => None
                    case Some(value) => that.apply(value)
        }*/
/**
    * the following are equivalent
    */

trait A:
    self =>

trait B:
    private val self = this

trait OptionFunction[-A, +B]:
    self =>
        def apply(a: A): Option[B]
        
        def andThen[C](that: OptionFunction[B, C]): OptionFunction[A, C] =
        new OptionFunction[A, C] {
            def apply(a: A): Option[C] =
                self.apply(a) match
                    case None => None
                    case Some(value) => that.apply(value)
        }

object StringToIntOption extends OptionFunction[String, Int]:
    override def apply(a: String): Option[Int] =
        myTry(a.toInt)

object ReciprocalOption extends OptionFunction[Int, Double]:
    override def apply(a: Int): Option[Double] =
        myTry(1 / a.toDouble)
/*usage of motivating example*/

val n = StringToIntOption
val nr = n.andThen(ReciprocalOption)

val test = nr("2")
