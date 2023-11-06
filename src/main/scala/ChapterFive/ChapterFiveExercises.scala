package ChapterFive
import ChapterThree.ChapterThreeExercises._
import scala.collection.immutable.Stream.{Cons as _, Empty as _}
import ChapterFour.ChapterFourExercises._
import ChapterFour.ChapterFourExercises.Option.{None, Some}
import scala.{Some as _, None as _}
import ChapterThree.ChapterThreeExercises._
import ChapterThree.ChapterThreeExercises
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import scala.collection.immutable.List as sList

object ChapterFiveExercises:


    def diagMessage[A](messagePrefix: String, expressionDescription: String, result: A): String =
        val message = "%s %s is %f".format(messagePrefix, expressionDescription, result)
        message


    object Stream:
        import ChapterFiveExercises.Stream as Stream
        import ChapterFiveExercises.Stream.{Cons, Empty}
/**
  * Not 100% sure but I think the point of storing this as a lazy val is if hd and tl were some sort of compute intensive calculation then declaring them
  * lazy val allows for caching instead of recomputing hd and tl when the thunk is evaluated
  */
        def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
            lazy val head = hd
            lazy val tail = tl
            Cons(() => head, () => tail)

        

        def valCons[A](hd: => A, tl: Stream[A]): Stream[A] =
            val head = hd // this will evaluate hd immediately before assignment defeating the purpose of not evaluating the list
            val tail = tl
            Cons(() => head, () => tail)

        def passThroughCons[A](hd: => A, tl: Stream[A]): Stream[A] =
            Cons(() => hd, () => tl)
            
        def empty[A]: Stream[A] = Empty

        def apply[A](as: A*): Stream[A] =
            if as.isEmpty then empty
            else cons(as.head, apply(as.tail*))

    enum Stream[+A]:
        case Empty
        case Cons(head: () => A, tail: () => Stream[A])

        def isEmpty: Boolean =
            this match
                case Empty => true
                case Cons(head, tail) => false

        /**
          * headOption only evaluates the head of the LazyList and the rest of the LazyList will remain unevaluated (meaning the tail of the LazyList
          * will not be evaluated to a value) 
          * 
          *
          */
/**
  * This foldRight only evaluates the head and leaves the rest of the entries in the Stream unevaluated until needed when invoked
  * because the second argument in the combine function f is by-name
  */
        def foldRight[B](z: => B)(f: (A, => B) => B): B =
            this match
                case Cons(head, tail) => f(head(), tail().foldRight(z)(f))
                case Empty => z

/**
  * this function will give you a stream instead of a thunk but it doesn't actually evaluate any of the elements in the stream
  * for ex. () => Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, () => Empty)) becomes Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, () => Empty))
  */
        def tailS: Stream[A] =
            this match
                case Empty => Empty
                case Cons(head, tail) => tail()
            
        def headOption: Option[A] = this match
            case Empty => None
            case Cons(head, tail) => Some(head())
/**
 * converts to my own definition of list
  */
        def makeList: List[A] =
            def go(left: Stream[A], result: List[A]): List[A] =
                left match
                    case Empty => result
                    case Cons(head, tail) => go(tail(), List.Cons(head(), result))
            List.reverse(go(this, List.Nil))

/**
  * converts to a scala list
  */
        def toList: sList[A] =
            def go(left: Stream[A], result: sList[A]): sList[A] =
                left match
                    case Empty => result
                    case Cons(head, tail) => go(tail(), head() :: result)
            val result = go(this, Nil)
            result.reverse

        /**
          * this implementation isn't stack safe because I need to remember the previous context in order to create my list from right to left using the stream
          * also since List.Cons is strictly evaluated the right argument which is a recursive call will need to fully resolve
          * before List.Cons evaluates and returns. This implementation will fully evaluate to a result
          *
          * @return
          */
        def makeListV2: List[A] =
            this match
                case Empty => List.Nil
                case Cons(head, tail) => List.Cons(head(), tail().makeListV2)
        /**
         * takeV3 doesn't actually evaluate any of the entries contained in the list.. It explicitly creates the thunks for the tails of the list
         * and I believe each time an entry is needed in the list it will have to be re-evaluated to a value.
          */
        def takeV3(n: Int): Stream[A] =
            def go(left: Stream[A], i: Int, res: Stream[A]): Stream[A] =
                left match
                    case Empty => res
                    case Cons(head, tail) if i > 1 => go(tail(), i - 1, Cons(head, () => res))
                    case Cons(head, tail) if i == 1 => Cons(head, () => res)
                    case _ => res
            val backwardsStream = go(this, n, Stream.empty)
            go(backwardsStream, n, Stream.empty)
            
/**
  * takeV4 similar to take v3 doesn't actually evaluate any of the entries in the list.it uses the "smart" data constructor to build the result
  * so that we have caching after an entry in the list is evaluated.an since both parameters of the "smart" data constructor are by name even though
  * h() is passed in, it isn't actually evaluated.
  */

        def takeV4(n: Int): Stream[A] =
            @tailrec
            def go(left: Stream[A], i: Int, res: Stream[A]): Stream[A] =
                left match
                    case Cons(head, tail) if i > 1 => go(tail(), i - 1, Stream.cons(head(), res))
                    case Cons(head, tail) if i == 1 => Stream.cons(head(), res)
                    case _ => res
            val reversed = go(this, n, Stream.empty)
            go(reversed, n, Stream.empty)
/**
  * This function does not actually evaluate anything it is purely a description of what I want to happen.
  * This is accomplished by using by-name parameters in smart constructor so that I don't
  * evaluate anything in the list at no point during the execution of this function will any element
  * in the stream be resolved to a value? in fact of think it just will return Cons(() => hd, () => tl)
  * where lazy val hd = head() //unevaluated, and lazy val tl = tail().takeV5(n - 1) //unevaluated
  * Even though it isn't tail recursive, it won't be a problem because it doesn't resolve any of the list and when you need
  * to resolve the list, presumably what you resolve should be able to fit into memory
  * The difference between TakeV5 and the above versions of Take is that the above versions will actually build a new stream without evaluating the
  * entries while takeV5 doesn't do anything but return an object. No computation will be executed until something like makeList is called on the object
  * returned. I believe this is better reflecting what a Stream should be doing
  */
            
        def takeV5(n: Int): Stream[A] =
            this match
                case Cons(head, tail) if n > 1 => Stream.cons(head(), tail().takeV5(n - 1))
                case Cons(head, _) if n == 1 => Stream.cons(head(), Stream.empty)
                case _ => Stream.empty
            

        def drop(n: Int): Stream[A] =
            if this.isEmpty then Empty
            else if n <= 0 then this
            else this.tailS.drop(n - 1)
    /**
      * will actually evaluate the thunk that leads to tail of Stream n times. This actually will 
      * return a stream that is n length shorter than the original (I believe), but id doesn't resolve any
      * of the elements in the stream to a value
      */

        def dropV2(n: Int): Stream[A] =
            this match
                case Cons(_, t) if n > 0 => t().dropV2(n - 1)
                case Cons(_, t) if n == 0 => this
                case _ => Stream.empty
            

        def takeWhile(p: A => Boolean): Stream[A] =
            def go(left: Stream[A], result: Stream[A]): Stream[A] =
                left match
                    case Cons(head, tail) if p(head()) => go(tail(), Stream.cons(head(), result))
                    case _ => result
            val backwardsStream = go(this, Stream.empty)
            go(backwardsStream, Stream.empty)

        /**
          * this function actual resolves the head of the stream once to use p and then if p is true
          * it returns Cons(() => hd, () => tl) where hd, and tl and defined simmilar to takeV5 so its basically
          * a purely descriptive and involves minimal actual computation
          *
          * @param p
          * @return
          */
        def takeWhileV2(p: A => Boolean): Stream[A] =
            this match
                case Cons(head, tail) if p(head()) => Stream.cons(head(), tail().takeWhileV2(p))
                case _ => Stream.empty

        def takeWhileV3(p: A => Boolean): Stream[A] = this.foldRight(Stream.empty)((a, b) => if p(a) then Stream.cons(a, b) else Stream.empty)

        /**
          * map will return Stream.Cons(() => hd, () => tl) where lazy val hd = f(head()) //unevaluated
          * and lazy val tl = tail().foldright(z)(f) //unevaluated this is possible because our combine function is the smart constructor
          * which uses by-name parameters that are store in lazy vals and wrapped inside of thunks which are passed as arguments to Stream data constructor
          */
        
        def map[B](f: A => B): Stream[B] = this.foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

        /**
          * let h be foldRight function for map, g be foldRight function for filter z1 be initial map value
          * and z2 be initial filter value testList.map(f).filter(p) will return (if p evaluates to true
          *  in the beginning in my implementation)
          * Stream.Cons(() => f(head()), () => tail.foldRight(z1)(h).foldRight(z2)(g)) make list will iteratively
          * evaluate the head of this Stream (computation?) with something like if p(f(head())) then ... else ...)
          * these actions of map and filter are interleaved and no intermediate Streams are generated
          */


/**
  * the issue with writing to a covariant read safe stream (I can substitute a more specific stream for program
  * that specifies a more generic type stream and reading from it isn't an issues since I can discard type specific information)
  * is that its not type safe to put more generic objects in a more specific typed stream. I believe the specifying
  * a type is a supertype of the covariant type and then using that type as the input and return type of your program
  * lifts the type specification for your program above the original covariant type and then makes it invariant in the
  * type so that its read and write safe
  * (still not sure)  
  *
  * @param as
  * @return
  */

    /**
      * since z: B is unevaluated and f is by-name in its second parameter append will actually return
        Cons(() => hd, () => tl) where lazy val hd = head() //unevaluated 
        and lazy val tl = tail().foldright(z)(f) //unevaluated f = Stream.cons. append is purely
        descriptive and doesn't evaluate anything
      *
      * @param as
      * @return
      */
        def append[B >: A](as: => Stream[B]): Stream[B] = this.foldRight(as)((a, b) => Stream.cons(a, b))

        /**
          * filter either return Empty: Stream[A] or it will resolve one element
          * to a value and apply it to p. if p is true Cons(() => hd, () => tl) is returned where
          * lazy val hd = head() and lazy val tl = tail.foldRight(z)(f) where f is same as below
          * otherwise if p is false tail().foldRight(z)(g) returns so basically the function will run until
          * the end if p always evaluates to false but the first time its true then it will stop and send description of next
          * computation back (I believe)
          */

        def filter(p: A => Boolean): Stream[A] = this.foldRight(Stream.empty)((a, b) =>
            if p(a) then Stream.cons(a, b) else b)

/**
  * this will run until it finds the first entry that satisfies the predicate and then stop. since foldRight as defined passes its recursive step as the
  * second argument to the combine function f and the argument as defined by name, the recursive step doesn't executed unless explicitly called which it is only
  * done if the else branch is executed
  */
        def find(p: A => Boolean): Option[A] = this.foldRight[Option[A]](None)((a, b) =>
            if p(a) then Some(a) else b)


/**
  * I believe f will be invoked once two generate the first stream and then a Cons object will be returned with the first argument the unevaluated
  * head of the first Stream generated and the second argument containing the recursive step of the foldRight. So very little work done by this function
  */
        
        def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(Stream.empty[B])((a, b) =>
            f(a).append(b))
/**
  * because the second parameter for f is non strict any argument passed into it will not
  * have to be evaluated before it can be used in the body of f. In this case the second argument
  * is the recursive call to foldRight. This means that I have control over when recursive calls get executed.
  * the by name parameter along with short circuiting logic within function f will allow me to terminate recursion
  * on a condition that I choose. control structures that have short circuiting logic include &&, ||, if then else
    etc. these short circuiting control structures resolve to a value depending on the "branch" taken
    in effect this allows for a step by step incremental processing of this stream as long as the "branch" involving
    the recursive call is taken and when the "branch" involving the termination condition is taken we will have
    early termination. for example suppose p: A => Boolean and f: (A, B) = B = (a, b) => p(a) || b in foldRight
    for a Stream(1element, 2element, 3element) a trace of the execution could look like:
    f(1element, f(2element, f(3element, z))) = p(1element) || p(2element) || p(3element) || z since || short circuits
    if any one of these is true and each terms to the right corresponds to the second argument in f (they are generated
    recursively) which is by name then we have the potential for early termination without evaluating the rest of the terms
  *
  * @param z
  * @param f
  * @return
  */
    
        def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

        def headOptionV1: Option[A] = this.foldRight(None: Option[A])((a, b) => Some(a))

        import ChapterFive.infiniteStreams.unfold
        def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
            unfold((this, that)) {
                case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()) -> Some(h2())), (t1() -> t2())))
                case (Empty, Cons(h2, t2)) => Some(((None -> Some(h2())), (Stream.empty -> t2())))
                case (Cons(h1, t1), Empty) => Some(((Some(h1()) -> None), (t1() -> Stream.empty)))
                case (Empty, Empty) => None

            }

        def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
            unfold(this -> bs) {
                case Cons(h1, t1) -> Cons(h2, t2) => Some(f(h1(), h2())  ->  (t1(), t2()))
                case _ => None
            }
        def zip[B](bs: Stream[B]): Stream[(A, B)] =
            this.zipWith(bs) {
                case (a, b) => (a, b)
            }
        
        def startsWith[A](that: Stream[A]): Boolean = this.zipAll(that).foldRight(true) {
            case ((None, h2), _) => false
            case ((h1, None), _) => true
            case ((h1, h2), b) => h1 == h2 && b
        }

        def tails: Stream[Stream[A]] = unfold(this)(s => 
            if s.isEmpty then None
            else Some((s, s.tailS)))

        def exists(p: A => Boolean): Boolean = this.foldRight(false)((a, b) => p(a) || b)

        def hasSubsequence[A](prefix: Stream[A]): Boolean = tails exists(_.startsWith(prefix))

        /*first attempt
        def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
            tails.foldRight(Stream.cons(z, Stream.empty[B]))((s, b) => Stream.cons(s.foldRight(z)(f), b))*/
            /**
              * Cons(1, Cons(2, Cons(3, Empty)))
              * Cons(f(1, f(2, f(3, z))), Cons(f(2, f(3, z)), Cons(f(3, z), Cons(z, Empty))))
              *
              * 
              * foldRight replaces Empty with z and Cons with g so for Cons(e1, Cons(e2, Cons(e3, Empty))) you will get g(e1, g(e2, g(e3, z')))
              * in this case z' = (z, Empty) at each step g needs to build the list in the second tuple coordinate and needs to accumulate the result of
              * f applied to the entry in the stream (e1, e2, e2) and the first coordinate in the tuple z
              * 
              * for function g foldRight will destructure a stream apply g to the head for the first argument, and for the second argument will apply the
              * recursive call  tail (as in tail.foldRight(z)(g))
              * 
              * g: (A, (B, Stream[B])) => (B, Stream[B]) = (a, (b, cons(h, t)))
                val (nextVal, (accumulated, result)) = {
                    val accumulated' = f(nextVal, accumulated)
                    val result' = Stream.cons(accumulated, result)
                    (accumulated', result')
                }
              * 
              * so for g(e3, z') we have
              * val accumulated' = f(e3, z)
              * val result' = Stream.cons(z, Empty)
              * (accumulated', result')
              * 
              * @param z
              * @param f
              * @return
              */
        def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = this.foldRight((z, Stream.cons(z, Empty))) {
            case (nextVal, (accumulated, result)) => {
                lazy val newAccumulated = f(nextVal, accumulated)
                val newResult = Stream.cons(newAccumulated, result)
                (newAccumulated, newResult)
            }
        }(1)
                


    /**
     * When I create a list with apply method it will take in a vararg arguments of type A which will be converted to a type Seq[A]
     * the head and tail of this sequence will be passed by name (so the arguments will not be evaluated until its called or used) to the cons
     * smart constructor the cons smart constructor takes these unevaluated arguments and stores them as lazy vals (which will have type lazy)
     * these will be sent as thunks to the Cons data constructor (I'm not sure what lazy does for us here since the thunks delay evaluation until they
     * are explicitly called)
     * Stream(1, 2, 3, 4, 5) will return Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, () => Cons(() => 4, () => Cons(() => 5, () => Empty)))))
     */

        



            

    /**
     * lazy vals have type lazy? and will not get evaluated until the reference holing the lazy value is used
     * from that point forward the lazy value reference will hold the result of the r.h.s expression and future calls to
     * that the lazy val will be cache hits (returning the result of the expression from cache) as opposed to vals which have the type
     * of the result of the r.h.s expression (type inference?). vals eagerly evaluate their r.h.s expression and store the result.
     */

object TestStreamScanRight:
    import ChapterFiveExercises._
    @main def printTestStreamScanRight: Unit =
        val l1 = Stream(1,2,3)
        val l2 = Stream()
        val result = l1.scanRight(0)(_ + _).makeList
        //val result1: Int = Stream[Int]().foldRight(0)((a, b) => a + b)
        println(result)
        //println(result1)
        //print(l2)

object TestHasSubsequence:
    import ChapterFiveExercises._
    @main def printTestHasSubsequence: Unit =
        val l1 = Stream(1,2,3,4,5)
        val l2 = Stream(3,4,5,6)
        val result = l1.hasSubsequence(l2)
        println(result)

object TestStartsWith:
    import ChapterFiveExercises._
    @main def printTestStartsWith: Unit =
        val l1 = Stream(1,2,3,4,5)
        val l2 = Stream(2,3)
        val result = l1.startsWith(l2)
        println(result)

object TestFilter:
    @main def printTestFilter: Unit =
        val testStream = Stream(1,2,3,4,5,2,2,1)
        val p1: Int => Boolean = _ >= 3
        val p2: Int => Boolean = _ > 5
        val p3: Int => Boolean = _ >= 1

        val result1 = testStream.filter(p1)
        val result2 = testStream.filter(p2)
        val result3 = testStream.filter(p3)

        println("actual: %s".format(result1))
        println("actual: %s".format(result2))
        println("actual: %s".format(result3))

object TestHeadOption:
    import ChapterFiveExercises._
    @main def printTestHeadOption: Unit =
        val testStream = Stream(1,2,3,4,5)
        val testStream1 = Stream.empty
        val result1 = testStream.headOptionV1 //expected: Some(1)
        val result2 = testStream1.headOptionV1 //expected None

        println("expected: Some(1)")
        println("actual: %s".format(result1))

        println("expected: None")
        println("actual: %s".format(result2))

object TestSFlatMap:
    import ChapterFiveExercises._
    @main def printTestSFlatMap: Unit =
        val f: Int => Stream[Int] = i => Stream(i, i)
        val testStream = Stream(1,2,3,4,5)
        val result = testStream.flatMap(f)
        println(result)
        println(result.makeList)

object TestForAll:
    import ChapterFiveExercises._
    @main def printTestForAll: Unit =
        val testStream = Stream(1,2,3,4,5)
        val p1: Int => Boolean = _ <= 5
        val p2: Int => Boolean = _ <= 3
        val result1 = testStream.forAll(p1) //expected: true
        val result2 = testStream.forAll(p2) //expected false

        println("expected: true")
        println("actual: %s".format(result1))

        println("expected: false")
        println("actual: %s".format(result2))

object TestStreamTake:
    import ChapterFiveExercises.Stream.{Cons, Empty}
    import ChapterFiveExercises._
    @main def printTestStreamTake: Unit =
        val testStream = Stream(1,2,3,4,5)
    
        //val testStream1 = Stream(1,2,3,{println("takeV3 doesn't cache once the head is evaluated?"); 4 },5)
        //val testStream2 = ChapterFiveExercises.Stream.Cons(() => 1, () => Cons(() => 2, () => Cons(() => {println("takeV3 doesn't cache once the head is evaluated?"); 3 }, () => Cons(() => 4, () => Cons(() => 5, () => ChapterFiveExercises.Stream.Empty) ))))
        val testStream3 = Stream.passThroughCons({println("expensive operation for reference pointing to 0"); 0}, testStream)
        lazy val n1 = {println("This expression should be evaluated once since reference pointing to -1 is lazy"); -1}
        lazy val n2 = -2
        lazy val n3 = -3
        def buildStream[A](xs: ChapterFiveExercises.Stream[A], as: => A*): ChapterFiveExercises.Stream[A] =
            if as.isEmpty then xs
            else buildStream(ChapterFiveExercises.Stream.Cons(() => as.head, () => xs), as.tail*)

        //val result7 = testStream3.takeV3(2)
        //val result8 = testStream3.takeV3(3)

        val testStream4 = buildStream(testStream3, n1, n2, n3)

        val result9 = testStream4.takeV4(2)
        val result10 = testStream4.takeV4(3)
        val result11 = testStream4.takeV4(4)
        val result12 = testStream4.takeV4(5)
        //val lList2 = empty
        //val lList1 = Stream(1,2)
        //val result = testStream.take(3) //expected: Cons(1, Cons(2, Cons(3, Nil)))
        //val result1 = lList1.take(3) //expected: Cons(1, Cons(2, Nil))
        //val result2 = lList2.take(3) //expected: Nil
        //val result3 = testStream1.takeV3(3) //expected(Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, () => Nil))))
        //val result4 = testStream1.takeV3(4)

        //val result5 = testStream2.takeV3(3) //expected(Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, () => Nil))))
        //val result6 = testStream2.takeV3(4)
    

        //println("expected: Cons(1, Cons(2, Cons(3, Nil)))")
        //println("actual %s".format(result))

        //println("expected: Cons(1, Cons(2, Nil))")
        //println("actual %s".format(result1))

        //println("expected: Nil")
        //println("actual %s".format(result2))

        //println("expected(Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, () => Nil))))")
        //println("actual %s".format(result5))
        //println("result 3 as a list with side effect: %s".format(result5.makeList))
        //println()
        //println("---------------Calling take 4 on the same list with side effect---------------")
        //println("expected(Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, () => Cons(() => 4, () => Nil)))))")
        //println("actual: %s".format(result6))
        //println("result 4 as a list with side effect: %s".format(result6.makeList))

        //println(result7)
        //println(result7.makeList)
        //println()
        //println(result8)
        //println(result8.makeList)

        println(s"the original stream as a list is: ${testStream4.makeList}")

        println(result9)
        println(result9.makeList)
        println()
        println(result10)
        println(result10.makeList)
        println()
        println(result11)
        println(result11.makeList)
        println()
        println(result12)
        println(result12.makeList)

object testZipAll:
    import ChapterFiveExercises._
    @main def printTestZipAll: Unit =
        val s1 = Stream(1,2,3)
        val s2 = Stream(4,5,6,7,8,9)
        val result = s1.zipAll(s2).makeList
        println(result)

object TestTakeWhile:
    import ChapterFiveExercises.Stream.{Cons, Empty}
    import ChapterFiveExercises._
    @main def printTestTakeWhile: Unit =
        val testStream = Stream(1,2,3,4,5)
        val result = testStream.takeWhileV3(x => x <= 3) //expected: Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, () => Empty)))
        println("expected: Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, () => Empty)))")
        println("actual: %s".format(result))
        println("result as a list: %s".format(result.makeList))

object TestMakeList:
    import ChapterFiveExercises.Stream.{Cons, Empty}
    import ChapterFiveExercises._
    @main def printtestMakeList: Unit =
        val lList = Stream(1,2,3,4,5)
        val resList = lList.makeList //expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))

        println("expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))")
        println("actual: %s".format(resList))

object TestLazy:
    import ChapterFiveExercises.Stream.{Cons, Empty}
    import ChapterFiveExercises._
    @main def printTestLazy: Unit =
        val xs = Stream(2,3,4)
        //val newXs: ChapterFive.ChapterFiveExercises.Stream[Int] = Cons(() =>{println("This is an expensive computation"); 1}, () => xs)
        //val smartNewXs: ChapterFive.ChapterFiveExercises.Stream[Int] = cons({println("This is an expensive computation"); 1}, xs)
        //val res = newXs.headOption
        //val res1 = newXs.headOption
        //val res2 = smartNewXs.headOption
        //val res3 = smartNewXs.headOption
        //println(res)
        //println(res1)
        /**
          * after passing the arguments in by name into valCons (which stores them as vals instead of lazy val) if I call headOption
          * multiple times the head will be recomputed (thunk will be called) each time.
          */
        //val valConsNewXs: ChapterFive.ChapterFiveExercises.Stream[Int] = valCons({println("This is an expensive computation"); 1}, xs)
        val directCons: ChapterFive.ChapterFiveExercises.Stream[Int] = ChapterFiveExercises.Stream.Cons(() => {println("This is an expensive computation"); 1}, () => xs)
        //val res5 = valConsNewXs.headOption
        //val res6 = valConsNewXs.headOption

        val res7 = directCons.headOption
        val res8 = directCons.headOption
        //println(res2)
        //println(res3)

        //println(res5)
        //println(res6)

        //println(res7)
        //println(res8)


/*def expensiveOp: Unit =
    Thread.sleep(2000)

case class TestData(data: () => String)

lazy val lazyData = {println("I will evaluate to: "); "Iam lazy data"} //this code is not run yet
val regData = {println("I will evaluate to: "); "Iam reg data"} //this is ran as soon as the assignment is executed executed 
// if I pass regData to a thunk afterward it will have already evaluated to a result

val data1 = TestData(() => lazyData)
val data2 = TestData(() => regData)*/

object TestTailOfRegularList:
    import ChapterThreeExercises._
    import ChapterThree._
    @main def printTestTailOfRegularList: Unit =
        val testList = ChapterThreeExercises.List(1,2,3,4,5)
        //val result = testList.exists(x => x > 2)
        val p: Int => Boolean = _ > 2
        val result1 = ChapterThreeExercises.List.foldRight(false)(testList)((a, b) => {println("The head is being evaluated. The current head is: %s".format(a)); p(a)} || {println("The tail is being evaluated. The previously evaluated head is: %s".format(a)); b})


object infiniteStreams:
    import ChapterFiveExercises._
    def ones: Stream[Int] = Stream.cons(1, ones)

    def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

    def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

    def fibs: Stream[Int] =
        def go(previous: Int, current: Int): Stream[Int] =
            Stream.cons(previous, go(current, previous + current))
        go(0, 1)

    /*def unfold[A, S](z: S)(f: S => (A, S)): Stream[A] =
        def go(state: S): Stream[A] =
            val (value, nextState) = f(state)
            Stream.cons(value, go(nextState))
        go(z)*/

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
        f(z) match
            case Some((value, nextState)) => Stream.cons(value, unfold(nextState)(f)) 
            case None => Stream.empty

    def fibsViaUnfold: Stream[Int] = unfold[Int, (Int, Int)](0 -> 1) {
        case (current, next) => Option.Some((current, (next -> (current + next))))
    }

    def fromViaUnfold(n: Int): Stream[Int] = unfold(n) {
        case current => Option.Some((current, current + 1))
    }

    /**
      * need to take notes here
      *
      * @param a
      * @return
      */
    def constantViaUnfold[A](a: A): Stream[A] = unfold(a) {
        case value => Option.Some((value, value))
    }

    def onesViaUnfold: Stream[Int] = constantViaUnfold(1)

    def mapViaUnfold[A, B](as: Stream[A])(f: A => B): Stream[B] = unfold(as) {
            case Stream.Empty => None
            case Stream.Cons(head, tail) => Some((f(head()), tail()))   
    }
    
    def takeViaUnfold[A](as: Stream[A], n: Int): Stream[A] = unfold(as) {
            case Stream.Cons(head, tail) if n > 0 => Some((head(), takeViaUnfold(tail(), n - 1)))
            case _ => None
        
    }

    def takeViaUnfold1[A](as: Stream[A], n: Int): Stream[A] = unfold((as, n)) {
            case (Stream.Cons(head, tail), r) if r > 1 => Some((head(), (tail(), r - 1)))
            case (Stream.Cons(head, _), r) if r == 1 => Some((head(), (Stream.empty, 0)))
            case _ => None
        
    }

    def takeWhileViaUnfold[A](as: Stream[A])(p: A => Boolean): Stream[A] =
        unfold(as) {
                
                case Stream.Cons(head, tail) if p(head()) => Some((head(), tail()))
                case _ => None
            
        }
        

    @main def printTestInfiniteStreams: Unit =
        val myOnes = ones.takeV5(3).makeList
        val myConstant = constant(7).takeV5(4).makeList
        val myConstant1 = constantViaUnfold(7).takeV5(4).makeList
        val myFrom = from(9).takeV5(10).makeList
        val myFrom1 = fromViaUnfold(9).takeV5(10).makeList
        val myFibs = fibs.takeV5(10).makeList
        val myFibs1 = fibsViaUnfold.takeV5(10).makeList
        val myMap = mapViaUnfold(Stream(1,2,3,4,5))(n => s"number $n")
        val myMap1 = myMap.makeList
        val myTake = takeViaUnfold(Stream(1,2,3,4,5), 3)
        val myTake1 = takeViaUnfold(Stream(1,2,3,4,5), 3).makeList
        val myTake2 = takeViaUnfold1(Stream(1,2,3,4,5), 3)
        val myTake3 = takeViaUnfold1(Stream(1,2,3,4,5), 3).makeList
        val myTails = Stream(1,2,3,4,5).tails.map(_.makeList).makeList
        

        println(myOnes)
        println(myConstant)
        println(myConstant1)
        println(myFrom)
        println(myFrom1)
        println(myFibs)
        println(myFibs1)
        println(myMap)
        println(myMap1)
        println(myTake)
        println(myTake1)
        println(myTake2)
        println(myTake3)
        println(myTails)










