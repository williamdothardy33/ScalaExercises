package ChapterSeven

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Callable
import ChapterSeven.fpinscala.answers.parallelism.Actor
import java.util.concurrent.Executors
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.util.Random

/**
  * This future type takes as an input a side-effecting function (aka callback/ continuation) and doesn't return anything
  * I think with this mechanism we can specify what to do with a returned value from a parallel computation.
  * The callback also makes the computation asynchronous (I think) because we are not block waiting on a returned value
  * but instead we are specifying, using a continuation, what to do after a computation is finished processing on a worker thread.
  * I think the idea for this callback/continuation is that within this continuation I will have some logic that will mutate
  * some shared resource (reference/field or something) that is accessible across threads and so we have to ensure
  * that it's synchronized/atomic in its reads and writes to that resource
  */

opaque type Future[+A] = (onFinish: A => Unit) => Unit
opaque type Par[+A] = ExecutorService => Future[A]

object Par:
/**This Par that unit returns doesn't actually do anything with the ExecutorService dependency it just takes the value a provided to unit
 * and passes it to the callback passed in ("registered") to the Future that it returns
  * 
  */    
    def unit[A](a: A): Par[A] =
        (s: ExecutorService) =>
            (onFinish: A => Unit) => onFinish(a)
    
    def evalToOnFinish(s: ExecutorService)(block: => Unit): Unit =
        s.submit(new Callable[Unit] {
            def call(): Unit = block
        })

/**
  * if a is unit(x) this will fork off the main thread (with the Callable call method set to invoke the continuation with the value x that 
  * was passed into unit). the future returned from the Par that fork returns does not submit to the thread pool or whatever concurrent/parallel evaluating
  * strategy you use until you pass it a continuation. This continuation will be passed onto worker threads where it will be invoked
  * when those threads finish processing the computation (Pars will evaluate to a Future and instead of calling get and blocking (until we can
  * return a value from the thread) we pass in a continuation
  * that will be invoked asynchronously when processing is done.) fork just takes an unevaluated Par and "pushes" the
  * evaluation of that par another thread (just submits it to an Executor Service)
  */

    def fork[A](a: => Par[A]): Par[A] =
        (s: ExecutorService) =>
            (onFinish: A => Unit) =>
                evalToOnFinish(s)(a(s)(onFinish))

    

    
 
    def lazyUnit[A](a: => A): Par[A] =
        fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
        (a: A) =>
            lazyUnit(f(a))

    def splitSequence[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
        val length = pas.length
        if length == 0 then unit(IndexedSeq.empty[A])
        else if length == 1 then pas.head.map(res => IndexedSeq(res))
        else
            val (l, r) = pas.splitAt(length / 2)
            splitSequence(l).map2(splitSequence(r)) {
                (lSeq, rSeq) => lSeq ++ rSeq
            }

    end splitSequence

    def sequence[A](pas: List[Par[A]]): Par[List[A]] = splitSequence(pas.toIndexedSeq).map(res => res.toList)
        
    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {
        val pas = as.map(asyncF(f))
        sequence(pas)
    }

    def parFilter[A](as: List[A])(p: A => Boolean): Par[List[A]] =
        val f = (a: A) =>
            if p(a) then List(a)
            else List.empty[A]
        parMap(as)(f).map(res => res.flatten)

    extension [A](p: Par[A])
        def run(s: ExecutorService): A =
            val result = new AtomicReference[A]()
            val latch = CountDownLatch(1)
            p(s) {
                a => result.set(a)
                latch.countDown()
            }
            latch.await()
            result.get()
        end run

        def map2[B, C](bPar: Par[B])(f: (A, B) => C): Par[C] =
            (s: ExecutorService) =>
                (combinedOnFinish: C => Unit) =>
                    var aResult: Option[A] = None
                    var bResult: Option[B] = None
                    
                    val combineActor: Actor[Either[A, B]] = Actor(s) {
                        case Left(aValue) =>
                            bResult match
                                case None => aResult = Some(aValue)
                                case Some(bValue) => evalToOnFinish(s)(combinedOnFinish(f(aValue, bValue)))
                
                        case Right(bValue) => 
                            aResult match
                                case None => bResult = Some(bValue)
                                case Some(aValue) => evalToOnFinish(s)(combinedOnFinish(f(aValue, bValue)))
                    }

                    p(s) {
                        aRes => combineActor ! Left(aRes)
                    }

                    bPar(s) {
                        bRes => combineActor ! Right(bRes)
                    }
        end map2

        def map[B](f: A => B): Par[B] =
            p.map2(unit(())) {
                (aRes, _) => f(aRes)
            }
        end map
                
        def flatMapWithActor[B](f: A => Par[B]): Par[B] =
            (s: ExecutorService) =>
                (onFinish: B => Unit) =>
                    var handleActor = Actor[A](s) {
                        result =>
                            evalToOnFinish(s)(f(result)(s)(onFinish))
                    }
                    p(s)(a => handleActor ! a)
        
        def flatMap[B](f: A => Par[B]): Par[B] =
            fork {
                (s: ExecutorService) =>
                    (continuation: B => Unit) =>
                        p(s) {
                            a =>
                                f(a)(s)(continuation)
                        }
            }

object TestParMapNonBlocking:
    import ChapterSeven.Par._
    @main def printTestParMapNonBlocking: Unit =
        val testList = List.range(1, 100_000)
        val sqrts = parMap(testList)(Math.sqrt(_))
        val s = Executors.newFixedThreadPool(1)
        val res = sqrts.run(s)
        println(res)

object TestException:
    import ChapterSeven.Par._
    @main def printTestException: Unit =
        val exceptionPar = lazyUnit(Try(1 / 0))
        val s = Executors.newFixedThreadPool(1)
        val res = exceptionPar.run(s)
        res match
            case Failure(exception) => println(exception) 
            case Success(value) => println(value)

object TestFlatMap:
    import ChapterSeven.Par._
    @main def printTestFlatMap: Unit =
        val randPar = lazyUnit(Random.between(1,3))
        val addTwoPar = asyncF[Int, Int](x => x + 2)
        val s = Executors.newFixedThreadPool(1)
        val resPar = randPar.flatMapWithActor(addTwoPar)
        val result = resPar.run(s)
        println(result)
