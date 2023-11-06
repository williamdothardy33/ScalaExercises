package ChapterSevenBlocking
import scala.concurrent.ExecutionContext
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import java.util.concurrent.Callable
import java.util.concurrent.Executors
import ChapterSevenBlocking.ChapterSevenExercises.Par.parReduce
import ChapterSevenBlocking.ChapterSevenExercises.Par.run
import ChapterSevenBlocking.ChapterSevenExercises.Par.parMap
import ChapterSevenBlocking.ChapterSevenExercises.Par.countWords
import ChapterSevenBlocking.ChapterSevenExercises.Par.lazyUnit
import ChapterSevenBlocking.ChapterSevenExercises.Par.equal
import ChapterSevenBlocking.ChapterSevenExercises.Par.fork
/**
  * Note with Par you cannot think of the Par type as something like a container of a type that you can wrap/unwrap to transform a containing value
  * that is not what is happening at all!!!!!
  */
/**
  * There is an important distinction between parallelism and concurrency. Parallelism is the simultaneous execution of different parts of the program
  * (using cores which can execute instructions simultaneously because they are like subprocessors embedded within the cpu)
  * while concurrency is the ability for different parts/units/sub-program of your application to run out of order without affecting the result
  * So a concurrent program can be parallel but an important difference is that for concurrent programs the parts that are running out of order and 
  * simultaneously must compose so as to not affect the final outcome. (So I believe there is a constraint on the algorithm used to solve the problem?)
  * If your processor only has 4 cores then a parallel program can only run 4 computations at a time? but a concurrent program may run any amount of computations
  * concurrently by interleaving computations on cores (and within cores based on scheduling/ possibly there is some optimal scheduling strategy
  * although I don't know if that much control is possible?) that are freed up from executing computations. (I picture a situation like having four cores running
  * computations, and one of the cores finishes it's task before the other three and the execution strategy being able to interleave another computation or
  * possibly part of a running computation on this freed up resource) (I also picture within the same core some scheduler kicking a subtask
  * of the "executor" and pulling some other subtask in to get execution time in) Another difference is that even in one core you can have 
  * concurrency because you can interleave the execution of multiple subprocesses in application (In this situation I believe it's like you can 
  * "pause" the execution on one long running task start the execution of another task etc and interleave the execution of multiple long running tasks)
  * A Thread is a unit of execution (cpu utilization?). they are provisioned their own stack cpu registers (this maybe for program counter, stack counter, and cache I think) and a program counter and thread ID.
  */
/**
  * A library is a wrapper around primitive language features that make some problem domain more ergonomic to work with/ easier to use
  * Some design choices considered include what get parallelized (fork) when does it get parallelized (executing function body vs returning a function)
  * evaluation strategy (by-name vs by-value) that facilitates parallelization
  * 
  * 
  * 
  */

object ChapterSevenExercises:

/**
 * In general we have some idea of what what our library should be used for.
 * For the example in the book we want our library to be able to: Create parallel computations
  * Step 1: identify a use case that your library will help facilitate
  */

/**
  * an example of a computation that is parallelizable is summing of integers (I believe that this is parallelizable because
  * 1) Integers have Identity so if I break a list of integers into parts and sum each part in different threads I can have a starting point from which 
  * to accumulate the sum on that thread
  * 2) addition operator is commutative so when I'm merging the sums the order of the merging will not change the outcome
  * 3) addition operator is left and right associative so I've some flexibility in how I go about accumulating the sums in each thread not to mention
  * I think associativity is required to repeatedly apply the operator on a list of more than 2 elements)
  */

  def sum(as: Seq[Int]): Int =
    as.foldLeft(0)(_ + _)

/**
  * The above sums sequentially. How could we split apart the sum so that each "piece" could be sent to a thread that can work on it
  * Below we use IndexSeq[Int] because this is the base type for data structures like Vector which has efficient random access to it elements
  * unlike list which is O(n). So if I need to split at some index that I compute I would use an IndexSeq
  */

  def sum(as: IndexedSeq[Int]): Int =
    val length = as.size
    if length <= 1 then as.headOption getOrElse 0
    else
      val (l, r) = as.splitAt(length / 2)
      sum(l) + sum(r)

/**
  * the returned value in the above is sum(l) + sum(r). these are computations (recursive) that each return Int when they are finished (they are good
  * candidates to be run in parallel) just like we used a container type to represent our Option / Either we can also use a container to 
  * represent these computations that we would like to run in parallel (here we are just creating a container for our parallel computation 
  * api. We are not actually saying anything about the execution strategy) unit is a way to "lift" our (unevaluated) computation into a parallel
  * computation? (I suspect unit is important for modularity and composability although I can't quite articulate why.)
  * Why would you want to compose to parallel computations? I think the merging of parallel computations like when I splitting a sum into parts and asking
  * a threads to calculate partial sums and then the "combining" those partial sums is a "composition" of the parallel computations
  */

  opaque type Par[A] = ExecutorService => Future[A]
  
  object Par:
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
      
    def get[A](a: Par[A]): A = ???

/**
  * I believe this version builds a stack on the main thread (recursive step is happening on main thread? and
  * map2 doesn't run until recursive calls have been fully exhausted) because function arguments in strict
  * functions need to be fully evaluated before entering function body map2 won't run until foldRight is
  * finished?) the first "combine" step (the last element with unit)
  * may fork off the main thread (previous element the unit element by as implemented doesn't fork) return and 
  * combine, and then the "previous" element will fork off return and combine with the "next" result. rinse and repeat. (Here our repackaging
  * is done on the main I think but the parallel processes that compose our original list may be forked off)
  * (essentially we may have forking only to get result out of the Par)
  */

    def naiveSequence[A](pas: List[Par[A]]): Par[List[A]] = pas.foldRight(unit[List[A]](Nil))((px, pxs) =>
      px.map2(pxs)((x, xs) => x::xs))

/**
  * This implementation ensures the recursive calls are forked off to another thread and
  * now repackaging happens across a span of threads (another way of say this is that the execution of forkedSequence is spread across a bunch of threads
  * instead of spread across a bunch of stack frames)
  */

    def forkedSequence[A](pas: List[Par[A]]): Par[List[A]] =
      pas match
        case head :: next => head.map2(fork(forkedSequence(next)))(
          (x, xs) => x::xs)
        case Nil => unit[List[A]](Nil)

/**
  * Since map2 is strict and both arguments to map2 are recursive calls to splitSequence the recursive calls will be
  * exhausted before we get into the body of map2. The code examples where I got this says that after splitting the list
  * they will be run in parallel. I'm not exactly sure how that is true. I looks like this builds a stack until splitSequence
  * is exhausted on both the left and the right and then map2 can begin its work (The way I picture this working is for the recursive calls
  * where the sequence is length 2 (or 1 is the length is odd) the next recursive call for both the left and right will return and map2 will combine them
  * this is where the make fork off onto another thread, but to me it looks like they only fork to get the result out of the Par) To me
  * the only difference between this and the naive solutions is that we can reduce the number of recursive calls to get to base case from n to log(n)
  * but are we leveraging any parallelism
  */
        
    def splitSequence[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
      val length = pas.length
      if length == 0 then unit(IndexedSeq.empty)
      else if length == 1 then map(pas.head)(a => IndexedSeq(a))
      else
        val (l, r) = pas.splitAt(length / 2)
        splitSequence(l).map2(splitSequence(r))((x1s, x2s) => x1s ++ x2s)

    def sequence[A](pas: List[Par[A]]): Par[List[A]] = map(splitSequence(pas.toIndexedSeq))(_.toList)
        
    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {
      val pas = as.map(asyncF(f))
      sequence(pas)
    }

    def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] = fork {
      val pas = as.map(asyncF(f))
      splitSequence(pas)
    }

    def parFilter[A](as: List[A])(p: A => Boolean): Par[List[A]] = fork {
      val f: A => List[A] = (a: A) =>
        a match
          case u if p(u) => List(u)
          case _: A => List.empty[A]
      val pas = as.map(asyncF(f))
      val pxs = map(sequence(pas))(_.flatten)
      pxs
    }

    def parReduce[A](as: IndexedSeq[A], f: (A, A) => A): Par[A] = fork {
      val length = as.length
      if length == 1 then unit(as.head)
      else if length == 2 then unit(f(as.head, as.tail.head))
      else
        val (l, r) = as.splitAt(length / 2)
        fork(parReduce(l, f)).map2(fork(parReduce(r, f)))((a1, a2) => f(a1, a2))
    }

    def map3[A, B, C, D, E](a: Par[A], b: Par[B], d: Par[D])(f: (A, B) => C, g: (C, D) => E) = a.map2(b)(f).map2(d)(g)

    def map4[A, B, C, D, E, F, G](a: Par[A], b: Par[B], d: Par[D], fpar: Par[F])(f: (A, B) => C, g: (C, D) => E, h: (E, F) => G) = map3(a, b, d)(f, g).map2(fpar)(h)

    def map5[A, B, C, D, E, F, G, H, I](a: Par[A], b: Par[B], d: Par[D], fpar: Par[F], hpar: Par[H])(f: (A, B) => C, g: (C, D) => E, h: (E, F) => G, l: (G, H) => I): Par[I] = map4(a, b, d, fpar)(f, g, h).map2(hpar)(l)
  
    def equal[A, B](s: ExecutorService)(a: Par[A], b: Par[B]): Boolean =
      a(s).get() == b(s).get()

    def parEqual[A, B](a: Par[A], b: Par[B]): Par[Boolean] =
      a.map2(b) {
        case (resA, resB) => resA == resB
      }

/**
  * although fork takes it's argument by-name the block passed to fork returns a type Par which is valid (I was confused by this) when the block is actually
  * evaluated it will return either unit(f(as.head, z)) or map2(fork(parFold(z)(l, f, g)), fork(parFold(z)(r, f, g)))((b1, b2) => g(b1, b2)) this
  * is done when fork sets to call method for the Callable it submits to the ExecutorService both unit/map2 produce an explicit Par implementation
  * I need to look at this further!!! because there an assumption being made about f???
  */

    def parFold[A, B](z: B)(as: IndexedSeq[A], f: (A, B) => B, g: (B, B) => B): Par[B] = fork {
      val length = as.length
      if length == 1 then unit(f(as.head, z))
      else
        val (l, r) = as.splitAt(length / 2)
        fork(parFold(z)(l, f, g)).map2(fork(parFold(z)(r, f, g)))((b1, b2) => g(b1, b2))
    }

    def countWords(paragraphs: List[String]): Par[Int] = parFold(0)(
      paragraphs.toIndexedSeq, 
      (paragraph, count) => count + paragraph.split(" ").length,
      (count1, count2) => count1 + count2)

    def timedMap2[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C] =
      (s: ExecutorService) => new Future[C]:

/**
  * below is where I execute my task (or at least submit for execution). I know this because Future is something I get back after
  * after I submit a task for execution (using java ExecutorService) If my par was made with unit then execution will be on the current thread. 
  * If my Par was made with fork then depending on it's implementation there is a possibility that it will be executed on a different
  *  thread/shared thread pool
  * In the book the terminology "primitives" is used. This just mean that the function is and indivisible "unit"
  * of functionality (meaning I cannot provide an implementation of the function in terms of other functions
  * already in the library)
  * 
  * The code for the book mentions Right nested: when you fork of the main thread (barring problems) the result will
  * be returned to the main thread after the worker thread gets done (that's what we have futures for). Imagine each recursive
  * call forking a new thread. Then recursive call leaves an I.O.U (Future) on the thread it forked off of. So for a recursive
  * call instead of building a stack to "drill" into the recursive calls to get to a base case we are "drilling"
  * through threads leaving an I.O.U at each "depth" so we don't have to worry about order??? (nice!!!) apparently because
  * of this "right-nesting" we can split this in half to get better performance (will have to think more on this)
  */

        val futureA = a(s)
        val futureB = b(s)
        @volatile var resultCached: Option[C] = None
        def isDone(): Boolean = resultCached.isDefined
        def get(): C = get(Long.MaxValue, TimeUnit.NANOSECONDS)

/**
  * Note: Whatever thread calls get will block (That thread will suspend execution until we get a result back) Our definition for get with a timeout
  * has nothing to do with the strategy for execution, The only thing it does is block the calling thread until there is a result
  * I mistakenly called get immediate after passing an ExecutorService to my Par which in fact did make it sequential but what I want to do is
  * pass an ExecutorService to each one of my Pars and get a Future back and then spread the timeout time window between the two futures,
  * and then execution and calling get will be decoupled. It does not matter which future gets the full/remaining time because both must finish
  * in the the alloted timeout time period (I'm assuming sort of interrupt will be sent if timeout time period expires but idk)
  * 
  */

        def get(timeout: Long, unit: TimeUnit): C =
          val timeoutNano = unit.convert(timeout, TimeUnit.NANOSECONDS)
          val start = System.nanoTime()
          val aResult = futureA.get(timeoutNano, TimeUnit.NANOSECONDS) //This call is blocking so this is why we can clock it
          val elapsed = System.nanoTime() - start
          val remaining = timeoutNano - elapsed
          val bResult = futureB.get(remaining, TimeUnit.NANOSECONDS) //I don't know if this means that these operations are synchronous
          val result = f(aResult, bResult)
          resultCached = Some(result)
          result
        def isCancelled(): Boolean = futureA.isCancelled() || futureB.isCancelled()

/**
  * The java api for concurrency handles the cases where the task has not run yet (removing it from the task queue) or if it has finished
  * (ignoring the call to cancel) but it your task is still running the only thing it does is set an internal flag on the thread running the task
  * Your code (task code) needs to check (by polling perhaps) with the method Thread.isInterupted() and respond accordingly. this api backs scala
  * concurrency so it's important!
  * 
  */
        def cancel(mayInterruptIfRunning: Boolean): Boolean = futureA.cancel(mayInterruptIfRunning) || futureB.cancel(mayInterruptIfRunning)


/**
  * ExecutorService is a sub-interface of Executor that specifies the method called submit. It takes in a Callable (which specifies a call method) or a
  * Runnable (which specifies a run method). The fork method returns a Par (function that takes in an ExecutorService and returns a Future)
  * for the implementation of the returned Par, it takes in an ExecutorService and calls submit on it with a anonymous Callable object (object from
  * anonymous class that implements Callable interface in java) the unevaluated Par (Parallel Computation) passed into fork is used to
  *  implement the call method in the Callable sent to submit. It takes the Par and feeds it along with the ExecutorService passed into it
  * to run which will return a Future which get will be invoked on  (which blocks the calling thread until there is a result)
  * with those arguments. (The Par is a unit, fork, map2 or some composition of these primitives). The submit method on the ExecutorService will return
    a Future (when the Par returned from fork is invoked with an ExecutorService). The Callable submitted to ExecutorService will (presumably)
    sit in a queue until a thread becomes available. When a thread is available it will pull the Callable from the queue and invoke its call method
    In this implementation of call the get method is used which will block the thread until a result comes back. Now suppose we provisioned a threadpool
    using the static method newFixedThreadPool(1) with only one thread. Then fork { x } == x only if x is a composition of unit primitives or 
    in other words it does not fork off the calling thread. if x does fork of the calling thread then I think with fixed resource of 1 worker 
    thread the computation will hang because fork { x } will send x to our one available worker thread and when that worker thread
    invokes call on the Callable sent to it, get method will block this worker thread but x 
    which when run will be submitted to a queue waiting for a thread to be available (since it forks) but the one thread that we have is blocked waiting
    on a returned value for x. So in effect x is waiting on a thread to become available so it can run its computation and return a value
    while the thread we have (not available) is waiting on x to return a value. and we end up in a stalemate. but it we just run x from the main thread
    it will fork of to our worker thread leaving main with a Future for reference. and Once the thread finished execution of x our Future will contain a
    value (result of x) therefore fork { x } != x and our api Algebra is broken. For this implementation we have a dependency on number of threads.
    We will have to consider for every parallel computation the minimum number of threads needed in order for the parallel computation to run. This
    is (for me) too much work not to mention hard/error prone, a hassle, difficult to reason about 
    and doesn't make my life easier which for me defeats the purpose of this api. (The below test demonstrates that forking is directly linked to system
    threads? I say this because running a computation that needs forks once (lazyUnit(42 + 1)) works on a threadpool of size 1, 
    but running a computation that needs forks twice (fork { lazyUnit(42 + 1) }) deadlock a threadpool of size 1)
  */

    def fork[A](a: => Par[A]): Par[A] = (s: ExecutorService) =>
      s.submit(new Callable[A] {
        def call(): A = run(a)(s).get()
      })

/**
  * lazyUnit just forks a "promoted" parallel computation off the main thread
  */

    def lazyUnit[A](a: => A) = fork(unit(a)) //derived combinator

    def run[A](a: Par[A])(s: ExecutorService): Future[A] = a(s)

/**
  * I think this function just takes a function (That get evaluated on the main thread when called) and
  * returns a function that potentially gets evaluated off the the main thread when called
  * (in fact this returns a Par for any input so it won't be ran until I send in an ExecutorService)
  */

    def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))
    
    def delay[A](a: => Par[A]): Par[A] =
      (s: ExecutorService) => a(s)

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      (s: ExecutorService) =>
        val index = run(n)(s).get() % choices.size
        choices(index)(s)

    def choice[A](p: Par[Boolean])(l: Par[A], r: Par[A]): Par[A] = choiceN(map(p) {
      case true => 0
      case _ => 1 
    })(List(l, r))

      
      
    def choiceNViaFlatMap[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(n)(a => choices(a % choices.size))
    
    def choiceViaFlatMap[A](p: Par[Boolean])(l: Par[A], r: Par[A]): Par[A] = flatMap(p)(b => if b then l else r)

    def choiceMap[K, V](k: Par[K])(pEntries: Map[K, Par[V]]): Par[V] = flatMap(k)(key => pEntries(key))

    def flatMapViaJoin[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))

/**
 * so I specified that I want the result of the first parallel computation to be applied to f but f returns a Par[B] so I will get a
 * Par[Par[B]] back indicating that I need to wait for the first parallel computation before I can run the second one. I don't think this is actually a nested
 * computation (like it would be for a List[List[A]] so in this case nested types indicate a pending computation that another computation depends on
  */
    def flatMapWithoutJoin[A, B](a: Par[A])(f: A => Par[B]): Par[Par[B]] = map(a)(f)
    //inner[outer[]] weird
    /**
     * this is wrong!!! but the trace of nested forks is right so I will keep it.
     * (I think a Par[Par[A]] is a parallel computation that returns a parallel computation which resolves to a result of type A for example on main thread
     * run(fork(fork(unit(3))))(s).get: A will be called (blocking the main thread) which will be submit new Callable { def call: A = run(fork(unit(a)))(s).get }
     *  to a queue and when a thread is ready it will "call" run(fork(unit(3)))(s).get on thread_1 (and in this case block the thread it's calling from)
     * this will submit new Callable { def call: A = unit(a).get } and when a thread is available it will call unit(3).get on thread_2
     *  will will immediately return 3 this will cascade to the previous call on thread_1 and this will cascade to the main thread returning 3)
      */
    def join[A](a: Par[Par[A]]): Par[A] =
      (s: ExecutorService) =>
        val outer = run(a)(s).get()
        outer(s)

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(res => res)
          
/**
  * Since we made the decision for unit to not fork off the main thread, Based on our representation of Par we actually don't need to use the
  * ExecutorService. We just need wrap the expression pass to unit in a Future. We need to create a custom Future Object since we will not
  * be getting one from submitting one to Task to ExecutorService
  */

/**
  * In this implementation of sumPar the return expression (at the end)
  * the first term in the expression is evaluated first and then the second term is evaluated (not parallel) in order for this to run parallel
  * I would need sumL to immediately beginning running (in another thread and not blocking the main thread) and immediately sumR can run
  * so that we can get concurrency. expressions R.H.S of sumL and sumR in the returning expression breaks referential transparency. if you make the substitution
  * the program wont be parallel because the last expression will evaluate its first term (left term) first. substitution changes the behavior of the
  * (a side effect) function from parallel to sequential after direct substitution into get. Note that this problem arose because we were trying
  * to get the combine the result of 2 parallel computations. So we need to figure out how to combine to parallel computations without running it.
  * I chose my map2 functions to take in 2 parallel computations which make sense since it's what I've done for Options, Either, State[A, S]
  * I chose to have my parameters in map2 by name because if it's by value then arguments will be evaluated sequentially which will defeat purpose
  * of combining parallel computations while keeping the fact that they remain parallel invariant. because this example uses unit in the base case
  * (this is the case where a value will be returned and there will not be any more recursive calls) it's up in the air if arguments to unit need to be by name
  * 
  */

    def sumPar(as: IndexedSeq[Int]): Int =
      val length = as.size
      if length <= 1 then as.headOption getOrElse 0
      else
        val (l, r) = as.splitAt(length / 2)
        val sumL = unit(sum(l))
        val sumR = unit(sum(r))
        get(sumL) + get(sumR)

    def sumParV1(as: IndexedSeq[Int]): Par[Int] =
      val length = as.size
      if length <= 1 then unit(as.headOption getOrElse 0) //base case delivers a parallel computation created from unit
      else
        val (l, r) = as.splitAt(length / 2)
        sumParV1(l).map2(sumParV1(r))(_ + _)

/**
  * map2(unit(1), unit(1))(_ + _) will fork a separate thread for its two operands. This doesn't make sense since this is not a long running computation
  * currently, our API doesn't have an explicit way to make explicit when we need to create a separate thread off the main thread to run a computation
  * arguments sent to map2 automatically are sent to a separate logical thread we need a way to explicitly specify that we would like to fork a computation
  * of the main thread. Question: for the function passed into a higher order function that takes its argument by-name, does the argument
  * of the passed in function get evaluated if it is strict? the book makes the decision to have the argument passed in fork an unevaluated. I'm 
  * not sure why? This implementation below doesn't fork to a new logical thread in the base case. because fork gives us explicitly control of evaluation
  * strategy (by-name parameters) and when to create a separate logical threads we don't need unit to have the ability to handle when to evaluate it's
  * arguments. unit can be made strict
  */

    def sumParV2(as: IndexedSeq[Int]): Par[Int] =
      val length = as.size
      if length <= 1 then unit(as.headOption getOrElse 0)
      val (l, r) = as.splitAt(length / 2)
      fork(sumParV2(l)).map2(fork(sumParV2(r)))(_ + _)

/**
  * fork signals that it's argument is earmarked for execution in a separate logical thread. This is the parallelizing strategy. But what about
  * whether it should be done immediately (the arguments are by name so arguments are unevaluated but the function body will evaluate
  * when called. We have a choice of whether we would like to return a function that requires another argument, deferring sending computation off to another
  * thread until some other requirement is met (another argument passed)) or if we would like to execute forks body (sending the computation
  * off to another thread immediately)
  */

  /**
    * whenever you run into a design choice where the tradeOffs aren't immediately clear or there is some ambiguity of the impact of selecting one design
    * choice over another, it can be useful to consider the information needed to implement each design decision. 
    * In the case of choosing whether to have fork or get begin immediately evaluating computation in thread/thread pool, we can start
    * by thinking about what sort of information we would need to execute the parallel computation for each choice. In order to have fork submit a 
    * computation to execute in an actual thread/thread pool, we would need a globally accessible and initialized resource (thread/thread pool) available
    * for use. usage of fork is now tied to have resources available but so far forks sole purpose of earmarking what should be run in a logical thread
    * shouldn't need to have this dependency in order for it to be used. I should be able to say what I want to run on a separate thread off the main
    * thread without actually having those resources accessible. The book gives an example where we may have subsystems of a large application where
    * a different configuration/service that facilitate parallel computation are needed which hints that we should not force parallel evaluate when 
    * fork is called. We began with Par being a container for a parallel computation that we can pull a value out of when it became available (like a Future).
    * our reasoning about fork has shifted the original conception so that Par is now a description of a parallel computation (what it should do) which fork will hold
    * onto (unevaluated) until it gets ran (So par will behave similar to a state action?) For now I will use the books representation of Par  ExecutorService => Future
    * but I will be looking the benefits of having an ExecutionContext representation. 
    */

    extension [A](par: Par[A])
      def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] =
        (s: ExecutorService) =>
          val af = par(s)
          val bf = b(s)
          UnitFuture(f(af.get(), bf.get()))
  
      def map[B](f: A => B): Par[B] = map2(Par.unit(()))((a, _) => f(a))
  
      def flatMap[B](f: A => Par[B]): Par[B] =
        (s: ExecutorService) =>
          val res = Par.run(par)(s).get()
          f(res)(s)

case class UnitFuture[A](get: A) extends Future[A]:
  def isDone(): Boolean = true
  def get(timeout: Long, unit: TimeUnit): A = get
  def isCancelled(): Boolean = false
  def cancel(mayInterruptIfRunning: Boolean): Boolean = false

object TestForkInvariant:
  import ChapterSevenExercises._
  @main def printTestForkInvariant: Unit =
    val s: ExecutorService = Executors.newFixedThreadPool(1)
  
    val par1 = lazyUnit(42 + 1)
    val par2 = fork { par1 }
    //println(run(par1)(s).get())
    println(run(par2)(s).get())

object TestParReduce:
  import ChapterSevenExercises._
  @main def printTestParReduce: Unit =
    val s: ExecutorService = Executors.newFixedThreadPool(80)
    
    /*val testSeq = IndexedSeq(18, 10, 19, 23)
    val testList = List(18, 10, 19, 23)
    val resultPar = parReduce(testSeq, (a1, a2) => if a1 > a2 then a1 else a2)
    val future = run(resultPar)(s)
    val result = future.get()
    val resultPar2 = parMap(testList)(x => x / 2)
    val future1 = run(resultPar2)(s)
    val result2 = future1.get()*/
    val testList1 = List("the quick brown fox over the verdant hill", "the quick brown fox over the verdant hill", "the quick brown fox over the verdant hill", "the quick brown fox over the verdant hill", "the quick brown fox over the verdant hill", "the quick brown fox over the verdant hill", "the quick brown fox over the verdant hill", "the quick brown fox over the verdant hill")
    val resultPar3 = countWords(testList1)
    val future2 = run(resultPar3)(s)
    val result3 = future2.get()
    //println(result)
    //println(result2)
    println(result3)