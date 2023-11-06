package ChapterSeven

package fpinscala.answers.parallelism

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.{Callable,ExecutorService}
import annotation.tailrec
/**
  * The book explains Actors as a concurrency primitive that facilitates code that needs to be accessed by multiple threads 
  * by "acting" as the go-between for those threads and the code. Threads will only send messages (results of their processing?) to an Actor
  * and if the Actor is available it will process these results one at a time. If it is currently processing it will queue received messages
  * for future processing. It only occupies a thread while processing a message and it makes sure that it interacts while the code in such a way
  * as to avoid race conditions/deadlocks etc. (atomically read/write?) 
  */
/*
 * Implementation is taken from `scalaz` library, with only minor changes. See:
 *
 * https://github.com/scalaz/scalaz/blob/scalaz-seven/concurrent/src/main/scala/scalaz/concurrent/Actor.scala
 *
 * This code is copyright Andriy Plokhotnyuk, Runar Bjarnason, and other contributors,
 * and is licensed using 3-clause BSD, see LICENSE file at:
 *
 * https://github.com/scalaz/scalaz/blob/scalaz-seven/etc/LICENCE
 */

/**
 * Processes messages of type `A`, one at a time. Messages are submitted to
 * the actor with the method `!`. Processing is performed asynchronously using the provided executor.
 *
 * Memory consistency guarantee: when each message is processed by the `handler`, any memory that it
 * mutates is guaranteed to be visible by the `handler` when it processes the next message, even if
 * the `executor` runs the invocations of `handler` on separate threads. This is achieved because
 * the `Actor` reads a volatile memory location before entering its event loop, and writes to the same
 * location before suspending.
 *
 * Implementation based on non-intrusive MPSC node-based queue, described by Dmitriy Vyukov:
 * [[http://www.1024cores.net/home/lock-free-algorithms/queues/non-intrusive-mpsc-node-based-queue]]
 *
 * @see scalaz.concurrent.Promise for a use case.
 *
 * @param handler  The message handler
 * @param onError  Exception handler, called if the message handler throws any `Throwable`.
 * @param executor Execution strategy
 * @tparam A       The type of messages accepted by this actor.
 */
final class Actor[A](executor: ExecutorService)(handler: A => Unit, onError: Throwable => Unit = throw(_)):
  self =>

  private val tail = new AtomicReference(new Node[A]())
  private val suspended = new AtomicInteger(1)
  private val head = new AtomicReference(tail.get)

  infix def !(a: A): Unit =
    val n = new Node(a)
    head.getAndSet(n).lazySet(n)
    trySchedule()
    /**   ^^
      * this is pretty confusing but I believe head.getAndSet will set the atomic reference of head to
      * have a value of
      * the node created from the value passed in. The get portion will be a Node reference to the old value
      * which I believe is the same as the reference used to initialize the tail. and lazySet will update the
      * tail to the new passed in value eventually. head is updated immediately and cpu cache is flushed
      * to common memory layer making change of value of head visible by all threads immediately and tail is
      * will updated by it's new value may not be immediately visible (I assume this is more efficient)
      * A is a type for which we have an Actor[A] that can handle (input) messages of type A
      * and we would like to bootstraps the functionality for type A to some type B so we use the pull-back mechanism
      * to get Actors[A]'s functionality onto some type B (create an Actor that can handle messages of type B)
      * we need a translation function f: B => A in order to use ! defined for Actor[A]. ! is used to submit
      * messages in Actor[A] and is used as a "handler" when creating Actor[B]
      *
      *
      */
      
  def contramap[B](f: B => A): Actor[B] =
    new Actor[B](executor)((b: B) => (this ! f(b)), onError)

  /**  ^^
    * we can use contramap to create a new Actor?
    */
  
  /**
    * compareAndSet will set atomic variable/reference to value if current value is equal to expected value
    * I believe compareAndSet "acts as a happens before edge" meaning change made by compare and set will be visible
    * to other threads once an update happens (If assuming a value of 1 means the suspended state is true
    * so this function if suspended is 1 will set the flag to 0 (false) and call schedule which
    * submits the function act to the ExecutorService (in the form of a call with it's call method set
    * to an invocation of act()
    */
  private def trySchedule(): Unit =
    if suspended.compareAndSet(1, 0) then schedule()

  private def schedule(): Unit =
    executor.submit(new Callable[Unit] { def call = act() })
    ()

  private def act(): Unit =
    val t = tail.get
    val n = batchHandle(t, 1024)
    if n ne t then
      n.a = null.asInstanceOf[A]

      /**
        * lazySet works like set but cpu cache isn't "flushed to common memory layer"
        * (local caches for other threads are not immediately updated so lazySet does not
        * "act as a happens before edge". Other threads will possibly read old values but eventually
        * it will be consistent. Common practice might be to use lazySet to set some reference
        * to null to trigger garbage collection and other thread will have access to old cached value? for a time
        */
      tail.lazySet(n)
      schedule()
    else
      suspended.set(1)
      if n.get ne null then trySchedule()
/**
  * This is where the Actor will call the handler passed in to it. From what I can infer the actor receives a message via ! queues it up to be processed
  * when it it available to process the message (if it's not currently processing some other message) it will call the handler used to create the Actor.
  * Any thread using the Actor must do 2 things. it's continuation (when it has a result) must send a message (with !) to the Actor with the result
  * and the Actor that you are using to liaison with the thread must know how to handle (type etc) the message that you pass to it so you will create it 
  * with a
  * side effecting (A => Unit) callback that prescribes what to do when it is available to handle your result. batchHandle() gets called when act() is called (which is done
  * asynchronously aka act() is wrapped in a Callable that is submitted to an executor service. the wrapping is done by schedule function which is 
  * called if trySchedule is successful and trySchedule is called when the actor receives a message)
  * So the sequence of events is ! (get a message) -> trySchedule (if successful) -> schedule (asynchronously) this is where
  * processing of message is submitted to ExecutorService -> act is invoked (in worker thread 1) ->
  * batchHandle is invoke (in worker thread 1) -> handler (used to create Actor) is invoked (in worker thread 1)
  */
  @tailrec
  private def batchHandle(t: Node[A], i: Int): Node[A] =
    val n = t.get
    if n ne null then
      try
        handler(n.a)
      catch
        case ex: Throwable => onError(ex)
      if i > 0 then batchHandle(n, i - 1) else n
    else t

private class Node[A](var a: A = null.asInstanceOf[A]) extends AtomicReference[Node[A]]

