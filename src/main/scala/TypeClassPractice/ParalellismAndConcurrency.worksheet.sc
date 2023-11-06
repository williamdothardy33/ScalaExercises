






















































import java.util.concurrent.ForkJoinPool
import java.util.concurrent.{Executor, ExecutorService, Executors}
import scala.concurrent.{Future, ExecutionContext}

/**
    * From what I can glean this is the instantiation of a Thread object that takes a no-arg/unit return anonymous function that will get executed off the main
    * thread. This object comes with a start method which will allow you to fork off the main thread and executive the anonymous function immediately
    * According to youtube video a Thread represents a "linear flow of execution" managed by the O.S schedular
    * Execution Order depends on the number of instructions and the number of threads
    * The concept of a thread pool is that there is an executor service that you can submit computations (tasks) to for concurrent execution it will store a
    * backlog of submitted tasks in a queue (F.I.F.O) it will be responsible for managing this queue and the utilization of one or more threads that are
    * available for use. Executor manages the submission of task to be run (it doesn't really legislate how it will be run)
    * Executors has factor method for a whole bunch of types in concurrent package. ExecutorServices can manage termination and methods that can produce
    * a Future (I'm assuming a Future is an IOU for a value to be returned from a submitted computation to the ExecutorService)
    *
    * @param i
    * @return
    */
def task(i: Int) =
    new Thread(() =>
        println(s"Instruction 1: Task $i")
        println(s"Instruction 2: Task $i")

    )

task(2)

def otherTask(i: Int): Future[Unit] = Future {
    println(s"Instruction 1: Task $i")
    println(s"Instruction 2: Task $i")
}

implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(
    Executors.newFixedThreadPool(2))

val ec3: ExecutionContext = ExecutionContext.fromExecutor(
    Executors.newFixedThreadPool(3))
/**
    * This one uses all available threads for otherTask
    */
//implicit val ec: ExecutionContext = ExecutionContext.global
/**
    * the below allows use to constrained the number of threads used for otherTask
    */

def someOtherTask(i: Int): Future[Unit] = Future {
    println(s"Instruction 1: Task $i")
    println(s"Instruction 2: Task $i")
}(ec3)


val service: ExecutorService = Executors.newFixedThreadPool(2)

(1 to 1000).foreach(i =>
    service.submit(task(i))    
)
/**
    * An instance of Thread needs code to run. There are Two ways to do this:
        1) provide a instance/anonymous object (I think in scala a lambda works
        so I don't need to type new Runnable.../Runnable) of type Runnable (needs a run method)
    */

class HelloRunnable extends Runnable:
    def run(): Unit =
        println(s"Hello from thread: ${Thread.currentThread().getName()}")
println(s"Hello from main thread: ${Thread.currentThread().getName()}")
val helloTask = Thread(HelloRunnable())
helloTask.start()
/**
    * 2) you can subclass Thread because Thread implements Runnable (but doesn't do anything with run method)
    */

class HelloThread extends Thread:
    override def run(): Unit = println(s"Hello from thread: ${Thread.currentThread().getName()}")

val helloThreadTask = HelloThread()
helloThreadTask.start()

/**
    * use Thread.sleep(t: ms/ns) to suspend execution of the current thread and make
    * make processor time available to other threads. Thread.sleep throws InteruptedException (you 
    * have to write code to handle it) if the reference of the thread is currently suspended (with sleep)
    * and the method interrupt is invoked on that reference
    */

val sleep5Task = Thread(() => {
    try
        println("getting some shut eye for 5 seconds")
        Thread.sleep(5000)
        println("I'm awake!")
    catch
        {
            case e: InterruptedException => println("WHO WOKE ME UP!")
        }
})

val sleep3Task = Thread(() => {
    println("getting some shut eye for 3 seconds then waking sleep5Task up")
    Thread.sleep(3000)
    println("waking sleep5Task up")
    sleep5Task.interrupt()
})

val sleep5 = sleep5Task
val sleep3 = sleep3Task
sleep5.start()
sleep3.start()

/**
    * default behavior for many prepackaged methods are to cease execution (return from run) of
    * thread task if the thread is interrupted (this behavior is implemented in catch block)
    * You can also periodically check if a Thread has be interruped with Thread.interrupted(): Boolean if you
    * have a method that does not listen for this.
    */

val sleep4Task = Thread(() => {
    try
        println("getting some shut eye for 4 seconds")
        println("sleep6Task will have to wait for me!")
        Thread.sleep(4000)
        println("I'm awake after 4 seconds!")
    catch
        {
            case e: InterruptedException => println("WHO WOKE ME UP!")
        }
})

val sleep6Task = Thread(() => {
    println("getting some shut eye for 6 seconds, but first I have to wait for sleep4Task")
    /**
        * the effect of every statement in sleep4Task is visible to sleep6Task because
        * join (as well as start) creates a happens before relationship)
        */
    sleep4Task.join()
    Thread.sleep(6000)
    println("I'm awake after 6 seconds!")
})

val sleep4 = sleep4Task
val sleep6 = sleep6Task
/**
    * the effect of every statement before forking off to a new thread is visible in the new thread
    */
sleep4.start()
sleep6.start()

/**
    * Threads communicated by sharing access to fields (What is this? I'm assuming this is another name
    * for a variable perhaps a global oee) and the object references fields refer to (per java documentation.
    * I will try to make this into simpler language) Two problems can arise because of this communication
    * 1) Thread Interference (Is this another name for deadlock)
    * 2) Memory consistency problems (Is this another name for race conditions)
    * A solution is Synchronization. But this introduces another problem
    * Thread contention (per documentation starvation and livelock are examples of this. I don't know what this means)
    * I can demonstrate how thread interference will work for with print statements below. For this example
    * we have some shared object with a state count and two methods that increment/decrement the count respectively
    * even though these look like  simple operations they are actually broken up even further when compile down to machine instructions?
    * So interleaving of these instructions may cause inconsistent states
    * incrementing can be broken down into
    * 1) retrieve the value of the count
    * 2) add one to this value
    * 3) assign the result back to the count value
    * 
    * decrement
    * 1) retrieve the value of the count
    * 2) subtract one from the value
    * 3) assign result back to count value
    * 
    */

val incrementTask = Thread(() => {
    println(s"retrieve the value of the count. This is from Thread ${Thread.currentThread().getName()}")
    println(s"add one from the value This is from Thread ${Thread.currentThread().getName()}")
    println(s"assign result back to count value This is from Thread ${Thread.currentThread().getName()}")
})

val decrementTask = Thread(() => {
    println(s"retrieve the value of the count This is from Thread ${Thread.currentThread().getName()}")
    println(s"subtract one from the value This is from Thread ${Thread.currentThread().getName()}")
    println(s"assign result back to count value This is from Thread ${Thread.currentThread().getName()}")
})

val iTask = incrementTask
val dTask = decrementTask

iTask.start()
dTask.start()

/**
    * I got this result after run in the REPL:

    retrieve the value of the count. This is from Thread Thread-0
    retrieve the value of the count This is from Thread Thread-1
    add one from the value This is from Thread Thread-0
    subtract one from the value This is from Thread Thread-1
    assign result back to count value This is from Thread Thread-1
    assign result back to count value This is from Thread Thread-0

    The interleaving of these instructions will cause The subtraction to be lost
    and the value will end of being 1 (From Thread-0)
    */

/**
    * using synchronized keyword on methods that mutate some global state will enforce happens before
    * relationship between thread currently mutating some global state and another thread that needs to
    * get access to the same state. methods annotated with synchronized will ensure that if they
    * are currently using a shared memory resource any other thread in contention with that shared
    * memory resource will be block and the result of the synchronized method on the shared memory
    * resource will be visible to subsequent synchronized methods invocations. The mechanism that a shared resource uses to indicate
    * whether it is open or close for mutation of its member fields is intrinsic lock. before a thread can access an access/mutate an objects
    * fields it must acquire an intrinsic lock. Any other thread the tries to acquire the intrinsic lock while this lock is owned already by another thread
    * will block (meaning it will suspend execution until it can acquire the lock) If multiple synchronized methods use the same lock
    * (Does this mean they are defined on the same top level object?) A Thread may acquire this lock multiple times (in order to invoke multiple
    * synchronize functions I believe?) The documentation specifies a use case where synchronized code (directly/indirectly) calls a method that is
    * also synchronized.
    */
/**
    * A synchronized statement provides a finer granularity for the declaration that you would like
    * a statement to have a happens before relationship with respect to Another Thread accessing the same
    * resource that that statement modifies. You have to specify the object the provides the intrinsic lock
    * (usually "this" so synchronized(this) { statements that modify some shared mutable state })
    * Invoking other objects methods in a synchronized block can cause problems. I don't know if this
    * would force the object whose method is invoked to provide a lock or what kind of problem it would
    * cause but this is direct from documentation
    */

/**
    * if you have members in an object that need to be synchronized but not with respect to each other
    * you can create Objects specifically to provide intrinsic locks so that they can be interleaved
    * with respect to each other but also be synchronized w.r.t other synchronized methods that mutate
    * them (This may not work in scala)
    * Atomic access is an all or nothing execution of instructions that you can specify in your
    * program. for ex. you can specify reads and writes are atomic for ref/all variables with volatile
    * annotation. They cannot be interleaved (all or nothing)
    * Liveness is a concurrent application's ability to complete execution in a timely manner
    * Deadlock is a situation where at least 2 thread are block (suspended execution) forever waiting on
    * each other (to release/acquire a lock for an object with a synchronized method)
    * Starvation: A thread is unable to gain access to a shared resource (acquire a lock from an object
    * with a synchronized method) so that it cannot make any progress on it's Task
    *  because another (greedy) Thread is monopolizing the resource. This could happen if 
        the synchronized method is a long running process which is called frequently by some other thread(s)
        Livelock: you have a situation where one Thread acts in response to another thread, and the other
        acts in response to a third thread. But the third Thread is acting in response to the first thread
        so three triggers two which triggers one which triggers three in a cycle? So the Threads are too busy
        keeping each other busy to make any progress on Tasks
        Guarded Block. A way to coordinate Actions on different threads. One strategy is to poll a flag variable
        (with something like a while loop) and call wait() which throws and InterruptedException so that
        execution is suspended until the Thread is interrupted and then poll again
    */

object SynchronizedObjects:
    var member1: Int = 0 
    var member2: Int = 0 
    val lockObj1: Object = Object()
    val lockObj2: Object = Object()
        lockObj1.synchronized {
            def incMember1: Unit =
                member1 = member1 + 1
        }

        lockObj2.synchronized {
            def incMember2: Unit =
                member2 = member2 + 1
        }
class Friend(val name: Option[String] = None)

extension (f: Friend)
    def bow(toAlreadyBowing: Friend): Unit =
        synchronized {
            println(s"${f.name getOrElse ""} will bow to ${toAlreadyBowing.name getOrElse ""}. This function was called by thread: ${Thread.currentThread().getName()}")
            toAlreadyBowing.bowedBack(f) 
            /**
                * There will be contention for this function or
                */
        }
        
    def bowedBack(from: Friend): Unit =
        synchronized {
            println(s"${f.name getOrElse ""} will received a bow from ${from.name getOrElse ""}. This function was called by thread: ${Thread.currentThread().getName()}")
        }
        


val john = Friend(Some("John"))
val paul = Friend(Some("Paul"))

val johnBowTask = Thread(() =>
    john.bow(paul)
)

val paulBowTask = Thread(() =>
    paul.bow(john)
)

/**
    * I think both threads will be waiting for the other to release the lock so they can call bowBack?
    */
    

/**
    * Executors abstract away the creation and management of Threads themselves from the creation of  Tasks (Runnable) to be ran off the main Thread
    * (I've be creating a thread sending it a task via constructor starting a thread and I probably should have been destroying a thread after my
    * task was done running and I received a result or something back) Executors deal with the lifecycle and configuration of Threads
    * and all you have to do is submit task that need to be ran off the main thread
    */

/**
    * Java.util.concurrent package has 3 interfaces (as of java5)
    * Executor: defines one method execute which supports launching new tasks. It may do this in many ways and in the most basic
    * way may for some runnable r execute(r) may just call Thread(r).start()
    * ExecutorService: A subinterface of Executor that also manages lifecycle of tasks as well as it's own life cycle? It defines a method submit()
    * which accepts a Callable (Which returns a value) and a Runnable (which is a side-effecting closure?/object). It returns a Future object which
    * is used to retrieve the returned value of a Callable Task as well as manage the status of Callable/Runnable Tasks
    * The ScheduledExecutorService: Which includes features for scheduling
    * ThreadPools are worker Threads that exists outside of Callable/Runnable task that use an internal queue to manage active task. 
    * (It pulls from this queue when a thread is available and when Tasks exceed available threads the tasks go in the queue). There are a bunch
    * of static factory methods like newFixedThreadPool That allow you to create a Thread pool easily. newCachedThreadPool allows you to create
    * an expandable Thread Pool
    */

/**
    * ForkJoin Framework: an implementation of executor service that allows you to take advantage of multiple processors (This may be what I need for 
        my Par[T] implementation!) It is designed to distribute tasks over worker threads in a thread pool but in addition at its core is the work
        stealing algorithm which allows threads that are not being utilized to steal work from busy threads. ForkJoinPool which is a implementation
        of AbstractExecutorService implements the work stealing algorithm and can execute ForkJoinPool Tasks
        Basic Use The first step for using the fork/join framework is to write code that performs a segment of the work. Your code should
        look similar to the following pseudocode:

        if (my portion of the work is small enough)
            do the work directly
        else
            split my work into two pieces
            invoke the two pieces and wait for the results

    Code in this form can be wrapped in a ForkJoinTasks or one of it's subclasses which include RecursiveTask (which returns a result)
    and RecursiveAction (side-effecting?). Then create an object to represent all the work to be done and pass it into the invoke method of a ForkJoinPool
    instance to begin execution. So basically according to the document it is enough to extend either RecursiveAction or RecursiveTask define the mothods
    compute which you wrap you basic case in (The condition where you can compute you task directly without spawning off a new thread) and
    pass in you recursive cases to invokeAll(). Then create a ForkJoinPool instance and pass in an instance of your RecursiveTask/RecursiveAction into it's
    invoke method
        */




    


enum Direction(dx: Int, dy: Int):
    case Right extends Direction(1, 0)
    case Up extends Direction(0, 1)
    case Left extends Direction(-1, 0)
    case Down extends Direction(0, -1)

    def leftTurn = Direction.values((ordinal + 1) % 4)

val left = Direction.Left




trait Fruit
class Apple extends Fruit
class Orange extends Fruit

type fToO = Fruit => Orange
type aToF = Apple => Fruit
val getOrange: fToO = (x: Fruit) => Orange()
val convertAppToFruit = (x: Apple) => x.asInstanceOf[Fruit]

val f: aToF = getOrange
/**
    * Here Fruit input is assigned a more specific type (apple which defines contravariant relations) 
    * and Orange is assigned a more  generic type (Fruit which defines covariant relationship)
    * 
    * fToO <: aToF because this is the case I can assign a function of type fToO to a variable of type aToF
    */

/**
    * Liskov Substitution Principle says that if A <: B then anything I can do with B, I can do with A.
    * If I have List[Fruit] ls ls.prepend(Orange()) is valid, now suppose as: List[Apple] is a List[Fruit], then as.prepend(Orange()) 
    * should be valid, but it isn't (we will get a type error) so List[Apple] is not a List[Fruit] But List[Fruit] is a List[Apple] 
    * 
    * To pass type checks we can use rule that covariant type parameters me appear in lower bound of method type parameters
    * (so if List[+T] then prepend[U >: T](elem: T, ...) is valid because T is a lower bound )
    * and contravariant type parameters may appear in upper bound of method type parameter
    * (so List[-T] then head[U :< T]: T is valid because T is an upper bound?)
    */


            
            
        
        
        





