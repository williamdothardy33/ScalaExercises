/**
  * Each signal maintains
  * - its current value
  * - the current expression that defines it's signal value
  * - a set of observers: the other signals that depend on its value
  * (this would be other signals derived from this signal
  * if this signal changes derived signals need 
  * to reflect that change)
  * 
  * How do we record the dependencies of observers (derived signals)
  * -When evaluating an expression that resolves to a signal value
  * we need to know which signal gets (potentially later on)
  * defined or updated (derived signal or observer) 
  * by the expression. We call these derived signals, "callers"
  * of this signal
  * - if we know what observer signals ("callers") are 
  * dependent upon the expression resolving to a signal 
  * value,  then executing this signal resulting from the 
  * expression means adding observers (derived signals or callers)
  * to the "observers" of this sig 
  * (I don't know how this works???) (is this analogous
  * to when a Publisher publishes it notifies all of it's
  * subscribers???) So Oderski says that any derived signal
  * that request's a value from the signals they are derived from
  * gets added to the "observers set" of the signals they are derived
  * from.
  * - When a sig's value changes all previously observing signals (derived signals)
  * get re-evaluated (I don't know what this means) and the
  * set sig.observers (I'm assuming this is a set of signals
  * that depends on this sig, which this sig maintains) is cleared
  * re-evaluation (of what?)
  * (Odersky say's the very act of a signal demanding a value
  * from another signal get's the entry into that signal's
  * observers set) 
  * will re-enter calling signals (derived signals) 
  * into this signal's observers set as long
  * as the derived signal still depends on sig
  * Odersky gives an example: (<- means derived from)
  * suppose s1 <- f(s2, s3) if s2 or s3 changes s1 needs 
  * to be re-evaluated for example if s1 is a Signal Var
  *  then it could re-evaluate to
  * a different expression say s1 <- g(s3, s4)
  * when this re-evaluation happens s1 is cleared from
  * s2 and s3's observers set. When s1 is called after 
  * this re-evaluation it will again gain entry in s3's observers
  * and it will also gain entry into s4's observers set
  * an implementing abstract class for the signal trait will be
  * placed in the signal trait's companion object
  * this is a common implementation technique that we can use
  * to hide global implementation details in an enclosing object.
  * (How does this achieve it???) We achieve this "hiding"
  * by "shadowing" the Abstract class with an opaque type alias
  * which will not "leak" (allowing client code to see underlying abstraction)
  * when this object is imported for use by client code
  * A signal value (currentValue) is evaluated (using compute which uses eval) only :
    - on initialization
    - when a signal for whom it is a observer (part of the observers set of that signal) changes
    (that happens via the observer handle in the observers set of the signal that this signal depends on)
    when that signal is evaluated (presumably when it changes)
    it "forces" a recompute of all the signals it has a handle for in it's observers set 
    and then it clears it's observer set (the re compute "forces" the previous observers to evaluate and if
    they are still dependent on the signal they will be re-entered in the signals observers set)
    Note eval returns a function of type () => T and is defined as a method (with def)
    an implementing method of eval may be defined as a val/var as long as it returns the same type??? our implement "val"
    raps an unevaluated expr of type T into a thunk (() => expr)
    In this code we must make a decision about how to calling Signal will get passed to the callee signal so that the callee
    can add the caller to it's list of observers. We could pass it explicitly as a dependency so that when the caller needs
    a callee we would have callee(caller) this is lots of work. We could also use specifying implicitly that caller will get
    passed with a using clause. But this is also a lot of work and definition and call sites will be very "loud"
    We can use implicit function types to avoid having to specify passing a parameter with a using clause. This is the
    direction taken in the book. We declare an implicit function type type ObserverEnv[T] = Observer ?=> T. This can be taken
    to mean Something that yields a T in the context of the ObserverEnv. 
    Now for any function/expression that resolves to a T we can pass an Observer implicitly without specifying a using
    clause just by declaring that the type/return type of that expression/function is ObserverEnv[T]
    t
  */
import Signal._

trait Signal[+T]:
    def apply(): ObserverEnv[T] //I think this is what allows you to query a signal like a function (It needs a given instance)

object Signal:
    given neutralObserver: Observer =
        new AbstractSignal[Nothing]:
/**
 * this signal is only used when we need a default signal to send as a caller of some other signal we need so
 * we can have computeValue return () and the eval method which is only called when we compute a value (on initialization
 * and when a signal that it has observed or called changes) will be left undefined
  */
            override def eval: ObserverEnv[Nothing] = ???
            override def computeValue() = ()

    opaque type Observer = AbstractSignal[?]
/**
 * caller allows me to get a handle for the implicitly passed observer in scope (same thing as summon)
  */
    def caller(using o: Observer): Observer = o
    type ObserverEnv[T] = Observer ?=> T

/* creates a signal in the context of ObserverEnv*/

    def apply[T](expr: ObserverEnv[T]): Signal[T] = //this is a factory method allowing you to create a new signal
        new AbstractSignal[T]:
            val eval = expr
            computeValue()
    end apply

    /*def apply[T](expr: => T): Signal[T] =
        new AbstractSignal[T]:
            val eval = () => expr
            computeValue()
    end apply*/
    
    abstract class AbstractSignal[+T] extends Signal[T]:
        private var observers: Set[Observer] = Set()
        private var currentValue: T = _

        protected def eval: ObserverEnv[T]

        protected def computeValue(): Unit =
/**
  * so the signal passes itself as the context when computeValue is called ???
  * Odersky says that the current signal will show up as a dependency (put in its own observers set???) when the eval
  * method gets called (very confused) perhaps later on we will redefine apply method in terms of eval method???
  * Ok so when the factory method apply gets called to create a signal, the method does 2 things. 
  * - define it's eval as the expression passed into it.
  * - calls computeValue to initialize it's currentValue. When computeValue is called a reference is created containing
  * the value that eval resolves to. a self reference is pass implicitly to eval. If the expr that eval is defined as
  * contains terms that are other signals this self reference with get passed to them as well (it will take priority over
  * all values for a given instance since it is passed explicitly) this is how callee signals get a handle on who called them
  */
            val newValue = eval(using this) //this is where the signal passes itself as the caller to callee signals
            val shouldNotify = !observers.isEmpty && currentValue != newValue
            currentValue = newValue
            if shouldNotify then
                val previousObs = observers
                observers = Set()
                previousObs.foreach(_.computeValue())
        override def apply(): ObserverEnv[T] =
            println(implicitly[Observer])
            observers += caller
            assert(!caller.observers.contains(this), "cyclic signal definition")
            currentValue
    end AbstractSignal

    class Var[T](initExpr: ObserverEnv[T]) extends AbstractSignal[T]:
        protected var eval = initExpr
        computeValue()

    /*class Var[T](initExpr: => T) extends AbstractSignal[T]:
        protected var eval = () => initExpr
        computeValue()*/

/* updates a Signal.Var in the context of an ObserverEnv. Not clear on why we need to do this here.*/
        def update(newExpr: ObserverEnv[T]): Unit =
            eval = newExpr
            computeValue()

        /*def update(newExpr: => T): Unit =
            eval = () => newExpr
            computeValue()*/
    end Var


class SignalBankAccount:
    private var accountBalance: Var[Int] = Signal.Var[Int](0)

    def balance: Signal[Int] = accountBalance

    def deposit(amount: Int): Unit =
        val previous = balance()
        if amount >= 0 then accountBalance() = previous + amount

    def withdraw(amount: Int): Int =
        if amount > 0 && balance() - amount >= 0 then
            val previous = balance()
            accountBalance() = previous - amount 
            balance()
        else throw Error("insufficient funds")
end SignalBankAccount

def consolidated(as: List[SignalBankAccount]): Signal[Int] =
    println(implicitly[Observer])
/* apparently the Signal the we've defined here gets passed to any signals used in its creation (I can't really see it)*/
    Signal(as.map(_.balance()).sum)

val a = SignalBankAccount()
val b = SignalBankAccount()

val c = consolidated(List(a, b))

a.deposit(20)

c()
b.deposit(80)
c()

b.withdraw(35)
c()

