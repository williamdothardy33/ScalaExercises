package Cheats

/**
  * Observer Pattern
  * - Mainly used when views need to respond to changes in a model
  * - Variants include MVC/Pub/Sub
  * in MVC we have a Model which hold some state that informs what the views should display at any given time
  * the view which only contains UI logic and the controller coordinates between the model and view to make sure 
  * the view is reflecting the current state
  * - Consequently it easy to setup multiple views for the same state because state has been "abstracted" away from the view
  * - It is simple to setup
  * Pub/Sub  gets rid of the middle man and makes the Model a publisher and the view a subscriber. 
  */


/**
 * Advantages of observer pattern
 * - Decouples views from state. Any logic dealing with state and its changes our outside of the scope of view, and 
 * any logic dealing with UI is outside the scope of state 
  * A disadvantage of Pub/Sub is that in a concurrent execution environment if a subscriber is subscribed to more than 1
  * publisher then multiple publishers have a handle to is handler function. If multiple publishers
  * invoke it's handler function at the same time there may be race conditions. making handler a synchronized function could
  * introduce  other issues like deadlock.
  * since handler (the handle to the subscriber which the publisher uses to notify the subscriber when there is a state change) is
  * invoke in publisher it cannot return a value. this implies that it must be a side affecting function
  * Views are still tightly bound to one state; view update happens immediately
  */
trait Subscriber:
    def handler(p: Publisher): Unit
end Subscriber

trait Publisher:
    self =>
        private var subscribers: Set[Subscriber] = Set()

        def giveSubscription(subscriber: Subscriber): Unit =
            subscribers += subscriber

        def takeSubscription(subscriber: Subscriber): Unit =
            subscribers -= subscriber

        def publish(): Unit =
            for 
                subscriber <- subscribers
            do subscriber.handler(self)
end Publisher

class PublisherBankAccount extends Publisher:
    private var balance = 0

    def currentBalance: Int = balance

    def deposit(amount: Int): Unit =
        if amount >= 0 then balance += amount
        publish()

    def withdraw(amount: Int): Unit =
        if amount > 0 && balance - amount >= 0 then
            balance -= amount
            publish()
        else throw Error("insufficient funds")
end PublisherBankAccount

class SubscriberAccountManager(observed: List[PublisherBankAccount]) extends Subscriber:
    self =>
        for
            account <- observed
        do account.giveSubscription(self)


        private var total: Int = _
        compute()

        override def handler(p: Publisher): Unit =
            compute()


        private def compute(): Unit =
            total = observed.map(_.currentBalance).sum

        def totalBalance = total
end SubscriberAccountManager
    
/**
  * Reactive programming is about reacting to a sequence of events that happen in time for example above if bank account publishes
  * (An Event) AccountManager reacts (gets it's handler called)
  * FRP view: Aggregate an event sequence into a signal
  * - A signal is a value that changes over time
  * - It is represented as a function from time to value domain (naturally)
  * -Instead of propagating updates to mutable state (like we do when an Account publishes after a withdrawal or deposit)
  * we define new signals in terms of existing one
  * so for example if we want to track the  movement of a mouse we could have a subscriber listen (subscribe to) for a
  * mouseMove event (publisher) and each time the mouse moves the state (xy position) of the publisher will be propagated 
  * to the listener (subscriber). This is pub/sub. The alternative is to model the sequence of Events as a signal 
  * (curve representing the trajectory of the mouse over time), a function from time to (xy position) 
  * an example of the difference implementation would be mouseMove(toPos: Position): Unit (the handler for a subscriber) 
  * which is side-affecting to mousePosition: Signal[Position]
  * Note f(E1, E2, ..., En) = E is translated to f.update(E1, E2, ..., En, E)  
  */
