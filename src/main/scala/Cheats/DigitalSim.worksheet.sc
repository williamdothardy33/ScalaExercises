

trait Simulation:
    type Action = () => Unit
    private case class Event(time: Int, action: Action)
    private type Agenda = List[Event]
    private var agenda: Agenda = List()
    private var currentTime: Int = 0
    def timeNow: Int = currentTime
    def afterDelay(delay: Int)(block: => Unit): Unit =
        val event = Event(timeNow + delay, () => block)
        val (earlier, later) = agenda.span(_.time <= event.time)
        agenda = earlier ::: (event :: later)

    private def loop(): Unit =
        agenda match
            case first :: rest =>
                agenda = rest
                currentTime = first.time
                first.action()
                loop()
            case Nil =>
        
    def run(): Unit =
        afterDelay(0){ println(s"***Simulation started, time: $timeNow***") }
        loop()
end Simulation

trait Gates extends Simulation:
    def inverterDelay: Int
    def andGateDelay: Int
    def orGateDelay: Int

    def inverter(in: Wire, out: Wire): Unit =
        def inverterAction(): Unit =
            val inSignal = in.getSignal
            afterDelay(inverterDelay)(out.setSignal(!inSignal))

        in.addAction(inverterAction)

    def andGate(in1: Wire, in2: Wire, out: Wire): Unit =

        def andAction(): Unit =
            val in1Signal = in1.getSignal
            val in2Signal = in2.getSignal
            afterDelay(andGateDelay)(out.setSignal(in1Signal & in2Signal))

        in1.addAction(andAction)
        in2.addAction(andAction)

    def orGate(in1: Wire, in2: Wire, out: Wire): Unit =

        def orAction(): Unit =
            val in1Signal = in1.getSignal
            val in2Signal = in2.getSignal
            afterDelay(orGateDelay)(out.setSignal(in1Signal | in2Signal))

        in1.addAction(orAction)
        in2.addAction(orAction)
    
    def probe(name: String, wire: Wire): Unit =
        def probeAction(): Unit =
            println(s"name: $name, time: $timeNow, value: ${wire.getSignal}")
        wire.addAction(probeAction)

    class Wire:
        private var sigVal = false
        private var actions = List[Action]()

    //returns value of current signal transported by wire

        def getSignal: Boolean =
            sigVal

    /*modifies signal transported by wire. In order for this new information
    (signal change) to be reflected in the state of the wire and the wire network
    attached to it after the actions are run over it we have to run the actions again under the new initial sigVal*/ 

        def setSignal(sig: Boolean): Unit =
            if sigVal != sig then sigVal = sig
            for 
                action <- actions
            do action()

/*registers Action to be performed when signal is changed. Here we only need to 
run the action over the current signal to reflect the new state of the wire network*/
        def addAction(action: Action): Unit =
            actions = action :: actions
            action()
end Gates

trait Circuits() extends Gates:

    override def inverterDelay: Int

    override def andGateDelay: Int

    override def orGateDelay: Int
    
    def halfAdder(aIn: Wire, bIn: Wire, s: Wire, c: Wire): Unit =
        val o, i = Wire()
        orGate(aIn, bIn, o)
        andGate(aIn, bIn, c)
        inverter(c, i)
        andGate(o, i, s)

    def fullAdder(aIn: Wire, bIn: Wire, cIn: Wire, s: Wire, c: Wire): Unit =
        val s1, c1, s2, c2 = Wire()
        halfAdder(bIn, cIn, s1, c1)
        halfAdder(aIn, s1, s2, c2)
        orGate(c2, c1, c)

    def notEqual(aIn: Wire, bIn: Wire, r: Wire): Unit =
        val i1, i2, o1, o2 = Wire()
        inverter(aIn, i1)
        inverter(bIn, i2)
        andGate(aIn, i2, o1)
        andGate(bIn, i1, o2)
        orGate(o1, o2, r)
end Circuits

trait Delays:
    def inverterDelay: Int = 2

    def andGateDelay: Int = 3

    def orGateDelay: Int = 5
end Delays

object sim extends Circuits, Delays

import sim._

val input1, input2, c, s = Wire()
probe("sum wire", s)
probe("carry wire", c)
halfAdder(input1, input2, s, c)

input1.setSignal(true)

run()

input2.setSignal(true)

run()

type Handler = () => Int

class TestClass1:
    private var state = 0
    def sharedHandler(newState: Int): Unit =
        state = newState

    private var handlers: List[Handler] = List()

    def addHandler(handler: Handler): Unit =
        handlers = handler :: handlers

    def runWhen(p: Int => Boolean): Unit =
        if p(state) then
            for 
                handler <- handlers
            do handler()

/*class TestClass3:
    def passHandler(from: TestClass1, to: TestClass2)

class Runner:
    case class Event(name: String, block: Handler)
    private var events: List[Event]*/
