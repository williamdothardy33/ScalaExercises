package ChapterSixState
import ChapterSixState.State.*

enum Input:
    case coin, turn

case class Machine(isLocked: Boolean, candyLeft: Int, coinsLeft: Int)


val insertStateUpdate: Machine => Machine =
    (m: Machine)  =>
        m match
            case Machine(isLocked, candyLeft, coinsLeft) if candyLeft > 0 && isLocked =>
                m.copy(isLocked = false, coinsLeft = coinsLeft + 1)

            case _ => m

val turnStateUpdate: Machine => Machine =
    (m: Machine) =>
        m match
            case Machine(isLocked, candyLeft, coinsLeft) if candyLeft > 0 && !isLocked =>
                m.copy(isLocked = true, candyLeft = candyLeft - 1)

            case _ => m

val selectStateUpdate: Input => Machine => Machine =
    in =>
        in match
            case Input.coin => insertStateUpdate
            case Input.turn => turnStateUpdate
        
        

def simulateMachine(inputs: List[Input]): State[(Int, Int), Machine] = traverse(inputs)(input => State.modify(selectStateUpdate(input))).flatMap(_ => get).map(s => (s.candyLeft, s.coinsLeft))
//def simulateMachine(inputs: List[Input]): List[State[(Int, Int), Machine]] = inputs.map(selectAction) 

val inputs = List(Input.coin, Input.turn, Input.turn, Input.coin, Input.coin, Input.turn, Input.coin, Input.turn, Input.coin, Input.turn, Input.coin, Input.turn)
val simulate = simulateMachine(inputs)

/*val testAction1 = (m: Machine) =>
    (Unit, m.copy(isLocked = false, coinsLeft = m.coinsLeft + 1))

val testAction2 = (m: Machine) =>
    (Unit, m.copy(isLocked = true, candyLeft = candyLeft - 1))*/

