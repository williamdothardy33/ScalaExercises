/**
  * The problem -
  * Name: Water Pouring Problem
  * Summary: You have a some specified amount of water
  * You have cups of various sizes (each cup can only 
  * hold a fixed amount of water). You can only empty
  * a cup, fill a cup or pour water from one cup to 
  * another until either the first cup is empty or 
  * the second cup is full. the task is given these 
  * constraints how can successively perform these 
  * "moves" until we have a cup that contains the 
  * specified amount of water
  * 
  * Glass type: each glass is identified with an int
  * State type: is represented with a Vector.
  * the index of an entry in the vector represents 
  * the corresponding glass identified with the same
  * integer. The entry at that index is identified
  * with the amount of water the glass represented by
  * that index has in it.
  */

type GlassId = Int
type State = Vector[Int]

class PouringSim(fullConstraints: State):
    enum Move:
        case Empty(glassId: GlassId)
        case Fill(glassId: GlassId)
        case Pour(from: GlassId, to: GlassId)

        def apply(state: State): State =
            this match
                case Empty(glassId) => state.updated(glassId, 0)
                case Fill(glassId) => state.updated(glassId, fullConstraints(glassId))
                case Pour(from, to) =>
                    val amount = state(from) min (fullConstraints(to) - state(to))
                    val fromAmount = state(from) - amount
                    val toAmount = state(to) + amount
                    state.updated(from, fromAmount).updated(to, toAmount)
    end Move
            

    val allMoves =
        val glassIds = 0 until fullConstraints.length
        (for 
            glassId <- glassIds
        yield Move.Empty(glassId)) ++
        (for 
            glassId <- glassIds
        yield Move.Fill(glassId)) ++
        (for 
            glassId1 <- glassIds
            glassId2 <- glassIds
            if glassId1 != glassId2
        yield Move.Pour(glassId1, glassId2))

    class Path(history: List[Move], val endState: State):
        def advanceFrom(next: Move): Path = Path(next :: history, next(endState))
        override
        def toString: String = s"${history.reverse.mkString(" ")} --> ${endState}"
    end Path

    def pathsFrom(paths: List[Path], explored: Set[State]): LazyList[List[Path]] =
        val frontier =
            for 
                path <- paths
                move <- allMoves
                extendedPath = path.advanceFrom(move)
                if !explored.contains(extendedPath.endState)
            yield extendedPath

        paths #:: pathsFrom(frontier, explored ++ frontier.map(_.endState))

    val beginningState: State = fullConstraints.map(_ => 0)
    val start: Path = Path(Nil, beginningState)
        
    def solutions(target: Int): LazyList[Path] =
        for 
            paths <- pathsFrom(List(start), Set(beginningState))
            path <- paths
            if path.endState.contains(target)
        yield path

    end solutions

val problem = PouringSim(Vector(4, 7))

val solutions = problem.solutions(6)

val first = solutions.head.toString




            
