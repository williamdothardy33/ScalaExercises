/**
    * the if condition allows us to eliminate the combinations that already contain the element so that we are
    * only yield combination + c where combination doesn't contain c for each c and all combinations
    * (you can expand this to flatMap/map calls to see that each c is applied to every combination at every recursive step)
    */
//def powerSet(cs: Set[Char]): Seq[Set[Char]] =
def combinations(elements: Set[Char], size: Int): Set[Set[Char]] =
    if size == 0 then Set(Set())
    else
        val nonzero = for
            c <- elements
            combination <- combinations(elements, size - 1)
            //if !combination.contains(c)
        yield combination + c
        nonzero + Set()            

combinations(Set('a', 'b', 'c'), 0)
combinations(Set('a', 'b', 'c'), 1)
combinations(Set('a', 'b', 'c'), 3)

