package ChapterSixRNG


trait RNG:
    def nextInt: (Int, RNG)

    def nonNegativeInt(rng: RNG): (Int, RNG) =
        val (nextInt, nextRNG) = rng.nextInt
        if nextInt < 0 then (-(nextInt + 1), nextRNG)
        else (nextInt, nextRNG)
