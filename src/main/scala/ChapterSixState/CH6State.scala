package ChapterSixState


opaque type State[+A, S] = S => (A, S)

object State:
    extension [A, S](underlying: State[A, S])
        def run(s: S) = underlying(s)

        def flatMap[B](f: A => State[B, S]): State[B, S] =
            s =>
                val (a, s1) = underlying.run(s)
                f(a)(s1)

        def map[B](f: A => B): State[B, S] =
            underlying.flatMap(a => unit[B, S](f(a)))

        

        def map2[B, C](sb: State[B, S])(f: (A, B) => C): State[C, S] =
            underlying.flatMap(a =>
                sb.map(
                    b => f(a, b)
                ))
        def simpleMap2[B, C](sb: State[B, S])(f: (A, B) => C): State[C, S] =
            s =>
                val (a, s1) = underlying.run(s)
                val (b, s2) = sb(s1)
                ((f(a, b)), s2)

        def potentialMap[B](f: A => B): State[B, S] = simpleMap2(unit(()))((a, _) => f(a))

    def apply[S, A](f: S => (A, S)): State[A, S] = f

    /**
      * this "combinator" just produces a state action that when run will pass on the incoming state as the value result of the state action
      * if I flatmap this with any other state action it will return a state action that when run will pull the state after running the first state action
      * (Basically this state action turns a state into a value when ran assuming state action is interpreted as state => (producedValue, nextState))
      * so get does state => (sameState, sameState)
      *
      * @return
      */
    def get[S]: State[S, S] =
        s =>
            (s, s)
    /**
      * this "combinator" produces a state action that always produces ((), s) no matter what the incoming argument is when ran.
      * (Basically this state action (produced by combinator allow you to arbitrarily produce a state depending on whats passed into set)) 
      * set will depending on whats pass as a argument, return a state action that has no dependance on the argument i when ran it produces the tuple
      * ((), statePassedIntoSet) 
      * 
      *
      * @param s
      * @return
      */
    def set[S](s: S): State[Unit,S] =
        _ =>
            ((), s)

    def modify[S](f: S => S): State[Unit, S] =
        for
            s <- get
            _ <- set(f(s))
        yield ()
        
    /**
      * This is the same as
      */

    def modify1[S](f: S => S): State[Unit, S] =
        get.flatMap(s => set(f(s)).map(_ => ()))

    /**
      * this is the same as
      */

    /**
      * basically modify takes in an state "updating" function (how should my state change based on current state) and gives  you back a state action
      *
      * @param f
      * @param h
      * @return
      */
    def modify2[S](f: S => S, h: State[Unit, S]): State[Unit, S] =
        s =>
            val (s1Value, s1State) = get(s)  //s1State == s1Value get just turns what state it receives  into a value
            val ((), s2) = set(f(s1Value))(s1State) // s2 = f(s1Value) // basically set would be used to give caller the ability to create a state action with some arbitrarily
            //transformed/defined state
            ((), s2)




    def unit[A, S](a: A): State[A, S] =
        s =>
            (a, s)
            
    def sequence[A, S](sas: List[State[A, S]]): State[List[A], S] =
        sas.foldRight(unit[List[A], S](Nil))(
            (sa, sxs) =>
                sa.map2(sxs)(
                    (a, as) => a :: as
                )
        )

    def traverse[A, B, S](as: List[A])(f: A => State[B, S]): State[List[B], S] =
        as.foldRight(unit[List[B], S](Nil))(
            (a, sxs) =>
                f(a).map2(sxs)(
                    (b, bs) => b :: bs
                )
        )
    

    