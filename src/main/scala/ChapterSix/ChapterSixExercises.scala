package ChapterSix
import ChapterSixRNG._
import scala.annotation.tailrec



/**
  * nonNegativeInt takes the current state and uses that state to get a result and the next state it applies a function to that result
  * then return the result and the next state
  * 
  * double does the same thing accept there is a function that is applied to the state argument instead of invoking a method on the state argument
  */

object ChapterSixExercises:
    import ChapterSixState.*
    import ChapterSixState.State.*
    case class SimpleRNG(seed: Long) extends RNG:
      def nextInt: (Int, RNG) =
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt 
        (n, nextRNG)

      def double(rng: RNG): (Double, RNG) =
        val (nextInt, nextRNG) = nonNegativeInt(rng)
        val nextDouble = nextInt.toDouble / (Int.MaxValue.toDouble + 1)
        (nextDouble, nextRNG)

end ChapterSixExercises

object RandState:
  import ChapterSixState.*
  import ChapterSixState.State.*
  import ChapterSixExercises.*
  import ChapterSixExercises.SimpleRNG
  import ChapterSix.*

  val int: State[Int, RNG] = State(_.nextInt)
  val nonNegativeInt: State[Int, RNG] = int.map(
    i =>
      if i < 0 then -(i + 1)
      else i
  )
  val double: State[Double, RNG] = nonNegativeInt.map(
    i =>
      i / (Int.MaxValue.toDouble + 1)
  )

  val intDouble: State[(Int, Double), RNG] = int.map2(double)(
    (i, d) => (i, d)
  )

  val doubleInt: State[(Double, Int), RNG] = intDouble.map {
    case (i, d) => (d, i)
  }

  val double3: State[(Double, Double, Double), RNG] =
    double.flatMap(d1 =>
      double.flatMap(d2 =>
        double.map(d3 =>
          (d1, d2, d3)
          )))
  def ints(count: Int): State[List[Int], RNG] =
    sequence{List.fill(count)(int)}
    /**
      * 
      *
      * @param s
      * @param f
      * @param g
      * @return
        this was my first stab at abstracting away the pattern of a function that takes in a function that
        returns a value the next state (a state action function) and applying a function to the returned value.
        this pattern is abstracted away with map
      */
    /*def execF[S, A, B](s: S)(f: S => (A, S), g: A => B): (B, S) =
        val (nextVal, nextState) = f(s)
        (g(nextVal), nextState)*/

    /*def nonNegativeIntV2(rng: RNG): (Int, RNG) = execF(rng)(s => s.nextInt, {
        case p: Int if p >= 0 => p
        case q: Int if q < 0  => -(q + 1)
    })*/

    /*def doubleV2(rng: RNG): (Double, RNG) = execF(rng)(s => nonNegativeInt(s), a => (a.toDouble / Int.MaxValue.toDouble + 1))*/
    
/**
  * for intDouble we execute the state twice and combine the results of 2 executions
  */

    /*def intDouble(rng: RNG): ((Int, Double), RNG) =
        val (nextInt, nextRNG1) = rng.nextInt
        val (nextDouble, nextRNG2) = doubleV2(nextRNG1)
        ((nextInt, nextDouble), nextRNG2)*/
    //maybe this will map a (value, state) to another one
    /*def map2State[A, B, C, S](s: S)(f: S => (A, S), g: S => (B, S), h: (A, B) => C): (C, S) =
        val (v1, s1) = f(s)
        val (v2, s2) = g(s1)
        (h(v1, v2), s2)*/
    //this will become unfold state
    /*def run2State[A, B, S](as: (B, S))(f: S => (A, S), g: (A, B) => B): (B, S) =
                val (a1, s1) = as
                val (a2, s2) = f(s1)
                (g(a2, a1), s2)*/

    /*def  flip[A, B](t: (A, B)): (B, A) =
        val (a, b) = t
        (b, a)*/
    // maybe change name to map state value
    /*def mapValue[A, B, S](as: (A, S))(f: A => B): (B, S) =
                val (a, s) = as
                (f(a), s)*/
    /**
      * They key here is that nextInt, intDouble, doubleInt all take in some function of type S => (A, S)
      * and applies some function to the result of type A. in the case of intDouble, and doubleInt. although
      * I only take in one value of type S => (A, S) I use 2 different functions of type S => (A, S) and they are combined
      * by taking the type S result of one state action and using it as the input of the second state action.
      * This composition is abstracted away with flatmap
      *
      *
      * @param rng
      * @return
      */
    //def doubleInt(rng: RNG): ((Double, Int), RNG) = mapValue(intDouble(rng))(flip)
    //need to write something like unfold state
    /*def ints(count: Int)(rng: RNG): (List[Int], RNG) =
        @tailrec
        def go(left: Int, result: (List[Int], RNG)): (List[Int], RNG) =
            left match
                case i if i > 0 => go(left - 1, run2State(result)(s => s.nextInt, (a, as) => a::as))
                case i if i <= 0 => result
        go(count, (List(), rng))*/

    /*def double3(rng: RNG): ((Double, Double, Double), RNG) =
        val (d1, s1) = doubleV2(rng)
        val (d2, s2) = doubleV2(s1)
        val (d3, s3) = doubleV2(s2)
        ((d1,d2,d3), s3)*/

    /*def nonNegativeLessThan(n: Int): Rand[Int] =
    rng =>
        val (i, r) = nonNegativeInt(rng)
        val mod = i % n
        if (i - mod) + n - 1 >= 0 then (mod, r)
        else nonNegativeLessThan(n)(r)

    def nonNegativeLessThanV2(n: Int): Rand[Int] = flatMap(nonNegativeInt)(a =>
        val mod = a % n
        if (a - mod) + n - 1 >= 0 then unit(mod)
        else nonNegativeLessThanV2(n))*/

    
/**
  * This represents a function or program that depends of some state RNG. it uses RNG to generate a value of type A and then it transitions
  * RNG to the next state. You can also view it as a data type like String and any value you assign to a reference with that type is an instance
  * of that type (is this a functor? is it a type constructor? can I "bolt-on" map, flatmap etc i.e make it a type instance of functor, monoid, monad etc)
  * not map unit and map2 are all functions that return functions
  */

    //type Rand[+A] = RNG => (A, RNG)


/**
  * 
  *
  * @param a
  * @return
    unit does not trigger a state transition it just passes on whatever value you give it returning a function  in the form of a state
    transition.
    if does not trigger a state transition even for the Rand that you get back if you send it a state (it just passes it along)
    unit is a neutral function for a state action in that if I compose a state action with unit it leaves the state action
    not triggered and the result unmodified (unchanged) i.e flatMap(ra: S => (A, S))(a => Unit(a)) = ra
  */
    /*def unit[A](a: A): Rand[A] =
        rng => (a, rng)*/

/**
  * 
  *
  * @param sa
  * @param f
  * @return
    map does not actually trigger a state update it just tell you want you want to do with the value result of a state update. the function f 
    transforms a type A to B we get back a state action of type Rand[B] which still needs an state (rng in this case) in order for it to actually trigger
    an update. This abstracts away the pattern of apply a function to the value result of a state action
  */
    /*def map[A, B](sa: Rand[A])(f: A => B): Rand[B] =
        rng =>
            val (a, s) = sa(rng)
            (f(a), s)*/

/**
  * map2 does do anything. it just a description of how you want to compose ra, and rb and combine the result of 
  * these 2 state transitions. You still need to send in a state  in order to trigger the updates. This abstracts
  * away the patter of composing 2 state actions and combining the result with f.
  */
        
    /*def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        rng =>
            val (a, s1) = ra(rng)
            val (b, s2) = rb(s1)
            (f(a, b), s2)*/

    /*def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
        map2(ra, rb)((_, _))*/
    /**
      * if un - unit(List()) then (last_ra, un) => map2(last_ra, un)((last_a, Nil) => last_a :: Nil) gives rng => (List(last_a), rng)
      * so it just put the last_ra in a list but does trigger state update so its really like a map 
      *
      * @param fs
      * @return
      */
    /*def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit[List[A]](List()))((ra, ras) => map2(ra, ras)((a, as) => a :: as))

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng =>
        val (a, r1) = f(rng)
        g(a)(r1)

    def mapV2[A, B](ra: Rand[A])(f: A => B): Rand[B] = flatMap(ra)(a => unit(f(a)))

    def map2V2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))


    val int: Rand[Int] = _.nextInt

    def intsV1(count: Int): Rand[List[Int]] =
        sequence(List.fill(count)(int))

    val double: Rand[Double] = map(int)(i => i.toDouble / (Int.MaxValue.toDouble + 1))*/




  






