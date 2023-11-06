package ChapterTen
/**
  * SemiGroup consist of
  * 1) Some type `S`
  * 2) An associative binary operation `combine`
  */
trait SemiGroup[S]:
    extension (s: S)
        def combine(other: S): S

/**
  * Monoid consist of
  * 1) Some type `M`
  * 2) Some underlying SemiGroup SemiGroup[M]
  * 3) a `neutral` element `Monoid[M].unit` such that for some underlying SemiGroup[M] and every value m which has type M, 
  * m combine Monoid[M].unit == Monoid[M].unit combine m == m
  */

trait Monoid[M] extends SemiGroup[M]:
    def combiner(m1: M, m2: M): M
    def unit: M
                



object Monoid:
    import ChapterSevenBlocking.ChapterSevenExercises.Par
    import ChapterSevenBlocking.ChapterSevenExercises.Par.*
    def par[M](m: Monoid[M]): Monoid[Par[M]] = new Monoid[Par[M]] {
        override def combiner(pm1: Par[M], pm2: Par[M]): Par[M] = pm1.map2(pm2)(m.combiner)

        override def unit: Par[M] = Par.unit(m.unit)

        extension (pm: Par[M]) 
            override def combine(other: Par[M]): Par[M] = combiner(pm, other)        
    }

    def dual[M](m: Monoid[M]): Monoid[M] = new Monoid[M] {
        def combiner(m1: M, m2: M): M = m.combiner(m1, m2)
        def unit: M = m.unit
        extension (m: M) 
            def combine(other: M): M = combiner(other, m)
    }

    def apply[M](using m: Monoid[M]): Monoid[M] = m

/**
  * if you have a collection of a type that forms a monoid you can concatenate it with concatenate
  * if you have a collection of a type that doesn't form a monoid but you have a "computation" that can turn that type to a type that can form a monoid
  * you can use foldMap to concatenate it. I think a Monad may be an abstraction of this fact??? I think it allows you to run dependent result
  * producing sequential computations in a monoidal fashion (like with Par)
  */

    def concatenate[M: Monoid](ms: List[M]): M =
        ms.foldLeft(Monoid[M].unit)((acc, m) => acc.combine(m))

    def foldMap[A, M: Monoid](as: List[A])(f: A => M): M =
        as.foldLeft(Monoid[M].unit)((acc, a) => acc.combine(f(a)))

    def isOrdered(as: List[Int]): Boolean = foldMap(as)(x => Interval(Some(x)))(orderedInterval) match
        case Interval(_, o) => o
    

    def foldMapBalanced[A, M: Monoid](as: IndexedSeq[A])(f: A => M): M =
        val length = as.size
        if as.isEmpty then Monoid[M].unit
        else if length == 1 then f(as.head) 
        else
            val split = length / 2
            val (l, r) = as.splitAt(split)
            foldMapBalanced(l)(f) combine foldMapBalanced(r)(f)

    def parFoldMap[A, M: Monoid](as: IndexedSeq[A])(f: A => M): Par[M] =
        Par.parMap(as)(f).flatMap {
            pms =>
                foldMapBalanced(pms)(m => lazyUnit(m))(par(Monoid[M]))
        }
    
        
/**
  * the fact that the function for foldLeft/foldRight which is f: (B, A) => B or f: (A, B) => B must return a type that can form a monoid allows us to 
  * implement foldLeft, and foldRight in terms of foldMap by currying (I could have implement foldMap using recursion instead of in terms of foldLeft)
  * since endoMonoid as I defined it is leftAssociative (it will combine adjacent wise from left to right) it can be used directly to implement foldLeft but
  * not foldRight for ex. consider List[B => B](f, g, h). (f andThen g and Then h)(b) will give h(g(f(b))) 
  * (f is accumulated first) while (f compose g compose h)(b) will give
  * f(g(h(b))) (h is accumulated first) so using our endoMonoid which is leftAssociative for foldRight will break the invariant 
  * (that foldRight is rightAssociative) of foldRight (it will not behave as we expect)
  * (SideNote foldRight is "right-recursive" and foldLeft "left-recursive" *refer to tree diagram")
  * (SideNoe foldRight and Foldleft have different behavior for :: because :: doesn't "form a monoid" with it's any type that it will operate on
  * because :: is only right associative in other words e1 :: e2 :: e3 == (e1 :: (e2 :: e3))) but ((e1 :: e2) :: e3) is not even defined!
  */
    def foldRightViaFoldMap[A, B](z: B)(as: List[A])(f: (A, B) => B): B =
        foldMap(as)(f.curried)(Monoid.dual(endoMonoid[B]))(z)

    def foldLeftViaFoldMap[A, B](z: B)(as: List[A])(f: (B, A) => B): B =
        foldMap(as)(a => f(_, a))(endoMonoid[B])(z)


    given stringConcat: Monoid[String] with
        def combiner(m1: String, m2: String): String = m1 + m2
        def unit: String = ""
        extension (s: String)
            def combine(other: String): String = combiner(s, other)

    given intAddition: Monoid[Int] with
        def combiner(m1: Int, m2: Int): Int = m1 + m2
        def unit: Int = 0
        extension (i: Int)
            def combine(other: Int): Int = combiner(i, other)

    given intMultiplication: Monoid[Int] with
        def combiner(m1: Int, m2: Int): Int = m1 * m2
        def unit: Int = 1
        extension (i: Int)
            def combine(other: Int): Int = combiner(i, other)

    case class Interval(value: Option[Int] = None, ordered: Boolean = true)

    given orderedInterval: Monoid[Interval] with
        def combiner(m1: Interval, m2: Interval): Interval =
            (m1, m2) match
                case (Interval(Some(v1), o1), Interval(Some(v2), o2)) => Interval(Some(v2), o1 && v1 < v2)
                case _ => m2

        def unit: Interval = Interval()
        extension (i: Interval)
            def combine(other: Interval): Interval = combiner(i, other)

    given booleanOr: Monoid[Boolean] with
        def combiner(m1: Boolean, m2: Boolean): Boolean = m1 || m2
        def unit: Boolean = false
        extension (b: Boolean) 
            def combine(other: Boolean): Boolean = combiner(b, other)

    given booleanAnd: Monoid[Boolean] with
        def combiner(m1: Boolean, m2: Boolean): Boolean = m1 && m2
        def unit: Boolean = true
        extension (b: Boolean) 
            def combine(other: Boolean): Boolean = combiner(b, other)

    given optionMonoid[A]: Monoid[Option[A]] with
        def combiner(m1: Option[A], m2: Option[A]): Option[A] = m1 orElse m2
        def unit: Option[A] = None
        extension (o: Option[A]) 
            def combine(other: Option[A]): Option[A] = combiner(o, other)

/**
  * Note: e andThen other andThen another is the same as ((e andThen other) andThen another) which is the same as another (compose (other compose e))
  * composition is right associative and andThen is left associative
  */

    given endoMonoid[A]: Monoid[A => A] with
        def combiner(m1: A => A, m2: A => A): A => A = m1 andThen m2
        def unit: A => A = x => x
        extension (e: A => A) 
            def combine(other: A => A): A => A = combiner(e, other)

    import ChapterEight.{Gen, Prop, Tag}
    import ChapterEight.Gen.`**`

    def monoidLaws[A: Monoid](genA: Gen[A]): Prop =
        val associativity = Prop.forAll(genA ** genA ** genA) {
            case arg1 ** arg2 ** arg3 =>
                val leftAssociativity = (arg1.combine(arg2)).combine(arg3)
                val rightAssociativity = arg1.combine((arg2.combine(arg3)))
                leftAssociativity == rightAssociativity
        }.tagProp(Tag.Name("associativity law"))
        val identity = Prop.forAll(genA) {
            a =>
                (Monoid[A].unit combine a) == a && (a combine Monoid[A].unit) == a
        }.tagProp(Tag.Name("identity law"))

        associativity && identity
        

/**
  * Functor consists of
  * 1) Some time F[_] which for any type T is a `producer` of T
  * 2) 
  */
trait Functor[F[_]]:
    extension [A](f: F[A])
        def map[B](g: A => B): F[B]

trait Apply[A[_]] extends Functor[A]:
    extension [U](u: A[U])
        def ap[V](f: A[U => V]): A[V]    


object TestMonoidInstances:
    import ChapterTen.Monoid.{intAddition, stringConcat, booleanAnd}
    import ChapterEight.{Gen, Prop}
    @main def printTestMonoidInstance: Unit =
        val intGen = Gen.choose(-100, 100)
        val lengthGen = Gen.choose(0, 30)
        val stringGen = Gen.stringN(lengthGen)
        val boolGen = Gen.boolean

        Prop.runDefaults(Monoid.monoidLaws(intGen))

        Prop.runDefaults(Monoid.monoidLaws(stringGen))

        Prop.runDefaults(Monoid.monoidLaws(boolGen))

object TestFold:
    import ChapterTen.Monoid.{intAddition, stringConcat, foldLeftViaFoldMap, foldRightViaFoldMap}
    @main def printTestFold: Unit =
        val t1 = List(1,2,3)
        val t2 = List("one", "two", "three")
        val r1 = foldLeftViaFoldMap(List.empty[Int])(t1) {
            case(acc, next) => next :: acc
        }
        val r2 = foldRightViaFoldMap(List.empty[String])(t2) {
            case (next, acc) => next :: acc
        }

        println(r1)
        println()
        println(r2)

object TestIntOrdered:
    import ChapterTen.Monoid.{isOrdered, foldMap}
    @main def printIntOrdered: Unit =
        val t1 = List(8,3,5,23,3)
        val t2 = List(12, 15, 21, 38)

        val r1 = isOrdered(t1)
        val r2 = isOrdered(t2)

        println(r1)
        println()
        println(r2)