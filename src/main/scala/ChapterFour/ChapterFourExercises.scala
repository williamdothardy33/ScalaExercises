package ChapterFour
import scala.{Option as _, Some as _, None as _}
import scala.{Either as _, Left as _, Right as _}
import scala.collection.immutable.List as _
//Using option can be considered the equivalent of insisting the caller of a function that could possibly fail to handle or pass only that failure

/**
  * option is how we can deal with partial functions which are functions that are not defined for every input implied by the parameter types
  */

object ChapterFourExercises:
    import ChapterThree.ChapterThreeExercises._
    import ChapterFour.OptionUtils._
    enum Option[+A]:
        case Some(value: A)
        case None

        def map[B](f: A => B): Option[B] =
            this match
                case Some(value) => Some(f(value)) 
                case None => None
        
        def flatMap[B](f: A => Option[B]): Option[B] = this.map(f) getOrElse None

        def mapV2[B](f: A => B): Option[B] = this.flatMap(a => unit(f(a)))  

        def getOrElse[B >: A](default: => B): B =
            this match
                case Some(value) => value
                case None => default
        
        def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(value => Some(value)) getOrElse ob
        def filter(f: A => Boolean): Option[A] = this.flatMap(value => if f(value) then Some(value) else None)

    enum Either[+E, +A]:
        case Left(value: E)
        case Right(value: A)

        def map[B](f: A => B): Either[E, B] =
            this match
                case Left(value) => Left(value)
                case Right(value) => Right(f(value))
        /**
          * getOrElse doesn't make sense for Either because if you use a default value you will loose the info
          * in the other case which at that point you might as well use an Option instead
          */
        /*def getOrElse[AA >: A](default: => AA): AA =
            this match
                case Left(value) => default
                case Right(value) => value*/

        def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
            this match
                case Left(value) => Left(value)
                case Right(value) => f(value)
            
        /**
          * use orElse when you would like to fork into an alternative "pipeline" if the first "pipeline" fails
          */
        def orElse[AA >: A, EE >: E](b: => Either[EE, AA]): Either[EE, AA] =
            this match
                case Left(value) => b
                case Right(value) => Right(value)

        def map2[B, C, EE >: E](e2: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
            this flatMap(e1Value => e2 map(e2Value => f(e1Value, e2Value)))

        def map2V2[B, C, EE >: E](e2: Either[List[EE], B])(f: (A, B) => C): Either[List[EE], C] =
            (this, e2) match
                case (Left(e1Value), Left(e2Values)) => Left(List.Cons(e1Value, e2Values))
                case (Left(e1Value), Right(e2Value)) => Left(List(e1Value))
                case (Right(e1Value), Left(e2Values)) => Left(e2Values)
                case (Right(e1Value), Right(e2Value)) => Right(f(e1Value, e2Value))
            
            

object OptionUtils:
    import ChapterFourExercises._
    import ChapterThree.ChapterThreeExercises._
    def lift[A, B](f: A => B): Option[A] => Option[B] =
        (op: Option[A]) => op map f

    def lift2[A, B, C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] =
        (op1: Option[A], op2: Option[B]) => map2(op1, op2)(f)

    def unit[A](a: A): Option[A] = Option.Some(a)
    /**
      * This takes a function as maps whatever inputs throws an exception to None
      */

    def Try[A](expression: => A): Option[A] =
        try
            Option.Some(expression)
        catch
            case e: Exception => Option.None

    def sequence[A](as: List[Option[A]]): Option[List[A]] = List.foldRightV2(Option.Some(List.Nil: List[A]), as)((opNext, opCombined) => map2(opNext, opCombined)((x, xs) =>
        List.Cons(x, xs)))

    def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = List.foldRightV2(Option.Some(List.Nil: List[B]), as)((next, opCombined) => map2(f(next), opCombined)((x, xs) =>
        List.Cons(x, xs)))

    def sequenceV2[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(op => op)

    def map2[A, B, C](op1: Option[A], op2: Option[B])(f: (A, B) => C): Option[C] =
        op1.flatMap(op1Value => op2.map(op2Value => f(op1Value, op2Value)))

object EitherUtils:
    import ChapterFourExercises._
    import ChapterThree.ChapterThreeExercises._

    def Try[A](expression: => A): Either[Exception, A] =
        try
            Either.Right(expression)
        catch
            case e: Exception => Either.Left(e)

    

    def traverseV2[A, B, E](as: List[A])(f: A => Either[E, B]): Either[List[E], List[B]] =
        List.foldRightV2(Either.Right(List.Nil: List[B]): Either[List[E], List[B]], as)((a, eCombined) => f(a).map2V2(eCombined)((b, bs) => List.Cons(b, bs)))

    def traverse[A, B, E](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
        List.foldRightV2(Either.Right(List.Nil: List[B]): Either[E, List[B]], as)((a, eCombined) => f(a).map2(eCombined)((b, bs) => List.Cons(b, bs)))

    def sequenceV2[A, E](es: List[Either[E, A]]): Either[List[E], List[A]] = traverseV2(es)(e => e)

    def sequence[A, E](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(e => e)

    /**
      * map2 will return None immediately if op1 is None, if op1 isn't None the function passed into flatmap uses a map, which will return
        None without evaluating f if op2 is None. This function "encapsulates" the logic needed to insure that a 2 parameter function lifted to
        the the option context evaluates the optional arguments only if both are valid
      */

object TestAccumulatingTraverse:
    import ChapterFourExercises._
    import ChapterThree.ChapterThreeExercises._
    import EitherUtils._
    @main def printTestAccumulatingTraverse: Unit =
        val testFun: Int => Either[String, Int]= (a: Int) => if a % 2 == 0 then Either.Right(a) else Either.Left("the value %s is not even".format(a))
        val testList1: List[Int] = List(1,2,3,4,5)
        val testList2: List[Int] = List(2,4,6,8,10)
        val result1 = EitherUtils.traverseV2(testList1)(testFun) //expected: Left(Cons(the value 1 is not even, Cons(the value 3 is not even, Cons(the value 5 is not even, Nil))))
        val result2 = EitherUtils.traverseV2(testList2)(testFun) //expected: Right(Cons(2, Cons(4, Cons(6, Cons(8, Cons(10, Nil))))))
        println("expected: Left(Cons(the value 1 is not even, Cons(the value 3 is not even, Cons(the value 5 is not even, Nil))))")
        println("actual: %s".format(result1))

        println("expected: Right(Cons(2, Cons(4, Cons(6, Cons(8, Cons(10, Nil))))))")
        println("actual: %s".format(result2))

object TestTraverse:
    import ChapterFourExercises._
    import ChapterThree.ChapterThreeExercises._
    import EitherUtils._
    @main def printTestTraverse: Unit =
        val testList = List(2, 4, 6, 8, 10)
        val testEvenFun: Int => Either[String, Int] = x => if x % 2 == 0 then Either.Right(x + 1) else Either.Left("the value %d is not even".format(x))
        val testOddFun: Int => Either[String, Int] = x => if x % 2 == 1 then Either.Right(x + 1) else Either.Left("the value %d is not odd".format(x))
        val result1 = EitherUtils.traverse(testList)(testEvenFun) //expected: Either.Right(Cons(3, Cons(5, Cons(7, Cons(9, Cons(11, Nil))))))
        val result2 = EitherUtils.traverse(testList)(testOddFun) //expected: Either.Left("the value 2 is not odd")
        println("expected: Right(Cons(3, Cons(5, Cons(7, Cons(9, Cons(11, Nil))))))")
        println("actual: %s".format(result1))
        println("expected: Left(the value 2 is not odd)")
        println("actual: %s".format(result2))

object TestSequence:
    import ChapterFourExercises._
    import ChapterThree.ChapterThreeExercises._
    import OptionUtils._
    @main def printTestSequence: Unit =
        val testList1 = List(Option.Some(1), Option.Some(2), Option.Some(3), Option.Some(4), Option.Some(5))
        val testList2 = List(Option.Some(1), Option.Some(2), Option.Some(3), Option.None, Option.Some(5))
        val result1 = sequence(testList1) //expected: Some(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))))
        val result2 = sequence(testList2) //expected: None
        val result3 = sequenceV2(testList1) //expected: Some(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))))
        val result4 = sequenceV2(testList2) //expected: None
        
        println("expected: Some(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))))")
        println("actual: %s".format(result1))
        println("expected: None")
        println("actual: %s".format(result2))
        println("----------Testing traverse via SequenceV2----------")
        println("expected: Some(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))))")
        println("actual: %s".format(result3))
        println("expected: None")
        println("actual: %s".format(result4))

object TestOptionalAndEitherFunctions:
    import ChapterFourExercises._, ChapterThree.ChapterThreeExercises._

    @main def printTestOptionalFunctions: Unit =
        val nonEmptyList: List[Double] = List(1,2,3,4,5)
        val emptyList: List[Double] = List.Nil
            
        val mean: List[Double] => ChapterFourExercises.Option[Double] = numbers =>
            if List.isEmpty(numbers) then Option.None
            else Option.Some(List.sum(numbers) / List.length(numbers))

        val meanEmptyList = mean(emptyList)
        val emptyLength = List.length(emptyList)
        val nonEmptyLength = List.length(nonEmptyList)
        val meanNonEmptyList = mean(nonEmptyList)

        val emptyVariance = meanEmptyList.flatMap(mean => Option.Some(List.foldLeft(emptyList, 0.0)((sumxsquared, x) => sumxsquared + math.pow(x - mean, 2))/emptyLength)) getOrElse "Not Defined"

        val nonEmptyVariance = meanNonEmptyList.flatMap(mean => Option.Some(List.foldLeft(nonEmptyList, 0.0)((sumxsquared, x) => sumxsquared + math.pow(x - mean, 2))/nonEmptyLength)) getOrElse "Not Defined"

        val nonEmptyVariance1 = meanNonEmptyList.flatMap(meanOfList => mean(List.map(nonEmptyList)(number => math.pow(number - meanOfList, 2)))) getOrElse "Not Defined"
        
        println("expected: 2.0")
        println("actual: %s".format(nonEmptyVariance1))

        println("expected: Not Defined")
        println("actual: %s".format(emptyVariance))



    





