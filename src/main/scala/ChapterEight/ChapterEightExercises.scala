package ChapterEight
import ChapterSixState.State
import ChapterSixRNG._
import ChapterSix.RandState.{int, nonNegativeInt, double}
import ChapterSixState.State.{unit as sUnit, sequence, flatMap as sFlatMap, map2 as sMap2, map as sMap}
import ChapterFive.ChapterFiveExercises._
import ChapterFive.infiniteStreams._
import ChapterFour.ChapterFourExercises.Option
import ChapterFour.ChapterFourExercises.Option.{Some, None}
import ChapterFour.ChapterFourExercises
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import TypeClassPractice.testReader
import scala.{None as _, Some as _, Option as _}
import scala.annotation.targetName
import ChapterTwo.ChapterTwoExercises.partial2
import ChapterSix.ChapterSixExercises.SimpleRNG
import java.util.concurrent.Executors
import ChapterSevenBlocking.ChapterSevenExercises.{Par}
import ChapterSevenBlocking.ChapterSevenExercises.Par.{flatMap as pFlatMap, map as pMap, map2 as pMap2, unit as pUnit, parEqual}

/**
  * from the book, the goal is to split the number to test case you want to run between all of the sizes
  * that you would like to test for. to run a test obviously you need at a minimum to specify that you would like
  * one test case to be ran
  * formula from the book casesPerSize = ((n - 1) + max) / max = y will increase by 1 when n increases by max so if x = n + max
  * ((x - 1) + max) / max = ((n - 1) + 2max) / max = y + 1 so to get 2 cases per property the number of test cases
  * need to be specified to max + 1 and to get 3 test cases per property it needs to specified as 2 * max + 1
  * because these test cases need to be split between properties you cannot generate more than n properties
  * (otherwise you will have more properties than you can allot specified total number of test cases)
  * the program is structured so that if n <= max then you will get n properties with 1 test case each and if n >= max + 1 you will 
  * get max number of properties properties with with 2 or more test cases each. 
  * SGen is a type that maps Int to some generator I believe the cardinality of the
  * int represents the "complexity" of the test cases that it will generate but I believe its completely up to you
  * to implement this mapping and what it "represents". 
  * note for n >= max + 1 && n < 2max + 1 (The numbers may work out differently but its true in other ranges also) 
  * you will run 2 * max - n more test cases than you have specified I think so implementation isn't sound but will work
  * I think this "over-budgeting" of test cases is roughly constant so what you specify as the number of test cases and what you actually get is roughly proportional
  * and unless you have some constraint where test cases specified and actual test case must be exactly the same this is a good "engineering" solution. 
  */

opaque type SuccessCount = Int
opaque type FailedCase = String
opaque type NumberOfCases = Int
opaque type MaxComplexityIndex = Int

extension (mci: MaxComplexityIndex)
    def toInt: Int = mci

object MaxComplexityIndex:
    def apply(x: Int): MaxComplexityIndex = x

enum TestResult:
    case Passed
    case Failed(failedCase: FailedCase, successCount: SuccessCount, fromProp: Option[Tag] = None)
    case Proved

enum Tag:
    case Name(propName: String)

opaque type Prop = (MaxComplexityIndex, NumberOfCases, RNG) => TestResult        
            
object Prop:
    import ChapterEight.Gen.{randomStream}
    
    def check(f: => Boolean): Prop =
        (_, _, _) => f match
            case true => TestResult.Proved
            case _ => TestResult.Failed(().toString(), 0, None)

    def runDefaults(p: Prop,
                    maxCIndex: MaxComplexityIndex = 100,
                    numCases: NumberOfCases = 103,
                    rng: RNG = SimpleRNG(System.currentTimeMillis())): Unit =
                        p.run(maxCIndex, numCases, rng) match
                            case TestResult.Passed => println(s"OK, passed $numCases tests!")
                            case TestResult.Failed(failedCase, successCount, fromProp) =>
                                println(s"$fromProp\n failed after $successCount passed tests\n $failedCase")
                            case TestResult.Proved => println(s"Ok, proved property.")

    def apply(f: (NumberOfCases, RNG) => TestResult): Prop =
        (_: MaxComplexityIndex, n: NumberOfCases, rng: RNG) => f(n, rng)

    def buildMessage[A](testCase: A, e: Throwable): String =
        s"test case: $testCase:\n generation exception: ${e.getMessage()}\n stack trace:\n ${e.getStackTrace().mkString("\n")}"

    def forAll[A](gen: Gen[A])(p: A => Boolean): Prop =
        Prop {
            (numberOfCases, rng) =>
                gen.randomStream(rng).zip(from(1)).map {
                case (testCase, testNumber)  =>
                    try
                        if p(testCase) then TestResult.Passed else TestResult.Failed(testCase.toString(), testNumber - 1)
                    catch
                        case e: Throwable => TestResult.Failed(buildMessage(testCase, e), testNumber - 1)
                }.takeV5(numberOfCases).find {
                case testResult: TestResult.Failed => true
                case _ => false
            }.getOrElse(TestResult.Passed)
        }
    
    @targetName("sForAll")
    def forAll[A](sGen: SGen[A])(p: A => Boolean): Prop =
        (maxCIndex, numCases, rng) =>
            val casesPerIndex = (numCases - 1 + maxCIndex) / maxCIndex
            val propsFromIndexes = from(0).takeV5((numCases min maxCIndex) + 1).map {
                index => Prop.forAll(sGen(index))(p)
            }

            val indexedProps = propsFromIndexes.map {
                prop =>
                    (mcIndex: MaxComplexityIndex, _: NumberOfCases, rng: RNG) =>
                        prop.run(mcIndex, casesPerIndex, rng)
            }
            
            val combined = indexedProps.toList.reduce {
                case (prop, nextProp) => prop && nextProp
            }

            combined.run(maxCIndex, numCases, rng)

    extension (p: Prop)
        def run(mcIndex: MaxComplexityIndex, testCount: NumberOfCases, rng: RNG): TestResult = p(mcIndex, testCount, rng)

        def tagProp(withTag: Tag): Prop =
            (mcIndex, numberOfCases, rng) =>
                val result = p(mcIndex, numberOfCases, rng)
                result match
                    case TestResult.Proved => TestResult.Proved
                    case TestResult.Passed => TestResult.Passed
                    case TestResult.Failed(failedCase, successCount, fromProp) =>
                        fromProp match
                            case None => TestResult.Failed(failedCase, successCount, Some(withTag))
                            case Some(value) => result

        def &&(other: Prop): Prop =
            (mcIndex, numberOfCases, rng) =>

                p.tagProp(Tag.Name("Left Property"))(mcIndex, numberOfCases, rng) match
                    case TestResult.Passed => other.tagProp(Tag.Name("Right Property"))(mcIndex, numberOfCases, rng)
                    case failed => failed

        def ||(other: Prop): Prop =
            (mcIndex, numberOfCases, rng) =>
                p.tagProp(Tag.Name("Left Property"))(mcIndex, numberOfCases, rng) match
                    case TestResult.Proved => TestResult.Proved
                    case TestResult.Passed => TestResult.Passed
                    case TestResult.Failed(failedCase, successCount, _) => other.tagProp(Tag.Name("Right Property"))(mcIndex, numberOfCases, rng)
end Prop

opaque type Gen[+A] = State[A, RNG]

object Gen:
    def choose(start: Int, stopExclusive: Int): Gen[Int] = nonNegativeInt.map {
        case n: Int =>
            start + n % (stopExclusive - start)
    }

    def unit[A](a: => A): Gen[A] = sUnit(a)

    def boolean: Gen[Boolean] = int.map {
        case i => i % 2 == 0
    }

    def char: Gen[Char] = choose(0, 127).map(_.toChar)

    def string(length: Int): Gen[String] = char.listOfN(length).map(_.mkString)

    def stringN(genLength: Gen[Int]): Gen[String] = genLength.flatMap(length => string(length))

    object `**`:
        def unapply[A, B](out: (A, B)) = scala.Some(out)

    extension [A](gen: Gen[A])
        def toSGen: SGen[A] =
            (size: Int) => gen

        def listOf: SGen[List[A]] =
            (complexityIndex: Int) =>
                gen.listOfN(complexityIndex)
            
        def nonEmptyListOf: SGen[List[A]] =
            (complexityIndex: Int) =>
                gen.listOfN(complexityIndex max 1)

        def randomStream(rng: RNG): Stream[A] =
            unfold(rng)(s => Some(gen.run(s)))

        def listOfN(n: Int): Gen[List[A]] =
            sequence(List.fill(n)(gen))

        def listOfN(size: Gen[Int]): Gen[List[A]] =
            size.flatMap(l => gen.listOfN(l))

        def union(other: Gen[A]): Gen[A] =
            Gen.boolean.sFlatMap {
                case true => gen
                case _ => other
            }
        def parOf: Gen[Par[A]] =
            gen.map(a => pUnit(a))

        def nestedParOf: SGen[Par[A]] =
            (complexityIndex: Int) =>
                List.fill(complexityIndex)((a: Par[A]) => Par.fork(a)).foldLeft(gen.parOf) {
                    case (aGen, forkOff) => aGen.map {
                        case (a) => forkOff(a)
                    }
                }

        @targetName("gMap")
        def map[B](f: A => B): Gen[B] =
            gen.sMap(f)

        def map2[B, C](other: Gen[B])(f: (A, B) => C): Gen[C] =
            gen.sMap2(other)(f)

        def **[B](other: Gen[B]): Gen[(A, B)] =
            gen.map2(other)((_, _))

        def weighted(other: Gen[A], proportion: Double): Gen[A] =
            double.flatMap {
                case threshold if threshold <= proportion => other
                case _ => gen
            }

        def weighted1(other: Gen[A], proportion: Double): Gen[A] =
            Gen.choose(1, 101).sFlatMap {
                case choice if choice <=100 * proportion => other
                case _ => gen
            }
end Gen

opaque type SGen[+A] = Int => Gen[A]

object SGen:
    extension [A](sGen: SGen[A])

        def flatMap[B](f: A => SGen[B]): SGen[B] =
            (complexityIndex: Int) =>
                sGen(complexityIndex).flatMap(a => f(a)(complexityIndex))
        @targetName("sgMap")
        def map[B](f: A => B): SGen[B] =
            (complexityIndex: Int) =>
                sGen(complexityIndex).map(f)
end SGen
    
object TestMax:
    import ChapterEight._
    import ChapterEight.{SGen, Prop, Gen}
    @main def printTestMax: Unit =
        def myMax(xs: List[Int]): Int =
            def acc(currMax: Int, left: List[Int]): Int =
                left match
                    case head :: Nil => currMax
                    case head :: next => if head > currMax then acc(head, next) else acc(currMax, next)
                    case _ => currMax
            acc(xs.head, xs)
            
        val intGen = Gen.choose(-10, 10)
        val listGen1 = intGen.nonEmptyListOf
        val listGen2 = intGen.listOfN(10)
        val prop = Prop.forAll(listGen1) {
            xs =>
                val max = myMax(xs)
                !xs.exists(entry => entry > max)
        }
        Prop.runDefaults(prop)

object TestSorted:
    import ChapterEight._
    import ChapterEight.{SGen, Prop, Gen}
    @main def printTestSorted: Unit =                    
            
        val intGen = Gen.choose(-10, 10)
        val listGen1 = intGen.nonEmptyListOf
        val prop = Prop.forAll(listGen1) {
            xs =>
                val sortedXs = xs.sorted
                sortedXs.isEmpty || sortedXs.tail.isEmpty || !sortedXs.zip(sortedXs.tail).exists {
                    case (a, b) => a > b
                } && xs.length == sortedXs.length && !xs.exists {
                    x => !sortedXs.contains(x)
                } && !sortedXs.exists {
                    sx => !xs.contains(sx)
                }
        }
        Prop.runDefaults(prop)

object TestPar:
    import ChapterEight._
    import ChapterEight.{SGen, Prop, Gen}
    import ChapterSevenBlocking.ChapterSevenExercises._
    @main def printTestPar: Unit =                    
        val intGen = Gen.choose(-10, 10)    
        val fixedThreadGen = Gen.choose(1, 4).map {
            numThreads => Executors.newFixedThreadPool(numThreads)
        }
        val cachedThreadGen = Gen.unit(Executors.newCachedThreadPool())
        
        val executionStrategyGen = fixedThreadGen.weighted(cachedThreadGen, 0.25)
        val addOne = (value: Int) => value + 1
        val equalPredicate = (num: Int) => parEqual(Par.unit(num).map(addOne), Par.unit(addOne(num)))

        def parForAll[A](testCaseGen: Gen[A])(p: A => Par[Boolean]): Prop =
            Prop.forAll(executionStrategyGen ** testCaseGen) {
                (eStrategy, aTestCase) => Par.run(p(aTestCase))(eStrategy).get()
            }

        val mapUnitProp = parForAll(intGen)(equalPredicate)
        Prop.runDefaults(mapUnitProp)

object TestFork:
    import ChapterEight._
    import ChapterEight.{SGen, Prop, Gen}
    import ChapterSevenBlocking.ChapterSevenExercises._
    @main def printTestFork: Unit =                    
        val intGen = Gen.choose(-10, 10)    
        val fixedThreadGen = Gen.choose(1, 2).map {
            numThreads => Executors.newFixedThreadPool(numThreads)
        }
        val cachedThreadGen = Gen.unit(Executors.newCachedThreadPool())
        val executionStrategyGen = fixedThreadGen.weighted(cachedThreadGen, 0.25)
        val addOne = (value: Int) => value + 1
        val equalPredicate = (num: Int) => parEqual(Par.unit(num), Par.fork(Par.unit(num)))
        def parForAll[A](testCaseGen: Gen[A])(p: A => Par[Boolean]): Prop =
            Prop.forAll(executionStrategyGen ** testCaseGen) {
                (eStrategy, aTestCase) => Par.run(p(aTestCase))(eStrategy).get()
            }
        val mapUnitProp = parForAll(intGen)(equalPredicate)
        Prop.runDefaults(mapUnitProp)


        



