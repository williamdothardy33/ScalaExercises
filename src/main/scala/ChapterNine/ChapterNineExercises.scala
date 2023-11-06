package ChapterNine

import scala.util.CommandLineParser.{ParseError as _}
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]]:
    def string(s: String): Parser[String]
    def char(c: Char): Parser[Char] = string(c.toString()).map(s1 => s1.charAt(0))
    def succeed[A](a: A): Parser[A] = string("").map(_ => a)
    def wrap[A](p: => Parser[A]): Parser[A]
    def regex(r: Regex): Parser[String]

    extension [A](p: Parser[A])
        def flatMap[B](f: A => Parser[B]): Parser[B]
        def **[B](other: => Parser[B]): Parser[(A, B)] = p.flatMap(a => other.map(b => (a, b)))
        def |(other: => Parser[A]): Parser[A]
        def map[B](f: A => B): Parser[B] = p.flatMap(a => succeed(f(a)))
        def slice: Parser[String]
        def numA: Parser[Int] = p.many.slice.map(_.size)
        def run(s: String): Either[ParseError, A]
        def map2[B, C](other: => Parser[B])(f: (A, B) => C): Parser[C] = (p ** other).map(f.tupled)
        def listOfN(n: Int): Parser[List[A]] =
            if n <= 0 then succeed(Nil) 
            else p.map2(p.listOfN(n - 1))(_ :: _)
        def many: Parser[List[A]] = p.map2(p.many)(_ :: _) | succeed(Nil)
        def many1: Parser[List[A]] = p.map2(p.many)(_ :: _) //I have some feeling that many just repackages result into into List container ???


    given StringToParser: Conversion[String, Parser[String]] with
        def apply(s: String): Parser[String] = string(s)

    object ParserLaws:
        import ChapterNine.{Parsers}
        import ChapterEight.{Gen, SGen, Prop, TestResult}

        def equal[A](p1: Parser[A], p2: Parser[A])(stringGen: Gen[String]): Prop =
            Prop.forAll(stringGen)(string => p1.run(string) == p2.run(string))

        def mapLaw[A](p: Parser[A])(stringGen: Gen[String]): Prop =
            equal(p.map(x => x), p)(stringGen)

        def stringParserLaw[A](stringGen: Gen[String]): Prop =
            Prop.forAll(stringGen)(genString => string(genString).run(genString) == Right(genString))

        def succeedLaw[A](a: A)(stringGen: Gen[String]): Prop =
            Prop.forAll(stringGen)(string => succeed(a).run(string) == Right(a))

        def charLaw: Prop =
            Prop.check {
                (0 to 127).map(_.toChar).foldLeft(true) {
                    (result, c) => result && char(c).run(c.toString()) == Right(c)
                }
            }
    
