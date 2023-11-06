/**
    * motivate dependency injection/context passing for large systems
    * (This is used a lot sometimes without even knowing it so important to be able
    * to speak to this if asked in an interview!)
    */

case class Person(name: String)
case class Paper(title: String, authors: List[Person], body: String)

object ConfManagement1: //this is our context! 
    type Viewers = Set[Person]

    class Conference1(ratings: (Paper, Int)*):
        private val realScore = ratings.toMap

        def papers: List[Paper] = ratings.map(_._1).toList
/**
 * Here we are passing context explicitly
    */
        def score(paper: Paper, viewers: Viewers): Int =
            if paper.authors.exists(viewers.contains) then - 100
            else realScore(paper)
/**
 * passing context explicitly again. 
 * In this case we are passing the context
 * because another function that needs it
 * is called within this function.
    */
    
        def rankings(viewers: Viewers): List[Paper] =
            papers.sortBy(score(_, viewers)).reverse
/**
 * Here is where we are managing a context change (I believe)
 * with the query function because query has a dependency
 * on Viewers which is our context and ask passes
 * in our new context p: Person in a Set
 * to our query function
    */
        def ask[T](p: Person, query: Viewers => T) =
            query(Set(p))

        def delegateTo[T](p: Person, query: Viewers => T)(viewers: Viewers): T =
            query(viewers + p)
    end Conference1
end ConfManagement1

/**
 * Here we change the context (of Viewers) so that the query
 * function will depend on who is asking. (in this case
 * our Viewers context will dictate that if the query has
 * anything to do with ranking then and if the person asking is also an
 * author then the results that include them will be excluded from the output
 * of the query (unless they are asking for the lowest ranked (so policy 
 * should really filter instead of changing score)))
    */


val Black = Person("Black")
val Smith = Person("Smith")
val Abel = Person("Abel")
val Ed = Person("Ed")
val Peters = Person("Peters")

val conf = ConfManagement1.Conference1(
    Paper(
        title = "How to grow beans",
        authors = List(Smith, Peters),
        body = "...") -> 92,
    Paper(title = "Organic gardening",
    authors = List(Abel, Peters),
    body = "...") -> 83,
    Paper(
        title = "Composting done right",
        authors = List(Black, Smith),
        body = "...") -> 99,
    Paper(
        title = "The secret life of snails",
        authors = List(Ed),
        body = "...") -> 77
)

def highRankingProlificAuthors(asking: Person): Set[Person] =
    def query(viewers: ConfManagement1.Viewers): Set[Person] =
        val highRanked =
            conf.rankings(viewers).takeWhile(conf.score(_, viewers) > 80).toSet
        for 
            p1 <- highRanked
            p2 <- highRanked
            author2 <- p2.authors
            if p1 != p2 && p1.authors.contains(author2)
        yield author2
    conf.ask(asking, query)

def testAs(person: Person) =
    highRankingProlificAuthors(asking = person)
    .map(_.name)
    .mkString(", ")

testAs(Black)
testAs(Smith)
testAs(Abel)
testAs(Ed)

highRankingProlificAuthors(Person("anon"))


/**
    * I can pass an empty set to rankings and score and can get
    * access to everything. although these functions are typed to take viewers
    * the type alias "leaks"  type (abstraction/implementation) outside of the class
    * we can fix this making making Viewers an opaque type and then the below
    * will give an error
    */
conf.papers
conf.rankings(Set())
conf.papers.map(conf.score(_, Set()))
/**
    * inf ConfManagement2 since we are using an opaque type for viewers it is an abstract type and we don't have the leak
    * we can get a "handle" on the type from the ConfManagement2 object to create a type of Viewers => T but we cannot
    * create a Viewers type directly. An the only means of updating the context is with ask/delegate functions defined
    * in the ConfManagement2 object only with some Viewers => T type, although I don't think you can use delegateTo
    * because it has a dependency on Viewers which we cannot supply outside of ConfManagement2
    */
object ConfManagement2: 
    opaque type Viewers = Set[Person]

    class Conference2(ratings: (Paper, Int)*):
        private val realScore = ratings.toMap

        def papers: List[Paper] = ratings.map(_._1).toList
        
        def score(paper: Paper, viewers: Viewers): Int =
            if paper.authors.exists(viewers.contains) then - 100
            else realScore(paper)

        def rankings(viewers: Viewers): List[Paper] =
            papers.sortBy(score(_, viewers)).reverse

        def ask[T](p: Person, query: Viewers => T) =
            query(Set(p))

        def delegateTo[T](p: Person, query: Viewers => T)(viewers: Viewers): T =
            query(viewers + p)
    end Conference2
end ConfManagement2

/**
    * if we want to avoid passing viewers every time we make a query we can make Viewers and implicit parameter
    */

object ConfManagement3: 
    opaque type Viewers = Set[Person]

    class Conference3(ratings: (Paper, Int)*):
        private val realScore = ratings.toMap

        def papers: List[Paper] = ratings.map(_._1).toList
        
        def score(paper: Paper)(using viewers: Viewers): Int =
            if paper.authors.exists(viewers.contains) then - 100
            else realScore(paper)

        def rankings(using viewers: Viewers): List[Paper] =
            papers.sortBy(p => score(p)).reverse

        def ask[T](p: Person, query: Viewers => T) =
            query(Set(p))

        def delegateTo[T](p: Person, query: Viewers => T)(using viewers: Viewers): T =
            query(viewers + p)
    end Conference3
end ConfManagement3

val conf3 = ConfManagement3.Conference3(
    Paper(
        title = "How to grow beans",
        authors = List(Smith, Peters),
        body = "...") -> 92,
    Paper(title = "Organic gardening",
    authors = List(Abel, Peters),
    body = "...") -> 83,
    Paper(
        title = "Composting done right",
        authors = List(Black, Smith),
        body = "...") -> 99,
    Paper(
        title = "The secret life of snails",
        authors = List(Ed),
        body = "...") -> 77
)

def highRankingProlificAuthors3(asking: Person): Set[Person] =
    def query(viewers: ConfManagement3.Viewers): Set[Person] =
/**
    * this lets ConfManagement 3 supply the term/value of Viewers which it will create for us
    */
        given Viewers: ConfManagement3.Viewers = viewers
        val highRanked =
            conf3.rankings.takeWhile(conf3.score(_) > 80).toSet
        for 
            p1 <- highRanked
            p2 <- highRanked
            author2 <- p2.authors
            if p1 != p2 && p1.authors.contains(author2)
        yield author2
    conf3.ask(asking, query)

object ConfManagement4: 
    opaque type Viewers = Set[Person]
    def Viewers(using vw: Viewers) = vw

    class Conference4(ratings: (Paper, Int)*):
        private val realScore = ratings.toMap

        def papers: List[Paper] = ratings.map(_._1).toList
        
        def score(paper: Paper)(using Viewers): Int =
            if paper.authors.exists(Viewers.contains) then - 100
            else realScore(paper)

        def rankings(using Viewers): List[Paper] =
            papers.sortBy(p => score(p)).reverse

        def ask[T](p: Person, query: Viewers => T) =
            query(Set(p))

        def delegateTo[T](p: Person, query: Viewers => T)(using Viewers): T =
            query(Viewers + p)
    end Conference4
end ConfManagement4

object ConfManagement5: 
    opaque type Viewers = Set[Person]
    type Query[T] = Viewers ?=> T

    def Viewers(using vw: Viewers) = vw

    class Conference5(ratings: (Paper, Int)*):
        private val realScore = ratings.toMap

        def papers: List[Paper] = ratings.map(_._1).toList
        
        def score(paper: Paper): Query[Int] =
            if paper.authors.exists(Viewers.contains) then - 100
            else realScore(paper)
/**
 * Here we are using an implicit function type as the type of rankings, Query[List[Papers]] and the body of rankings
 * produces a List[Paper] the compiler will create a function of type Viewers => List[Paper] for us as well as supplying
 * all functions in the expression (the body) that require a Viewer context. Basically we can eliminate the using clause
 * and we only need the regular parameters and if our function only uses implicit parameters we can call it like a no
 * arg function basically making call sites for the function look like we are calling a computed property
 * this is really neat and shuts off all the noise!!! (quiet syntax indeed! quiet quiet hahaha I like it when my
 * code whisperrrrsssss)
    */
        def rankings: Query[List[Paper]] =
            papers.sortBy(p => score(p)).reverse

        def ask[T](p: Person, query: Query[T]) =
            query(using Set(p))

        def delegateTo[T](p: Person, query: Query[T]): Query[T] =
            query(using (Viewers + p))
    end Conference5
end ConfManagement5

val conf5 = ConfManagement5.Conference5(
    Paper(
        title = "How to grow beans",
        authors = List(Smith, Peters),
        body = "...") -> 92,
    Paper(title = "Organic gardening",
    authors = List(Abel, Peters),
    body = "...") -> 83,
    Paper(
        title = "Composting done right",
        authors = List(Black, Smith),
        body = "...") -> 99,
    Paper(
        title = "The secret life of snails",
        authors = List(Ed),
        body = "...") -> 77
)

def highRankingProlificAuthors5(asking: Person): Set[Person] =
    def query: ConfManagement5.Query[Set[Person]] =
        val highRanked =
            conf5.rankings.takeWhile(conf5.score(_) > 80).toSet
        for 
            p1 <- highRanked
            p2 <- highRanked
            author2 <- p2.authors
            if p1 != p2 && p1.authors.contains(author2)
        yield author2
    conf5.ask(asking, query)