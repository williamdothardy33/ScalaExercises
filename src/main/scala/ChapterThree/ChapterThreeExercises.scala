package ChapterThree

import scala.annotation.tailrec
import ChapterThree.ChapterThreeExercises.List.remove
import ChapterThree.ChapterThreeExercises.List.flatten
import ChapterThree.ChapterThreeExercises.List.pack
import ChapterThree.ChapterThreeExercises.List.encode
import ChapterThree.ChapterThreeExercises.List.append
import ChapterThree.ChapterThreeExercises.List.apply

object ChapterThreeExercises:
    enum List[+A]:
        case Nil
        case Cons(head: A, tail: List[A])

        def exists(p: A => Boolean): Boolean =
            this match 
                case Cons(head, tail) => {println("The head is being evaluated. The current head is: %s".format(head)); p(head)} || {println("The tail has been evaluated. The previously evaluated head is: %s".format(head)); tail.exists(p)}
                case _ => false

        /**
          * although existV2 uses the short circuiting of the || function the function f used in foldRight still evaluates its arguments strictly
          * so b will still be evaluated even when p(a) is true. b is the recursive portion of foldRight so in effect
          * the recursive portion needs to be evaluated until stopping criteria is met before we can even begin to evaluate the "combine" function
          *
          * @param p
          * @return
          */
        def existV2(p: A => Boolean): Boolean = List.foldRight(false)(this)((a, b) => p(a) || b)
            
    
    enum Tree[+A]:
        case Leaf(value: A)
        case Branch(left: Tree[A], right: Tree[A])
/* I think if I were to fold this tree into a list the list order would be the same as the order of the leaves left-to-right, head-to-tail
Since the combine function f is by value in it's parameters the entire left subtree down to the leafs will be traversed before evaluating the right subtree.
In this sense like the foldRight for List the left and right subtree will need to be fully traversed before the body of the combine function will be executed at
each level of the recursive tree.. It's evaluation I think turns nodes into f and leaves into g evaluated at the leaf value. f(f(f(1, 2), 3), f(f(f(4, 5), 6), 7))
so the sequence of evaluation after the traversal will be a = f(1, 2) -> b = f(a, 3) -> c = f(4, 5) ->d = f(c, 6) -> f(b, d)
*/
/**
 * One way of thinking about what to use for f combine function is to think about what you would like to replace the nodes with in order
 * to achieve the desired effect of the fold
  */
        def fold[B](g: A => B)(f: (B, B) => B): B =
            this match
                case Tree.Leaf(value) => g(value) 
                case Branch(left, right) => f(left.fold(g)(f), right.fold(g)(f))

        def size: Int =
            this match
                case Leaf(_) => 1 
                case Branch(left, right) => 1 + left.size + right.size

        def sizeViaFold: Int =
            this.fold(_ => 1)((leftResult, rightResult) =>1 + leftResult + rightResult)

        def max[B >: A](bigger: (B, B) => B): B =
            this match
                case Leaf(value) => value
                case Branch(left, right) => bigger(left.max(bigger), right.max(bigger))

        def maxViaFold[B >: A](bigger: (B, B) => B): B =
            this.fold(leafValue => leafValue)(bigger)

        def maxDepth: Int =
            this match
                case Leaf(value) => 0
                case Branch(left, right) => (1 + left.maxDepth) max (1 + right.maxDepth)

        def maxDepthViaFold: Int = this.fold(leafValue => 0)((leftResult, rightResult) => (1 + leftResult) max (1 + rightResult))

        def map[B](f: A => B): Tree[B] =
            this match
                case Leaf(value) => Leaf(f(value))
                case Branch(left, right) => Branch(left.map(f), right.map(f))

        /**
          * I called my anon function (subTree1, subTree2) because my replacement for my Leaf values is another leaf value and my f function only tells how to combine those transformed values
          */
        def mapViaFold[B](g: A => B): Tree[B] = this.fold(leafValue => Leaf(g(leafValue)))((subTree1, subTree2) => Branch(subTree1, subTree2))
            
            


    /*object Tree:
        def testTree: Tree[Int] =
            val testLeaf1 = Tree.Leaf(1)
            val testLeaf2 = Tree.Leaf(2)
            val testLeaf3 = Tree.Leaf(3)
            val testLeaf4 = Tree.Leaf(4)
            val testBranch1 = Tree.Branch(testLeaf1, testLeaf2)
            val testBranch2 = Tree.Branch(testLeaf3, testLeaf4)
            val testBranchRoot = Tree.Branch(testBranch1, testBranch2)
            testBranchRoot*/
    

    extension (tree: Tree[Int])
        def maximum: Int =
            tree match
                case Tree.Leaf(value) => value
                case Tree.Branch(left, right) => left.maximum max right.maximum
            
            
    
    object List:
        //Just made this so that I could use it in Chap 5 for testing
        def head[A](as: List[A]): A =
            as match
                case Nil => throw new Exception("Empty List!")
                case Cons(head, tail) => head
            
        def isEmpty[A](as: List[A]): Boolean =
            as match
                case Nil => true
                case _ => false
            
        def apply[A](as: A*): List[A] =
            if as.isEmpty then Nil
            else Cons(as.head, apply(as.tail*))

        def tail[A](as: List[A]): List[A] =
            as match
                case Cons(head, tail) => tail
                case _ => Nil
            
        def setHead[A](a: A, as: List[A]): List[A] =
            as match
                case Nil => List(a)
                case Cons(_, tail) => Cons(a, tail)
        
        def drop[A](as: List[A], n: Int): List[A] =
            if n <= 0 then as
            else as match
                case Nil => as
                case Cons(head, tail) => drop(tail, n - 1)    

        /**
          * def drop[A](as: List[A], n: Int): List[A] =
                if n <= 0 || as == Nil then as
                else drop(tail(as), n - 1)
          */ 

        def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
            as match
                case Cons(head, tail) if f(head) => dropWhile(tail, f) 
                case _ => as

        def append[A](as: List[A], bs: List[A]): List[A] =
            as match
                case Nil => bs 
                case Cons(head, tail) => Cons(head, append(tail, bs))
        
        def init[A](as: List[A]): List[A] =
            as match
                case Cons(head, Nil) => Nil
                case Cons(head, tail) => Cons(head, init(tail))
                case _ => as

        /**
          * for the foldRight as implemented the Cons case needs to "expand" the entire list before I can using the "combine" function
          * because this version of foldRight has by value parameters only for the combine function, the foldRight recursive call of the second parameter will
          * need to fully evaluate to a value before the body of the combine function can be executed so although the body of the combine function uses
          * short-circuiting it doesn't evaluate any of the head elements in it's expansion and only starts evaluating the head elements once it's reached
          * the end of the recursive tree corresponding to the expansion of the list
          * I cannot forget about the previous context in this recursive call because the previous context has what I need to combine "before"
          * so it is not stack safe. the evaluation ends up looking something like f(a1, f(a2, f(a3, f(a4, b)))) and since this is strict in parameters
          * the inner function gets evaluated (right side of list) before the outer
          */

        def foldRight[A, B](z: B)(as: List[A])(f: (A, B) => B): B =
            as match
                case Nil => z
                case Cons(head, tail) => f(head, foldRight(z)(tail)(f))
                
        def remove[A](index: Int, as: List[A]): List[A] =
            as match
                case Cons(head, tail) if index == 0 => tail
                case Cons(head, tail) if index > 0 => Cons(head, remove(index - 1, tail))
                case _ => as

        def flatten(as: List[Any]): List[Any] =
            as match
                case Nil => Nil
                case Cons(head: List[Any], tail) => append(flatten(head), flatten(tail))
                case xs: List[Any] => xs

        def takeWhile[A](as: List[A])(p: A => Boolean): List[A] =
            as match
                case Cons(head, tail) if p(head) => Cons(head, takeWhile(tail)(p))
                case _ => Nil
        
        /*def dropWhile[A](as: List[A])(p: A => Boolean): List[A] =
            as match
                case Cons(head, tail) if p(head) => dropWhile(tail)(p)
                case _ => as*/

        def span[A](as: List[A])(p: A => Boolean): (List[A], List[A]) = (takeWhile(as)(p), dropWhile(as, p)) 
            
            
        /**
          * Cons(Cons(1, Cons(2, Nil)), Cons(Cons(3, Cons(4, Nil))), Nil) == List(List(1,2), List(3, 4))
          */
/**
  * this pack will only work if same elements are in consecutive indices of list
  */
        def pack[A](as: List[A]): List[List[A]] =
            as match
                case Nil => Nil
                case Cons(head, tail) => {
                    val (packed, unpacked) = span(tail)(next => next == head)
                    Cons(Cons(head, packed), pack(unpacked))
                }

        def encode[A](as: List[A]): List[(A, Int)] =
            mapV3(pack(as))(packed => (head(packed), length(packed)))
            
            
            
            
        
        /**
          * ex. For List(1,2,3) this second case produces f(head1, f(head2, f(head3, b)))
          * if f == + we get head1 + head2 + head3 + b
          */

        /**
          * For the function in foldLeft, b can be interpreted as the result of the next "combine"
          * where the initial "combine" is f(z: B, first_element: A) == new_z
          */
        
        @tailrec
        def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
            as match
                case Nil => z
                case Cons(head, tail) => foldLeft(tail, f(z, head))(f)

        /**
          * The initial "combine" for reverse is f(Nil: List[A], first_element: A) = Cons(first_element, Nil) == new_z
          * the next "combine" is f(Cons(first_element, Nil), second_element) = Cons(second_element, Cons(first_element, Nil)) == new_z
          */
        
        def reverse[A](as: List[A]) = foldLeft(as, Nil: List[A])((as, a) => Cons(a, as))
                
            
            

        def reverseV2[A](as: List[A]) = foldLeftV2(as, Nil: List[A])((as, a) => Cons(a, as)) //to test foldLeftV2
/**
  * reverse list so that first is last then run combine function with foldRight so that we elements are combined starting from last (which is first of original)
  * reverse is actually in terms of foldLeft so technically a "circular implementation"
  */
        def reverseV3[A](as: List[A]): List[A] =
            def go(result: List[A], left: List[A]): List[A] =
                left match
                    case Nil => result
                    case Cons(head, tail) => go(Cons(head, result), tail)
            go(Nil, as)
                

        def foldLeftV2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
            foldRight(z)(reverse(as))((a: A, b: B) => f(b, a))

        
        /**
          * For the function in foldRightV2, b can be interpreted as the result of the previous "combine"
          * where the initial "combine" is f(last_element: A, z: B)
          */

        def foldRightV2[A, B](z: B, as: List[A])(f: (A, B) => B): B =
            foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))

        /**
          * The initial "combine" is f(last_a_element: A, bs: List[A]) == Cons(last_a_element, bs)
          * the previous "combine" is f(second_to_last_a_element: A, Cons(last_a_element, bs)) = Cons(second_to_last_a_element, Cons(last_a_element, bs))
          */

        def appendV2[A](as: List[A], bs: List[A]): List[A] =
            foldRightV2(bs, as)((a, b) => Cons(a, b))

        /**
          * The initial "combine" is append(last_list_in_asAS: List[A], Nil: List[A])
          * the previous "combine" is append(second_to_last_list_in_asAS: List[A], append(last_list_in_asAS: List[A], Nil: List[A]))
          */

        def concat[A](asAS: List[List[A]]): List[A] =
            foldRightV2(Nil: List[A], asAS)(append)

        def sum(as: List[Int]): Int =
            foldLeft(as, 0)(_ + _)

        def sum(as: List[Double]): Double =
            foldLeft(as, 0.0)(_ + _)

        def product(as: List[Double]): Double =
            foldLeft(as, 1.0)(_ * _)

        /**
          * The initial "combine"  is f(0, first_element) = 0 + 1 = new_z
          * The next "combine" is f(1, second_element) = 1 + 1 =  new_z
          */

        def length[A](as: List[A]): Int =
            foldLeft(as, 0)((b, a) => b + 1)

        def map[A, B](as: List[A])(f: A => B): List[B] =
            as match
                case Nil => Nil: List[B]
                case List.Cons(head, tail) => Cons(f(head), map(tail)(f))


        def mapV2[A, B](as: List[A])(f: A => B): List[B] =
            foldRightV2(Nil: List[B], as)((a, b) => Cons(f(a), b))

        def mapV3[A, B](as: List[A])(f: A => B): List[B] =
            val buf = scala.collection.mutable.ListBuffer[B]()
            def go(xs: List[A]): List[B] =
                xs match
                    case Nil => List(buf.toList*)
                    case Cons(head, tail) => {
                        buf += f(head)
                        go(tail)
                    }
            go(as)

        def mapFun[A, B](as: List[A])(f: A => B): List[B] = foldRightV2[A, List[B]](List.Nil, as)((a, acc) => Cons(f(a), acc))
        def lengthFun[A](as: List[A]): Int = foldRightV2(0, as)((a, acc) => acc + 1)

        def filter[A](as: List[A])(f: A => Boolean): List[A] =
            val buf = scala.collection.mutable.ListBuffer[A]()
            def go(xs: List[A]): List[A] =
                xs match
                    case Nil => List(buf.toList*)
                    case Cons(head, tail) if f(head) => {
                        buf += head
                        go(tail)
                    }
                    case Cons(_, tail) => go(tail)
            go(as)
        
        def filter_2[A](l: List[A], f: A => Boolean): List[A] =
            val buf = new collection.mutable.ListBuffer[A]
            def go(l: List[A]): Unit = l match
                case Nil => ()
                case Cons(h,t) => if f(h) then buf += h; go(t)
            go(l)
            List(buf.toList*)

        def filterV2[A](as: List[A])(f: A => Boolean): List[A] =
            foldRightV2(Nil: List[A], as)((a, b) => {
                if f(a) then Cons(a, b)
                else b
            })
        
        def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
            as match
                case Nil => Nil 
                case Cons(head, tail) => append(f(head), flatMap(tail)(f))

        def flatMapV2[A, B](as: List[A])(f: A => List[B]): List[B] =
            foldRightV2(Nil: List[B], as)((a, bs) => append(f(a), bs))

        def flatMapV3[A, B](as: List[A])(f: A => List[B]): List[B] =
            concat(map(as)(f))

        def filterV3[A](as: List[A])(f: A => Boolean): List[A] =
            flatMapV2(as)(a => if f(a) then Cons(a, Nil: List[A]) else Nil: List[A])

        def addLists(as: List[Int], bs: List[Int]): List[Int] = 
            (as, bs) match
                case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
                case _ => Nil: List[Int]

        def addListsV2(as: List[Int], bs: List[Int]): List[Int] = 
            def loop(xs: List[Int], ys: List[Int], zs: List[Int]): List[Int] =
                (xs, ys) match
                    case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(h1 + h2, zs))
                    case _ => zs
            reverse(loop(as, bs, Nil: List[Int]))

        def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
            def loop(xs: List[A], ys: List[B], zs: List[C]): List[C] =
                (xs, ys) match
                    case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), zs))
                    case _ => zs
            reverse(loop(as, bs, Nil: List[C]))
        // lists in scala are right associative. :: is the same thing as Cons and is a method attached to list. x :: xs is actually xs.::(x)
        // and in tern calls the constructor ::(x, xs) x2::x1::xs is actually (x2::(x1::xs)) == x2::(xs.::(x1)) == (xs.::(x1)).::(x2) == ::(x2, ::(x1, xs))
        def take[A](as: List[A], n: Int): List[A] =
            @tailrec
            def go(xs: List[A], result: List[A], count: Int): List[A] =
                (count, xs) match
                    case (_, Nil) => result
                    case (l, _) if l <= 0 => result
                    case (_, Cons(h, t)) => go(t, Cons(h, result), count - 1)
            reverse(go(as, Nil: List[A], n))

                

        def scanRight[A, B](z: B, as: List[A])(f: (A, B) => B): List[B] =
            val maxIndex = length(as)
            @tailrec
            def go(currentMaxIndex: Int, result: List[B]): List[B] =
                currentMaxIndex match
                    case i if (i > maxIndex) => result
                    case _ => go(currentMaxIndex + 1, Cons(foldRightV2(z, take(as, currentMaxIndex))(f), result))
            reverse(go(1, Nil: List[B]))

        def isContainedIn[A](as: List[List[A]], b: List[A]): Boolean =
            @tailrec
            def go(xs: List[List[A]], result: Boolean): Boolean =
                (xs, result) match
                    case (Nil, _) => result
                    case (Cons(h, t), r) => r || go(t, h == b)
            go(as, false)
        /**
          * first stab at has subsequence which is horrible lol
          * 
        def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = isContainedIn(elementNSubsequences(sup), sub) || hasSubsequence(tail(sup), sub)
          */
        def elementNSubsequences[A](as: List[A]): List[List[A]] = scanRight(Nil: List[A], as)((a, b) => List.Cons(a, b))

        def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = //def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = isContainedIn(elementNSubsequences(sup), sub) || hasSubsequence(tail(sup), sub)
            var subsequences = elementNSubsequences(sup)
            if sup == Nil then false
            else if isContainedIn(subsequences, sub) then true
            else hasSubsequence(tail(sup), sub)

        def startsWith[A](bs: List[A], as: List[A]): Boolean =
            (bs, as) match
                case (Cons(h1, t1), Cons(h2, t2)) => h1 == h2 && startsWith(t1, t2)
                case (Nil, Cons(h2, t2)) => true //since the second and third case adds to all possibilities for as (the second entry in the tuple) these 2 cases can be combine to _ and be ignored
                case (Nil,  Nil) => true 
                case _ => false
            
        def hasSubsequenceV2[A](sup: List[A], sub: List[A]): Boolean = startsWith(sub, sup) || hasSubsequence(tail(sup), sub)
            
        //def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
object TestPack:
    import ChapterThreeExercises._
    @main def printTestPack: Unit =
        val testList = List("a", "a", "a", "b", "c", "c","a")
        val result = pack(testList)
        val expected = List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))
        val runLengthEncoded = encode(testList)
        println(result)
        println(expected)
        println(runLengthEncoded)

object TestFlatten:
    import ChapterThreeExercises._
    @main def printTestFlatten: Unit =
        val testList = List(List(1,1), 2, List(3, List(5, 8)))
        val result = flatten(testList)
        println(result)

object TestRemove:
    import ChapterThreeExercises._
    @main def printTestRemove: Unit =
        val testList = List("a", "b", "c", "d", "e")
        val result = remove(1, testList)
        println(result)
object TestMapTree:
    import ChapterThreeExercises._
    @main def printTestMapTree: Unit =
        val testLeaf1 = Tree.Leaf(1)
        val testLeaf2 = Tree.Leaf(2)
        val testLeaf3 = Tree.Leaf(3)
        val testLeaf4 = Tree.Leaf(4)
        val testBranch1 = Tree.Branch(testLeaf1, testLeaf2)
        val testBranch2 = Tree.Branch(testLeaf3, testLeaf4)
        val testBranchRoot = Tree.Branch(testBranch1, testBranch2)
        val formatRule = (x: Int) => x match
            case 1 => "1st"
            case 2 => "2nd"
            case 3 => "3rd"
            case 4 => "4th"
            case _ => "%dth".format(x)
        
        val result = testBranchRoot.map( x => "%s node".format(formatRule(x))) //expected: Branch(Branch(Left("1st node"), Leaf("2nd node")), Branch(Leaf("3rd node"), Leaf("4th node")))
        val result1 = testBranchRoot.mapViaFold(x => "%s node".format(formatRule(x)))
        println("expected: Branch(Branch(Leaf(1st node), Leaf(2nd node)), Branch(Leaf(3rd node), Leaf(4th node)))")
        println("actual: %s".format(result))
        print("----------Test mapViaFold----------")
        println("expected: Branch(Branch(Leaf(1st node), Leaf(2nd node)), Branch(Leaf(3rd node), Leaf(4th node)))")
        println("actual: %s".format(result1))

object TestMaxDepth:
    import ChapterThreeExercises._
    @main def printTestMaxDepth: Unit =
        val testLeaf1 = Tree.Leaf(1)
        val testLeaf2 = Tree.Leaf(2)
        val testLeaf3 = Tree.Leaf(3)
        val testLeaf4 = Tree.Leaf(4)
        val testLeaf5 = Tree.Leaf(5)
        val testBranch2 = Tree.Branch(testLeaf3, testLeaf4)
        val testBranch1 = Tree.Branch(testLeaf1, testBranch2)
        val testBranchRoot = Tree.Branch(testBranch1, testLeaf5)
        val result = testBranchRoot.maxDepth //expected: 3
        val result1 = testBranchRoot.maxDepthViaFold
        println("expected: 3")
        println("actual: %d".format(result))
        print("----------Test maxDepthViaFold----------")
        println("expected: 3")
        println("actual: %d".format(result1))

object TestTreeMax:
    import ChapterThreeExercises._
    @main def printTestTreeMax: Unit =
        val testLeaf1 = Tree.Leaf(1)
        val testLeaf2 = Tree.Leaf(2)
        val testLeaf3 = Tree.Leaf(3)
        val testLeaf4 = Tree.Leaf(24)
        val testBranch1 = Tree.Branch(testLeaf1, testLeaf2)
        val testBranch2 = Tree.Branch(testLeaf3, testLeaf4)
        val testBranchRoot = Tree.Branch(testBranch1, testBranch2)
        val result = testBranchRoot.max((x, y) => if x > y then x else y) //expected: 4
        val result1 = testBranchRoot.maxViaFold((x, y) => if x > y then x else y) //expected: 4
        println("expected: 4")
        println("actual: %d".format(result))
        print("---------Test maxViaFold----------")
        println("expected: 4")
        println("actual: %d".format(result1))


object TestTreeSize:
    import ChapterThreeExercises._
    @main def printTestTreeSize: Unit =
        val testLeaf1 = Tree.Leaf(1)
        val testLeaf2 = Tree.Leaf(2)
        val testLeaf3 = Tree.Leaf(3)
        val testLeaf4 = Tree.Leaf(4)
        val testBranch1 = Tree.Branch(testLeaf1, testLeaf2)
        val testBranch2 = Tree.Branch(testLeaf3, testLeaf4)
        val testBranchRoot = Tree.Branch(testBranch1, testBranch2)
        val result = testBranchRoot.size //expected: 7
        val result1 = testBranchRoot.sizeViaFold //expected: 7
        println("expected: 7")
        println("actual: %d".format(result))
        print("----------Test size via fold----------")
        println("expected: 7")
        println("actual: %d".format(result1))

object TestStartsWith:
    import ChapterThreeExercises._
    @main def printTestStartsWith: Unit =
        val testSupList = List(1,2,3,4)
        val testSubList1 = List(1,2,3,4)
        val testSubList2 = List(1,2,3)
        val testSubList3 = List(1,2,3,4,5)
        val testSubList4 = List(1,2,4) 
        val result1 = List.startsWith(testSubList1, testSupList) //expected: true
        val result2 = List.startsWith(testSubList2, testSupList) //expected: true
        val result3 = List.startsWith(testSubList3, testSupList) //expected: false
        val result4 = List.startsWith(testSubList4, testSupList) //expected: false
        println("expected: true, true, false, false")
        println("actual: %b, %b, %b, %b".format(result1, result2, result3, result4))

object TesthasSubsequenceV2:
    import ChapterThreeExercises._
    @main def printTestHasSubsequenceV2: Unit =
        val testSup = List(1,2,3,4)
        val testSub1 = List(2,3)
        val testSub2 = List(4,5)
        val result1 = List.hasSubsequenceV2(testSup, testSub1) //expected: true
        val result2 = List.hasSubsequenceV2(testSup, testSub2) //expected: false
        println("----------Test hasSubsequenceV2 using startsWith and both using short circuiting.----------")
        println("expected: true, false")
        println("actual %s, %s".format(result1, result2))
            
object TesthasSubsequence:
    import ChapterThreeExercises._
    @main def printTestHasSubsequence: Unit =
        val testSup = List(1,2,3,4)
        val testSub1 = List(2,3)
        val testSub2 = List(4,5)
        val result1 = List.hasSubsequence(testSup, testSub1) //expected: true
        val result2 = List.hasSubsequence(testSup, testSub2) //expected: false
        println("expected: true, false")
        println("actual %s, %s".format(result1, result2))

object TestIsContainedIn:
    import ChapterThreeExercises._
    @main def printTestIsContainedIn: Unit =
        val testList = List(List(1,2), List(3,4), List(5,6))
        val b1 = List(3,4)
        val b2 = List(4,5)
        val b3 = List(8,9)
        val result1 = List.isContainedIn(testList, b1) //expected: true
        val result2 = List.isContainedIn(testList, b2) //expected: false
        val result3 = List.isContainedIn(testList, b3) //expected: false
        println("expected: true, false, false")
        println("actual: %s, %s, %s".format(result1, result2, result3))

object TestScanRight:
    import ChapterThreeExercises._
    @main def printTestScanRight: Unit =
        val testList = List(1,2,3,4,5)
        val result = List.scanRight(0, testList)((a, b) => a + b) //expected: Cons(1, Cons(3, Cons(6, Cons(10, Cons(15, Nil)))))
        val result1 = List.scanRight(List.Nil: List[Int], testList)((a, b) => List.Cons(a, b))
        val expected = List(List(1), List(1,2), List(1,2,3), List(1,2,3,4), List(1,2,3,4,5)) 
        println("expected: Cons(1, Cons(3, Cons(6, Cons(10, Cons(15, Nil)))))")
        println("actual: %s".format(result))
        println("----------Test scanRight to build subsequences----------")
        println("expected: ".format(expected))
        println("actual: %s".format(result1))
object TestTake:
    import ChapterThreeExercises._
    @main def printTestTake: Unit =
        val testList = List(1,2,3,4,5,6,7,8)
        val result1 = List.take(testList, 2) //expected: Cons(1, Cons(2, Nil))
        val result2 = List.take(testList, 5) //expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
        val result3 = List.take(testList, 9) //expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Nil))))))))
        println("expected: Cons(1, Cons(2, Nil))")
        println("expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))")
        println("expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Nil))))))))")
        println("actual: %s".format(result1))
        println("actual: %s".format(result2))
        println("actual: %s".format(result3))

object TestAddLists:
    import ChapterThreeExercises._
    @main def printTestAddList: Unit =
        val testList1 = List(1,2,3,4)
        val testList2 = List(5,6,7,8, 9)
        val result = List.addLists(testList1, testList2) //expected: Cons(6, Cons(8, Cons(10, Cons(12, Nil))))
        val result1 = List.addListsV2(testList1, testList2) //expected: Cons(6, Cons(8, Cons(10, Cons(12, Nil))))
        val result3 = List.zipWith(testList1, testList2)((a, b) => a + b) //expected: Cons(6, Cons(8, Cons(10, Cons(12, Nil))))
        println("expected: Cons(6, Cons(8, Cons(10, Cons(12, Nil))))")
        println("actual: %s".format(result))
        print("---------addList using tail recursion by reversing list----------")
        println("expected: Cons(6, Cons(8, Cons(10, Cons(12, Nil))))")
        println("actual: %s".format(result1))
        println("---------Test addList by using zipWith----------")
        println("expected: Cons(6, Cons(8, Cons(10, Cons(12, Nil))))")
        println("actual:%s".format(result3))

        
object TestFlatMap:
    import ChapterThreeExercises._
    @main def printTestFlatMap: Unit =
        val testList = List(1,2,3)
        val result = List.flatMap(testList)((a => List(a, a))) //expected: Cons(1, Cons(1, Cons(2, Cons(2, Cons(3, Cons(3, Nil))))))
        val result1 = List.flatMapV2(testList)(a => List(a, a))
        println("expected: Cons(1, Cons(1, Cons(2, Cons(2, Cons(3, Cons(3, Nil))))))")
        println("actual %s".format(result))
        print("----------Test flatMap in terms of foldRightV2----------")
        println("expected: Cons(1, Cons(1, Cons(2, Cons(2, Cons(3, Cons(3, Nil))))))")
        println("actual %s".format(result))

object TestFilter:
    import ChapterThreeExercises._
    @main def printTestFilter: Unit =
        val testList = List(1,2,3,4,5,6,7,8)
        val result = List.filter(testList)(a => a % 2 != 0) //expected: Cons(1, Cons(3, Cons(5, Cons(7, Nil))))
        val result1 = List.filterV2(testList)( a => a % 2 != 0)
        println("expected: Cons(1, Cons(3, Cons(5, Cons(7, Nil))))")
        println("actual: %s".format(result))
        print("----------Test filter in terms of flatMapV20----------")
        println("expected: Cons(1, Cons(3, Cons(5, Cons(7, Nil))))")
        println("actual: %s".format(result1))

object TestMap:
    import ChapterThreeExercises._
    @main def printTestMap: Unit =
        val testList1 = List(Math.PI / 6, Math.PI / 4, Math.PI / 3, Math.PI / 2, 2 * Math.PI / 3, 3 * Math.PI / 4, 5 * Math.PI / 6, Math.PI)
        val result1 = List.map(testList1)(a => s"${ ((a * 180)/ Math.PI).ceil.toInt } deg")
        val result2 = List.mapV2(testList1)(a => s"${ ((a * 180)/ Math.PI).ceil.toInt } deg")

        println("expected: Cons(30 deg, Cons(45 deg, Cons(60 deg, Cons(90 deg, Cons(120 deg, Cons(135 deg, Cons(150 deg, Cons(180 deg, Nil))))))))")
        println("actual: %s".format(result1))
        println("---------Test Map that uses foldRight implemented with foldLeft")
        println("expected: Cons(30 deg, Cons(45 deg, Cons(60 deg, Cons(90 deg, Cons(120 deg, Cons(135 deg, Cons(150 deg, Cons(180 deg, Nil))))))))")
        println("actual: %s".format(result2))        
        
object TestWorkingWithList:
    import ChapterThreeExercises._
    @main def printTestWorkingWithList: Unit =
        val testList = List(1,2,3,4)
        val testList1 = List(Math.PI / 6, Math.PI / 4, Math.PI / 3, Math.PI / 2, 2 * Math.PI / 3, 3 * Math.PI / 4, 5 * Math.PI / 6, Math.PI)
        val result = List.foldRightV2(List.Nil: List[Int], testList)((a, as) => List.Cons(a + 1, as)) //expected: Cons(2, Cons(3, Cons(4, Cons(5, Nil))))
        val result1 = List.foldRightV2(List.Nil: List[String], testList1)((a, as) => List.Cons(s"${ ((a * 180)/ Math.PI).ceil.toInt } deg", as))
        //expected: Cons("30 deg", Cons("45 deg", Cons("60 deg", Cons("90 deg", Cons("120 deg", Cons("135 deg", Cons("150 deg", Cons("180 deg", Nil))))))))
        println("expected: Cons(2, Cons(3, Cons(4, Cons(5, Nil))))")
        println("actual: %s".format(result))
        println("expected: Cons(30 deg, Cons(45 deg, Cons(60 deg, Cons(90 deg, Cons(120 deg, Cons(135 deg, Cons(150 deg, Cons(180 deg, Nil))))))))")
        println("actual: %s".format(result1))





object TestConcat:
    import ChapterThreeExercises._
    @main def printTestConcat: Unit =
        val testList = List(List(1,2), List(3,4), List(5,6))
        val result = List.concat(testList) //expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))
        println("expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))")
        println("actual: %s".format(result))

object TestReverse:
    import ChapterThreeExercises._
    @main def printTestReverse: Unit =
        val testList = List(1,2,3)
        val result = List.reverse(testList) //expected: Cons(3, Cons(2, Cons(1, Nil)))
        println("expected: Cons(3, Cons(2, Cons(1, Nil)))")
        println("actual: %s".format(result))

object TestFoldLeft:
    import ChapterThreeExercises._
    @main def printTestFoldLeft: Unit =
        val testList = List(1,2,3,4,5)
        val testList1 = List(1,2,3)
        val result = List.foldLeft(testList, 5)(_+_) //expected: 20
        val result1 = List.reverseV2(testList1) //This test whether foldLeftV2 works based on if reverseV2 works //expected: Cons(3, Cons(2, Cons(1, Nil)))
        println("expected: 20")
        println("actual: %d".format(result))
        println("expected: Cons(3, Cons(2, Cons(1, Nil)))")
        println("actual: %s".format(result1))


            
object TestFoldRight:
    import ChapterThreeExercises._
    @main def printTestFoldRight: Unit =
        val testList = List(1,2,3,4,5)
        val result1 = List.foldRight(2)(testList)((x,y) => x + y) //expected: 17
        val result2 = List.foldRight(List.Nil: List[Int])(testList)((a, as) => List.Cons(a, as)) //expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
        val result3 = List.foldRight(0)(testList)((a, i) => i + 1) //expected: 5
        val result4 = List.foldRightV2(List.Nil: List[Int], testList)((a, as) => List.Cons(a, as)) // to test foldRight in terms of fold left expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
        println("expected: 17")
        println("actual: %d".format(result1))
        println("expected: expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))")
        println("actual: %s".format(result2))
        println("expected: 5")
        println("actual: %d".format(result3))
        println("expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))")
        println("actual: %s".format(result4))


            
object TestInit:
    import ChapterThreeExercises._
    @main def printTestInit: Unit =
        val testList = List(1,2,3,4,5)
        val result = List.init(testList) //expected: Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
        println("expected: Cons(1, Cons(2, Cons(3, Cons(4, Nil))))")
        println("actual: %s".format(result))

object TestAppend:
    import ChapterThreeExercises._
    @main def printTestAppend: Unit =
        val testListA = List(1,2,3,4)
        val testListB = List(5,6,7,8)
        val result = List.append(testListA, testListB) //expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Nil))))))))
        val result1 = List.appendV2(testListA, testListB) // this is to test append in terms of foldRight which is in terms of foldLeft
        println("expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Nil))))))))")
        println("actual: %s".format(result))
        println("----------append in terms of foldRight, which in turn is in terms of foldLeft----------")
        println("expected: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Nil))))))))")
        println("actual: %s".format(result1))

object TestDropWhile:
    import ChapterThreeExercises._
    @main def printTestDropWhile: Unit =
        val testList = List(1,2,3,4,5)
        val lessThanFour = (x: Int) => x < 4
        val result = List.dropWhile(testList, lessThanFour) //expected: Cons(4, Cons(5, Nil))
        println("expected: Cons(4, Cons(5, Nil))")
        println("actual: %s".format(result))
            
            
object TestDrop:
    import ChapterThreeExercises._
    @main def printTestDrop: Unit =
        val testList = List(1,2,3,4,5)
        val result1 = List.drop(testList, 1) //expected: Cons(2, Cons(3, Cons(4, Cons(5, Nil))))
        val result2 = List.drop(testList, 3) //expected: Cons(4, Cons(5, Nil))
        val result3 = List.drop(testList, 5) //expected: Nil
        val result4 = List.drop(testList, 7) //expected: Nil
        println("expected: Cons(2, Cons(3, Cons(4, Cons(5, Nil)))) | Cons(4, Cons(5, Nil)) | Nil | Nil")
        println("actual: %s | %s | %s | %s".format(result1, result2, result3, result4))

object TestSetHead:
    import ChapterThreeExercises._
    @main def printTestSetHead: Unit =
        val testList = List(1,2,3)
        val result = List.setHead(4, testList) //expected: Cons(4, Cons(2, Cons(3, Nil)))
        println("expected: Cons(4, Cons(2, Cons(3)))")
        println("actual: %s".format(result.toString()))
            
object TestListTail:
    import ChapterThreeExercises._
    @main def printTestListTail: Unit =
        val testList = List(1,2,3,4,5)
        val result = List.tail(testList) //expected: Cons(2, Cons(3, Cons(4, Cons(5, Nil))))
        println("expected: Cons(2, Cons(3, Cons(4, Cons(5, Nil))))")
        println("actual: %s".format(result.toString()))


object TestMatchList:
    import ChapterThreeExercises._
    @main def printTestMatchList: Unit =
        val testList: List[Int] = List(1,2,3,4,5)
        val result = testList match
            case List.Cons(x, List.Cons(2, List.Cons(4, _))) => x
            case List.Nil => 42
            case List.Cons(x, List.Cons(y, List.Cons(3, List.Cons(4, _)))) => x + y //expected: 3
            case List.Cons(h, List.Cons(8, _)) => h
            case _ => 101

        println("expected: 3")
        println("actual: %d".format(result))

object TestTreeFold:
    import ChapterThreeExercises._
    @main def printTestTreeFold: Unit =
        val l1 = Tree.Leaf(1)
        val l2 = Tree.Leaf(2)
        val l3 = Tree.Leaf(3)
        val l4 = Tree.Leaf(4)
        val b1 = Tree.Branch(l1, l2)
        val b2 = Tree.Branch(l3, l4)
        val root = Tree.Branch(b1, b2)
        val result = root.fold[List[Int]](i => List(i))(append)
        println(result)
        println(root.size)