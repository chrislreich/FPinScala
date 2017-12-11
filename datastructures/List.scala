package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // 3.1
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }


  println {
    x
  }


  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  // 3.4
  //If N is negative, error checking?
  def drop[A](l: List[A], n: Int): List[A] =  n match {
    case 0 => l
    case _ => if (l == Nil) Nil
              else drop(tail(l), n - 1)
    }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f)
                        else Cons(x, xs)

  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  // 3.9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0) ((x, y) => 1 + y)
  }

  // 3.10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // 3.11
  def sumLeft(l: List[Int]) : Int = {
    foldLeft(l, 0)((x, y) => x + y)
  }

  // 3.11
  def productLeft(l : List[Int]) : Int = {
    foldLeft(l, 1)((x,y) => x * y)
  }

  // 3.11
  def lengthLeft[A](l : List[A]) : Int = {
    foldLeft(l, 0)((x, y) => x + 1)
  }

  // 3.12
  def reverse[A](l: List[A]) : List[A] = {
    foldLeft(l, List[A]())((x, xs) => Cons(xs, x))
  }

  // 3.14
  def appendFold[A](l1 : List[A], l2 : List[A]) : List[A] = {
    foldRight(l1, l2)((x, xs) => Cons(x, xs))
  }

  // 3.15
  def listOfLists[A](l : List[List[A]]) : List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => append(x, listOfLists(xs))
  }



  // 3.16
  def addOne(l : List[Int]) : List[Int] = {
    foldRight(l, List[Int]()) ((x, xs) => Cons(x + 1, xs))
  }

  // 3.17
  def doubleToString(l : List[Double]) : List[String] = {
    foldRight(l, List[String]())((x, xs) => Cons(x.toString, xs))
  }

  // 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, List[B]())((x, xs) => Cons(f(x), xs))
  }

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean) : List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f))
                        else filter(xs)(f)
  }

  // 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]) : List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  // 3.21
  def filterWithFlatMap[A](l : List[A])(f : A => Boolean) : List[A] = {
    flatMap(l)(x => if (f(x)) List(x) else List())
  }

  // 3.22
  //Error checking
  def addLists(l1 : List[Int], l2 : List[Int]) : List[Int] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Nil, Cons(y, ys)) => Cons(y, addLists(Nil, ys))
    case (Cons(x, xs), Nil) => Cons(x, addLists(xs, Nil))
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addLists(xs, ys))

  }


  // 3.23
  def zipWith[A](l1 : List[A], l2 : List[A])(f : (A, A) => A) : List[A] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Nil, Cons(y, ys)) => Cons(y, zipWith(Nil, ys)(f))
    case (Cons(x, xs), Nil) => Cons(x, zipWith(xs, Nil)(f))
    case (Cons(x,xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
  }


  // 3.24
  def hasSubsequence[A](sub : List[A], sup : List[A]) : Boolean = {
    if (length(sup) < length(sub)) false

    def checkSub[A](s1 : List[A], s2 : List[A]) : Boolean = (s1, s2) match {
      case (Nil, _ ) => true
      case(Cons(x, xs), Nil) => false
      case(Cons(x, xs), Cons(y, ys)) => if(x == y) checkSub(xs, ys)
                                        else false
    }

    def moveThroughList[A](l1 : List[A], l2 : List[A]) : Boolean = l2 match {
      case Nil => false
      case Cons(x, xs) => if (checkSub(l1, l2)) true
                          else (moveThroughList(l1, xs))
    }

    moveThroughList(sub, sup)
  }

  def foldRightviaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {

    foldLeft(reverse(as), z)((b,a) => f(a,b))
  }






}
