package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

   // 5.1
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(x, y) => x() :: y().toList
  }




  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // 5.2
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, y) =>  if (n == 0) Empty
                       else cons(x(), y().take(n - 1));

  }




  // 5.2
  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, y) => if (n == 1) y()
                       else y().drop(n - 1)
  }


  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, y) => if (p(x())) cons(x(), y().takeWhile(p))
                       else Empty
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(x, y) => if (p(x())) y().forAll(p)
                       else false
  }

  def forAllFoldRight(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  // 5.5
  def takeWhileFoldRight(p: A => Boolean): Stream[A] = {
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b)
                                  else empty)
  }


  // 5.6
  def headOption: Option[A] = {
    foldRight(None : Option[A])((a, b) => Some(a))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map_1[B](f: A => B) : Stream[B] = this match {
    case Cons(x, y) => cons(f(x()), y().map_1(f))
    case _ => empty

  }

  def map_2[B](f: A => B) : Stream[B] = {
    foldRight(empty[B])((a,b) => cons(f(a), b))
  }

  def filter(f: A => Boolean) : Stream[A] = {
    foldRight(empty[A])((a,b) => if (f(a)) cons(a, b)
                                  else b)
  }

  //Covariant issues
  // covariant type A occurs in contravariant position in type => fpinscala.laziness.Stream[A] of value seq
  def append[B>:A](seq: => Stream[B]) : Stream[B] = {
    foldRight(seq)((a, b) => cons(a, b))
  }


  def flatMap[B](f: A => Stream[B]) : Stream[B] = {
    foldRight(empty[B])((a, b) => f(a).append(b))
  }

  // 5.13
  def mapUnfold[B](f: A => B) : Stream[B] = {
    unfold(this){case Cons(x, y) => Some((f(x()), y()))
                 case Empty => None}
  }

  def takeUnfold(n : Int) : Stream[A] = {
    unfold((this, n)){case (Cons(x, y), n) => if (n == 0) None
                                          else Some((x(),(y(), n -1)))

                      case (_, _) => None}
  }

  def takeWhileUnfold(p: A => Boolean) : Stream[A] = {
    unfold(this){case Cons(x, y) => if (p(x())) Some((x(), y()))
                                    else None
                 case Empty => None}
  }

  def zipWithUnfold[B, C](seq: Stream[B])(f: (A, B) => C) : Stream[C] = {
    unfold((this, seq)){case (Cons(x, xs), Cons(y, ys)) => Some((f(x(), y()), (xs(), ys())))
                        case _ => None}
  }

  def zipAllUnfold[B](seq: Stream[B]): Stream[(Option[A] , Option[B])] = {
    zipWithAll(seq)((_, _))
  }

  def zipWithAll[B, C](s: Stream[B])(f: (Option[A],Option[B]) => C) : Stream[C] = {
    unfold((this, s)){case (Cons(x, xs), Cons(y, ys)) => Some((f(Some(x()), Some(y())), (xs(), ys())))
                      case (Empty, Cons(y, ys)) => Some((f(None, Some(y())), (empty[A], ys())))
                      case (Cons(x, xs), Empty) => Some((f(Some(x()), None), (xs(), empty[B])))
                      case (_, _) => None}
  }


  // 5.14
  def startsWith[B](s: Stream[B]): Boolean = {
    zipAllUnfold(s).takeWhile{ case (x, y) => y != None}.forAll{ case (a, b) => a ==b}
  }



  // 5.15
  def tails: Stream[Stream[A]] = {
    unfold(this){ case Cons(x, xs) => Some(cons(x(), xs()), xs())
                  case Empty => None}.append(Stream())
  }


  // 5.16
  def scanRight[B](z : B)(f: (A, => B) => B) : Stream[B] = {
    foldRight((Stream(z), z))((x, y) => {
                              lazy val y_val = y
                              val new_val = f(x, y._2)
                              (cons(new_val, y._1), new_val)})._1
  }



}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)


  def streamOfStreamsToList[A](s: Stream[Stream[A]]) : List[List[A]] = s match {
    case Empty => Nil
    case Cons(x, y) => x().toList :: streamOfStreamsToList(y())

  }

  def secondUnfold[A, S](z : S)(f: S => Option[(A, S)]) : Stream[A] = f(z) match {
    case None => empty[A]
    case Some((x, y)) => cons(x, unfold(y)(f))
  }




  def sieve = {
    from(2).filter(x =>  from(2).take(x -2).forAll(y => x % y != 0))
  }

  // 5.8
  def constant[A](a: A) : Stream[A] = Stream.cons(a, constant(a))

  // 5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  // 5.10
  def fibs = {
    def helper(n1 : Int, n2 : Int) : Stream[Int] = {
      cons(n1, helper(n2, n1 + n2))
    }
    helper(0, 1)
  }


  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty[A]
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }

  // 5.12

  def fibsUnfold = {
    unfold((0,1)){ case(x, y) => Some(x, (y, x + y))}
  }

  def fromUnfold(n : Int) = {
    unfold(n)(x => Some(x, x + 1))
  }

  def constantUnfold(n : Int) = {
    unfold(n)(x => Some(x, x))
  }

  def onesUnfold = {
    unfold(1)(x => Some(x, x))
  }

}
