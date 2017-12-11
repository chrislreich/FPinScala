package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Apply method?
object Tree {

  // 3.25
  def size[A](t : Tree[A]) : Int = t match {
    case Leaf(x) => 1
    case Branch(x, y) => 1 + size(x) + size(y)
  }

  // 3.26
  def maximum(t : Tree[Int]) : Int = t match {
    case Leaf(x) => x
    case Branch(x, y) => maximum(x) max maximum(y)
  }

  // 3.27
  def depth[A](t : Tree[A]) : Int = t match {
    case Leaf(x) => 1
    case Branch(x, y) => (1 + depth(x)) max (1 + depth(y))
  }

  // 3.28
  def map[A, B](t : Tree[A])(f : A => B) : Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(x, y) => Branch(map(x)(f), map(y)(f))
  }

  




}
