sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  //exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    //exercise 3.26
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    //exercise 3.27
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    //exercise 3.28
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    //exercise 3.29
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    //exercise 3.29
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(x => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(x => x)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(x => 0)((a,b) => 1 + (a max b))
}