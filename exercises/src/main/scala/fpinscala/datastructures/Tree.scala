package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size(t: Tree[_]): Int =
    t match {
      case _: Leaf[_] => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def max(t: Tree[Int]): Int =
    t match {
      case Leaf(i) => i
      case Branch(l, r) => max(l) max max(r)
    }

  def depth(t: Tree[_]): Int =
    t match {
      case _: Leaf[_] => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  def fold[A, B](t: Tree[A])(fLeaf: A => B)(fBranch: (B, B) => B): B =
    t match {
      case Leaf(a) => fLeaf(a)
      case Branch(l, r) => fBranch(fold(l)(fLeaf)(fBranch), fold(r)(fLeaf)(fBranch))
    }

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(a => Leaf(f(a)))((l, r) => Branch(l, r))
}