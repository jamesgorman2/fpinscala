package fpinscala.datastructures

import scala.annotation.tailrec

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

  val l: Int = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  // what is l?
  // 3

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

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => sys.error("nil!")
      case Cons(_, tail) => Cons(h, tail)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else
      l match {
        case Nil => sys.error("empty")
        case Cons(_, tail) => drop(tail, n - 1)
      }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) =>
        if (f(head)) dropWhile(tail, f)
        else l
    }
  }

  def dropWhile2[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) if f(head) => dropWhile2(tail, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }

  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, s) => s + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((acc, a) => Cons(a, acc))

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((acc, a) => f(a, acc))

  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight2(l1, l2)(Cons(_, _))

  def flatten[A](ll: List[List[A]]): List[A] =
    foldRight2(ll, Nil:List[A])(append2)

  def addOne(l: List[Int]): List[Int] =
    foldRight2(l, Nil:List[Int])((i, acc) => Cons(i + 1, acc))

  def toString[A](l: List[A]): List[String] =
    foldRight2(l, Nil:List[String])((a, acc) => Cons(a.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight2(l, Nil:List[B])((a, acc) => Cons(f(a), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight2(l, Nil:List[A]) {
      (a, acc) =>
        if (f(a)) Cons(a, acc)
        else acc
    }

  def flatMap[A,B](l: List[A])(f: A => List[B]) =
    flatten(map(l)(f))
  def flatMap2[A,B](l: List[A])(f: A => List[B]) =
    foldRight2(l, Nil:List[B])((a, acc) => append(f(a), acc))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def zip[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1, h2), zip(t1, t2)(f))
    }

  // 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith(l: List[A], sub: List[A]): Boolean = {
      (l, sub) match {
        case (_, Nil) => true
        case (Cons(h1,t1), Cons(h2,t2)) if h1 == h2 => startsWith(t1, t2)
        case _ => false
      }
    }
    sup match {
      case Nil => false
      case Cons(_, t) => if(startsWith(sup, sub)) true else hasSubsequence(t, sub)
    }
  }
}
