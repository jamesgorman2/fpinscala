package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] =
    foldRight(Nil:List[A])(_ :: _)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Stream(h())
      case _ => empty
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n -1)
      case x => x
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] =
    this match {
      case Cons(h, _) => Some(h())
      case _ => None
    }

  def headOption2: Option[A] =
    foldRight(None:Option[A])((a,_) => Some(a))


  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a), b.map(f)))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else b)

  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)((a,b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a,b) => f(a).append(b))

  def startsWith[B>:A](s: Stream[B]): Boolean =
    zipAll(this, s)
      .takeWhile{case (_,b) => b.nonEmpty}
      .forAll{case(a,b) => a == b}

  def tails: Stream[Stream[A]] =
    unfold(this){
      case Empty => None
      case s => Some((s, s.drop(1)))
    } append Stream(empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case s => Some((s.foldRight(z)(f), s.drop(1)))
    } append Stream(z)

  def scanRight2[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z))) {
      (a, acc) => {
        lazy val bs = acc
        val b = f(a, bs._1)
        (b, cons(b, bs._2))
      }
    }._2
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
  def repeat[A](a: A): Stream[A] = cons(a, repeat(a))
  def repeat2[A](a: A): Stream[A] ={
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(prev: Int, next: Int): Stream[Int] = {
      cons(prev, go(next, next + prev))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case _ => empty
    }
  }
  def fibs2: Stream[Int] = {
    unfold((0, 1)){
      case (p, n) => Some((p , (n, p+n)))
    }
  }

  def from2(n: Int): Stream[Int] =
    unfold(n)(i => Some(i, i + 1))

  def repeat[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  def map[A,B](as: Stream[A])(f: A=>B): Stream[B] =
    unfold(as) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }
  def take[A](as: Stream[A], n: Int): Stream[A] =
    unfold((as, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), i) if i > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhile[A](as: Stream[A], p: A => Boolean): Stream[A] =
    unfold(as) {
      case Cons(h, t) if p(h) => Some((h(), t()))
      case _ => None
    }

  def zipWith[A, B, C](as: Stream[A], bs: Stream[B])(f: (A,B)=>C): Stream[C] =
    unfold((as, bs)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
      case _ => None
    }

  def zip[A, B](as: Stream[A], bs: Stream[B]): Stream[(A, B)] =
    zipWith(as, bs)((a, b) => (a, b))

  def zipAll[A, B](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((as, bs)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some(((Some(ha()), Some(hb())), (ta(), tb())))
      case (Cons(ha, ta), _) => Some(((Some(ha()), None), (ta(), empty)))
      case (_, Cons(hb, tb)) => Some(((None, Some(hb())), (empty, tb())))
      case _ => None
    }
}