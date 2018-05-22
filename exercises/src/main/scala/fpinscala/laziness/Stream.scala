package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

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

  def take(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(a, as), 0) => None
    case (Cons(a, as), x) => Some((a(), (as(), x - 1)))
    case _ => None
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case a => a
  }

  def takeWhile(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(a, as) if p(a()) => Some(a(), as())
    case _ => None
  }

  def zipWith[B](s2: Stream[B]): Stream[(A, B)] = unfold((this, s2)){
    case (Cons(a, as), Cons(b, bs)) => Some(((a(), b()), (as(), bs())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(a, as), Cons(b, bs)) => Some(((Some(a()), Some(b())), (as(), bs())))
    case (Cons(a, as), Empty) => Some(((Some(a()), None), (as(), Empty)))
    case (Empty, Cons(b, bs)) => Some(((None, Some(b())), (Empty, bs())))
    case _ => None
  }

  def hasSequence[AA >: A](s2: Stream[AA]): Boolean = ???

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

  def map[B](f: A => B): Stream[B] = unfold(this){
    case Cons(a, as) => Some((f(a()), as()))
    case _ => None
  }

  def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[AA >: A](st: => Stream[AA]): Stream[AA] = foldRight(st)((a, b) => cons(a, b))

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(_._2.nonEmpty).forAll{
    case (a, b) => a.nonEmpty && a == b
  }

  def tails: Stream[Stream[A]] = unfold(this){
    case Empty => None
    case s => Some((s, s drop 1))
  } append Stream()

  def scanRight[B](z: B)(op: (A, =>B) => B): Stream[B] =
    foldRight(Stream(z)){
      (a, b) => cons(op(a, b.take(1).toList.head), b)
    }

//    this match {
//    case Cons(_, t) => cons(this, t().tails)
//    case Empty => Empty
//  }

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

  val ones: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def constant[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def from(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def fib: Stream[Int] = unfold((0, 1)){case (a, b) => Some(a + b, (b, a + b))}

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

}

object Test extends App {
  val s1 = Stream(3, 4, 5)
  val s2 = Stream(3, 4, 5, 5)
  val xx = s1.append(s2).filter(_ % 2 == 1).toList
  val x2 = fib.take(10).toList
  val x3 = from(3).take(3).toList
  println(s1.scanRight(2)(_ * _).take(2).toList)
}