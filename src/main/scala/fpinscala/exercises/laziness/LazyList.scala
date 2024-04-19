package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.{cons, empty, unfold}

import scala.annotation.tailrec

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] =
    @tailrec
    def helper(z: List[A], t: LazyList[A]): List[A] =
      t match
        case Cons(x, xs) => helper(x() :: z, xs())
        case Empty => z.reverse
    helper(List.empty, this)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    this match
      case Cons(x, xs) if n == 1 => cons(x(), Empty)
      case Cons(x, xs) if n > 1 => cons(x(), xs().take(n - 1));
      // The unevaluated expressions "x()" and "xs().take(n - 1)" are now the head and tail of the returned lazy list!
      case _ => Empty

  @tailrec
  final def drop(n: Int): LazyList[A] =
    this match
      case Cons(x, xs) if n > 0 => xs().drop(n - 1)
      case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] =
    this match
      case Cons(x, xs) if p(x()) => cons(x(), xs().takeWhile(p))
      case _ => Empty

  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] =
    this.foldRight(empty)((x, z) => {
      if p(x) then cons(x, z) // If p(x), produce list w/ x as head and recursive expression on tail as z
      else Empty // Else, terminate by not evaluating recursive call in expression z
    })

  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((x, z) => p(x) && z)
    // Short-circuits when p(x) == false, leaving the nonstrict recursive call passed in via z unevaluated

  def headOption: Option[A] =
    this.foldRight(Option.empty)((a, z) =>
      Some(a)
    )

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): LazyList[B] =
    this.foldRight(empty[B])((x, z) => cons(f(x), z))

  def filter(f: A => Boolean): LazyList[A] =
    this.foldRight(empty[A])((x, z) => {
      if f(x) then cons(x, z)
      else z
    })

  def append[B >: A](l: => LazyList[B]): LazyList[B] =
    this.foldRight(l)((x, z) => {
      cons(x, z)
    })

  def flatMap[B >: A](f: A => LazyList[B]): LazyList[B] =
    this.foldRight(empty[B])((x, z) => f(x).append(z))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Empty => None
      case Cons(x, xs) => Some(f(x()), xs())
    }

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold(this, n)((t: LazyList[A], i: Int) => {
      t match
        case Cons(x, xs) if i > 0 => Some(x(), (xs(), i - 1))
        case _ => None
    })

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this, true)((t: LazyList[A], v: Boolean) => {
      t match
        case Cons(x, xs) if v => Some(x(), (xs(), true))
        case _ => None
    })

  def zipWith[B, C](other: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold(this, other)((tt: LazyList[A], ot: LazyList[B]) => {
      (tt, ot) match
        case (Cons(xt, xst), Cons(xo, xso)) => Some(f(xt(), xo()), (xst(), xso()))
        case _ => None
    })

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold(this, that)((tt, ot) => {
      (tt, ot) match
        case (Cons(xt, xst), Cons(xo, xso)) => Some((Some(xt()), Some(xo())), (xst(), xso()))
        case (Cons(xt, xst), Empty) => Some((Some(xt()), None), (xst(), Empty))
        case (Empty, Cons(xo, xso)) => Some((None, Some(xo())), (Empty, xso()))
        case _ => None
    })

  def startsWith[B](s: LazyList[B]): Boolean =
    this.zipAll(s).takeWhile {
      case (_, Some(_)) => true
      case _ => false
    }.forAll((a, b) => a == b)

  def tails: LazyList[LazyList[A]] =
    unfold(this, false) ((s, b) => {
      s match
        case Cons(x, xs) if !b => Some(s, (xs(), false))
        case Empty if !b => Some(Empty, (Empty, true))
        case _ => None
    })

  def hasSubsequence[B >: A](l: LazyList[B]): Boolean =
    tails.exists(_.startsWith(l))

  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
    foldRight(init -> LazyList(init)): (a, b0) =>
      lazy val b1 = b0
      val b2 = f(a, b1(0))
      (b2, cons(b2, b1(1)))
    .apply(1)


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    lazy val inf = cons(a, continually(a))
    inf

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def fibs(h: Int, i: Int): LazyList[Int] = {
      val n = h + i
      cons(n, fibs(i, n))
    }

    cons(0, cons(1, fibs(0, 1)))

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some(value, newState) => cons(value, unfold(newState)(f))
      case _ => Empty

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1))((h: Int, i: Int) => Some(h, (i, h + i)))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(x => Some(x, x + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(())(_ => Some(a, ())) // () signifies Unit type

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(())(_ => Some(1, ()))
