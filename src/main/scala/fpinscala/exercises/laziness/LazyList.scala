package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.{cons, empty}

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

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: LazyList[B]): Boolean = ???


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

  def continually[A](a: A): LazyList[A] = ???

  def from(n: Int): LazyList[Int] = ???

  lazy val fibs: LazyList[Int] = ???

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???

  lazy val fibsViaUnfold: LazyList[Int] = ???

  def fromViaUnfold(n: Int): LazyList[Int] = ???

  def continuallyViaUnfold[A](a: A): LazyList[A] = ???

  lazy val onesViaUnfold: LazyList[Int] = ???
