package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  @tailrec
  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match
      case Nil => throw sys.error("Empty list")
      case Cons(_, xs) => xs

  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Nil => Cons(h, Nil)
      case Cons(_, xs) => Cons(h, xs)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if n <= 0 then return l
    l match
      case Nil => sys.error("Can't drop from an empty list")
      case Cons(_, xs) => drop(xs, n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case Nil => sys.error("Can't drop from an empty list")
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l

  def init[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("Can't drop from an empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, acc) => acc + 1)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A](), (acc: List[A], y) => Cons(y, acc))

  def foldRightViaFoldLeft[A, B](l: List[A], acc: B, f: (A, B) => B) =
    foldLeft(reverse(l), acc, (b: B, a: A) => f(a, b))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (x, y) => Cons(x, y))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A](), append)

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, List[Int](), (x, y) => Cons(x + 1, y))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String](), (x, y) => Cons(x.toString, y))

  def map[A,B](l: List[A], f: A => B): List[B] =
    foldRight(l, List[B](), (x, y) => Cons(f(x), y))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, List[A](), (x, y) => {
      x match
        case x if f(x) => Cons(x, y)
        case _ => y
    })

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    foldLeft(as, List[B](), (y, x) => {
      foldRight(y, f(x), (a, b) => Cons(a, b))
    })

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then Cons(a, Nil) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, y), Cons(w, z)) => Cons(x + w, addPairwise(y, z))

  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    (a, b) match
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, y), Cons(u, v)) => Cons(f(x, u), zipWith(y, v, f))

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
    
