package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil

  /** Another data constructor, representing nonempty lists. Note that `tail` is
    * another `List[A]`, which may be `Nil` or another `Cons`.
    */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: List[Double]): Double = ds match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  val x = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    // case _                                     => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def foldRight[A, B](
      as: List[A],
      acc: B,
      f: (A, B) => B
  ): B = // Utility functions
    as match
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(
      ns,
      1.0,
      _ * _
    ) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match
      case Cons(x, y) => y
      case Nil        => sys.error("tail on empty list")

  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Cons(x, xs) => Cons(h, xs)
      case Nil         => sys.error("setHead on empty list")

  def drop[A](l: List[A], n: Int): List[A] =
    l match
      case Cons(x, xs) => if n > 0 then drop(xs, n - 1) else Cons(x, xs)
      case Nil         => Nil

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case Cons(x, xs) => if f(x) then dropWhile(xs, f) else Cons(x, xs)
      case Nil         => Nil

  def init[A](l: List[A]): List[A] =
    l match
      case Cons(x, Nil) => Nil
      case Cons(x, xs)  => Cons(x, init(xs))
      case Nil          => sys.error("init on empty list")

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (x, y) => 1 + y)

  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil         => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (x, y) => 1 + x)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A](), (x, y) => Cons(y, x))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (x, y) => Cons(x, y))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A](), (x, y) => appendViaFoldRight(x, y))

  def incrementEach(l: List[Int]): List[Int] =
    l match
      case Cons(x, xs) => Cons(x + 1, incrementEach(xs))
      case Nil         => Nil

  def doubleToString(l: List[Double]): List[String] =
    l match
      case Cons(x, xs) => Cons(x.toString(), doubleToString(xs))
      case Nil         => Nil

  def map[A, B](l: List[A])(f: A => B): List[B] =
    l match
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
      case Nil         => Nil

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match
      case Cons(x, xs) => if f(x) then Cons(x, filter(xs)(f)) else filter(xs)(f)
      case Nil         => Nil

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    as match
      case Cons(x, xs) => append(f(x), flatMap(xs)(f))
      case Nil         => Nil

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((x) => if f(x) then List[A](x) else Nil)

  def head[A](l: List[A]): A =
    l match
      case Cons(x, xs) => x
      case Nil         => sys.error("head on empty list")

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match
      case (Cons(ax, axs), Cons(bx, bxs)) =>
        Cons(ax + bx, addPairwise(axs, bxs))
      case (ax, Nil) => Nil
      case (Nil, bx) => Nil

  // def zipWith - TODO determine signature
  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    (a, b) match
      case (Cons(ax, axs), Cons(bx, bxs)) =>
        Cons(f(ax, bx), zipWith(axs, bxs, f))
      case (ax, Nil) => Nil
      case (Nil, bx) => Nil

  def take[A](l: List[A], n: Int): List[A] =
    l match
      case Cons(x, xs) => if n > 0 then Cons(x, take(xs, n - 1)) else Nil
      case Nil         => Nil

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (0 to length(sup))
      .map(x => drop(sup, x))
      .foldLeft(false)((acc, x) => acc || take(x, length(sub)) == sub)
