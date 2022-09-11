package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E, +A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Left(x)  => Left(x)
    case Right(x) => Right(f(x))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(x)  => Left(x)
    case Right(x) => f(x)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Left(x) => b
    case r       => r

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(x => b.map(bb => f(x, bb)))

object Either:
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(List[B]()))((x, acc) =>
      f(x).map2(acc)(_ :: _)
    )

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(List[A]()))((x, acc) =>
      x.map2(acc)(_ :: _)
    )

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if xs.isEmpty then Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](
      a: Either[List[E], A],
      b: Either[List[E], B],
      f: (A, B) => C
  ): Either[List[E], C] =
    (a, b) match
      case (Left(ae), Left(be))   => Left(ae ++ be)
      case (Left(ae), _)          => Left(ae)
      case (_, Left(be))          => Left(be)
      case (Right(aa), Right(bb)) => Right(f(aa, bb))

  def traverseAll[E, A, B](
      es: List[A],
      f: A => Either[List[E], B]
  ): Either[List[E], List[B]] =
    es.foldRight[Either[List[E], List[B]]](Right(List[B]()))((x, acc) =>
      map2All(f(x), acc, _ :: _)
    )

  def sequenceAll[E, A](
      es: List[Either[List[E], A]]
  ): Either[List[E], List[A]] =
    es.foldRight[Either[List[E], List[A]]](Right(List[A]()))((x, acc) =>
      map2All(x, acc, _ :: _)
    )
