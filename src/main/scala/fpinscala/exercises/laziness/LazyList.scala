package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Cons(h, t) => h() :: t().toList
    case Empty      => Nil

  def foldRight[B](
      z: => B
  )(
      f: (A, => B) => B
  ): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h, t) =>
        f(
          h(),
          t().foldRight(z)(f)
        ) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) =>
      if n > 0 then Cons(h, () => t().take(n - 1)) else Empty
    case Empty => Empty

  def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) =>
      if n > 0 then t().drop(n - 1) else Cons(h, t)
    case Empty => Empty

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(LazyList[A]())((a, b) =>
      if p(a) then LazyList.cons(a, b) else Empty
    )
    // this match
    // case Cons(h, t) =>
    //   if p(h()) then Cons(h, () => t().takeWhile(p)) else Empty
    // case Empty => Empty

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, b) => Some(a))
    // this match
    // case Cons(h, t) => Some(h())
    // case Empty      => None

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): LazyList[B] =
    foldRight(LazyList.empty[B])((a, b) => LazyList.cons(f(a), b))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((a, b) =>
      if f(a) then LazyList.cons(a, b) else b
    )

  def append[B >: A](l: LazyList[B]): LazyList[B] =
    foldRight(l)((a, b) => LazyList.cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList.empty[B])((a, b) =>
      f(a).foldRight(b)((aa, bb) => LazyList.cons(aa, bb))
    )

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    LazyList.unfold(this)(x =>
      x match
        case Cons(h, t) => Some(f(h()), t())
        case _          => None
    )

  def takeViaUnfold(n: Int): LazyList[A] =
    LazyList.unfold((this, n))((x, i) =>
      x match
        case Cons(h, t) => if i > 0 then Some(h(), (t(), i - 1)) else None
        case _          => None
    )

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] =
    LazyList.unfold(this)(x =>
      x match
        case Cons(h, t) => if f(h()) then Some(h(), t()) else None
        case _          => None
    )

  def zipWith[B, C](list: LazyList[B])(f: (A, B) => C): LazyList[C] =
    LazyList.unfold((this, list))(x =>
      x match
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        case _                            => None
    )

  def zipAll[B](list: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold(this, list)(x =>
      x match
        case (Cons(h1, t1), Cons(h2, t2)) =>
          Some((Some(h1()), Some(h2())), (t1(), t2()))
        case (Cons(h1, t1), Empty) =>
          Some((Some(h1()), None), (t1(), Empty))
        case (Empty, Cons(h2, t2)) =>
          Some((None, Some(h2())), (Empty, t2()))
        case _ => None
    )

  def startsWith[B >: A](s: LazyList[B]): Boolean =
    this
      .zipAll(s)
      .takeWhile(!_._2.isEmpty)
      .forAll((h1, h2) => h1 == h2)

  def tails: LazyList[LazyList[A]] =
    LazyList
      .unfold(this) {
        case Empty => None
        case l     => Some(l, l.drop(1))
      }
      .append(LazyList(LazyList.Empty))

  def hasSubsequence[A](l: LazyList[A]): Boolean =
    tails.exists(_.startsWith(l))

  def scanRight[B >: A](z: B)(f: (A, B) => B): LazyList[B] =
    foldRight(z, LazyList(z))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, LazyList.cons(b2, p1._2))
    })._2

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

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def go(f0: Int, f1: Int): LazyList[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    val a = f(state)
    a match
      case None       => LazyList[A]()
      case Some(a, s) => cons(a, unfold(s)(f))

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1))((x, y) => Some(x, (y, x + y)))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(x => Some(x, x + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(a)(x => Some(x, x))

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(1)(x => Some(x, x))
