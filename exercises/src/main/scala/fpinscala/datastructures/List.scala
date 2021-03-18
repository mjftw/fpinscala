package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil
    extends List[
      Nothing
    ] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](
      as: List[A],
      z: B
  )(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(
      _ * _
    ) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(as: Array[A]): Boolean = {
      if (as.length < 2) true
      else if (ordered(as(0), as(1))) loop(as.drop(1))
      else false
    }

    loop(as)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))

  def appendfR[A](l: List[A], item: A): List[A] =
    foldRight(l, List(item))((a, b) => Cons(a, b))

  def tail[A](as: List[A]): List[A] = {
    as match {
      case Nil              => Nil
      case Cons(head, tail) => tail
    }
  }

  def setHead[A](as: List[A], head: A): List[A] = {
    as match {
      case Nil           => Nil
      case Cons(_, tail) => Cons(head, tail)
    }
  }

  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Nil                      => Nil
    case Cons(_, tail) if (n < 0) => drop[A](tail, n - 1)
    case _                        => as
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  // Not tail recursive!
  def init[A](as: List[A]): List[A] = as match {
    case Nil              => Nil
    case Cons(head, Nil)  => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def length[A](l: List[A]): Int = ???

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil          => z
    case Cons(hd, tl) => foldLeft(tl, f(z, hd))(f)
  }

  def head[A](as: List[A]): A = as match {
    case Cons(h, t) => h
  }

  def flatten[A](ll: List[List[A]]): List[A] = {
    def flattenAcc[A](las: List[List[A]], as: List[A]): List[A] = las match {
      case Nil           => as
      case Cons(Nil, tl) => flattenAcc(tl, as)
      case Cons(Cons(hlh, hlt), tl) =>
        Cons(hlh, flattenAcc(Cons(hlt, tl), as))
    }

    flattenAcc[A](ll, Nil)
  }

  def add1(ints: List[Int]): List[Int] =
    foldRight[Int, List[Int]](ints, Nil)((a, acc) => Cons(a + 1, acc))

  def doubleToStr(nums: List[Double]): String =
    foldLeft(nums, "")((str, num) => str + " " + num.toString())

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight[A, List[B]](l, Nil)((a, b) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight[A, List[A]](as, Nil)((a, acc) => if (f(a)) Cons(a, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

}
