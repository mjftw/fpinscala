package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  val eg = Branch(
    Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))),
    Branch(Leaf(5), (Leaf(6)))
  )

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v)      => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(fb: (B, B) => B, fl: A => B): B =
    t match {
      case Leaf(value)         => fl(value)
      case Branch(left, right) => fb(fold(left)(fb, fl), fold(right)(fb, fl))
    }

  def size2[A](t: Tree[A]): Int = fold[A, Int](t)((l, r) => l + r, _ => 1)
}
