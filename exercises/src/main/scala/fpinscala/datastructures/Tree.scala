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

}
