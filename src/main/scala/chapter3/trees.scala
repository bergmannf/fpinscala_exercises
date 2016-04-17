package chapter3{
  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    /** Count the number of nodes in a tree. */
    def count[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + (count(l) + count(r))
      }
    }

    def maximum(tree: Tree[Int]): Int = {
      tree match {
        case Leaf(a) => a
        case Branch(l, r) => {
          val lMax = maximum(l)
          val rMax = maximum(r)
          lMax max rMax
        }
      }
    }

    def depth[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 0
        case Branch(l, r) => 1 + (depth(l) max depth(r))
      }
    }

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
      tree match {
        case Leaf(a) => Leaf(f(a))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }
    }

    def fold[A, B](tree: Tree[A])(g: A => B)(f: (B, B) => B): B = {
      tree match {
        case Leaf(a) => g(a)
        case Branch(l, r) => f(fold(l)(g)(f), fold(r)(g)(f))
      }
    }

    def sizeViaFold[A](tree: Tree[A]): Int = {
      fold(tree)((_) => 1)(_ + _ + 1)
    }

    def maximumViaFold(tree: Tree[Int]): Int = {
      fold(tree)(a => a)(_ max _)
    }

    def depthViaFold(tree: Tree[Int]): Int = {
      fold(tree)(a => 0)(_ max _ + 1)
    }

    def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
      fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
    }
  }
}
