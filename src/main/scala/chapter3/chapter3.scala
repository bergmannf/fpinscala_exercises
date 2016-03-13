package chapter3 {
  sealed trait List[+A];
  case object Nil extends List[Nothing];
  case class Cons[+A](a: A, as: List[A]) extends List[A];

  object List {
    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    def tail[A](as: List[A]): List[A] = {
      as match {
        case Cons(_, bs) => bs
        case Nil => Nil
      }
    }

    def setHead[A](a: A, as: List[A]): List[A] = {
      as match {
        case Nil => Cons(a, Nil)
        case Cons(b, bs) => Cons(a, bs)
      }
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else {
        l match {
          case Cons(a, as) => drop(as, n - 1)
          case Nil => Nil
        }
      }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      l match {
        case Cons(a, as) if (f(a)) => dropWhile(as, f)
        case _ => Nil
      }
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(a, Nil) => Nil
      case Cons(a, as) => Cons(a, init(as))
    }

    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
      l match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
    }

    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
      l match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }
    }
  }
}
