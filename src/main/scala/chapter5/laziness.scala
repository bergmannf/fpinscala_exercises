package chapter5;
import scala.annotation;

sealed trait Stream[+A] {
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], l: List[A]): List[A] = {
      s match {
        case Cons(h, t) => go(t(), h() :: l)
        case Empty => l
      }
    }
    go(this, List.empty).reverse
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
      case _ => Stream.empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case Empty => Stream.empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Stream.empty
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case Empty => z
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    this.foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhileFold(p: A => Boolean): Stream[A] = {
    this.foldRight(Stream.empty[A])((a, b) => if (p(a)) {
      Stream.cons(a, b)
    }
    else Stream.empty)
  }

  def headOption(): Option[A] = {
    this.foldRight(None: Option[A])((a, b) => Some(a))
  }

  def map[B](f: A => B): Stream[B] = {
    this.foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    this.foldRight(Stream.empty: Stream[A])(
      (a, b) => if (f(a)) Stream.cons(a, b)
      else b.filter(f)
    )
  }

  def append[B >: A](bs: => Stream[B]): Stream[B] = {
    this.foldRight(bs)((a, t) => Stream.cons(a, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    this.foldRight(Stream.empty[B])((a, b) => f(a) append b)
  }

  def unfoldMap[B](f: A => B): Stream[B] = {
    Stream.unfold(this)({
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    })
  }

  def unfoldTake(n: Int): Stream[A] = {
    Stream.unfold((this, n))({
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    })
  }

  def unfoldTakeWhile(f: A => Boolean): Stream[A] = {
    Stream.unfold((this))({
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    })
  }

  def zipWith[B](s: Stream[B]): Stream[(A, B)] = {
    Stream.unfold((this, s))({
      case (Cons(h, t), Cons(h2, t2)) => Some(((h(), h2()), (t(), t2())))
      case _ => None
    })
  }
}
case object Empty extends Stream[Nothing];
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A];

object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def integers(n: Int): Stream[Int] = {
    cons(n, integers(n + 1))
  }

  def fibs(): Stream[Int] = {
    def go(i: Int, j: Int): Stream[Int] = {
      cons(i, go(j, i + j))
    }
    go(0, 1)
  }

  /** This might be the first usage of a state monad **/
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    val next = f(z)
    next match {
      case Some((a, state)) => cons(a, unfold(state)(f))
      case None => Stream.empty
    }
  }

  def unfoldFibs(): Stream[Int] = {
    def state(tup: (Int, Int)): Option[(Int, (Int, Int))] = {
      val (prev, curr) = tup
      Some((curr, (curr, prev + curr)))
    }
    unfold[Int, (Int, Int)]((0, 1))(state)
  }

  def unfoldOnes(n: Int): Stream[Int] = {
    unfold(1)(n => Some((1, 1)))
  }

  def unfoldConstant[A](a: A): Stream[A] = {
    unfold(a)(a => Some((a, a)))
  }

  def unfoldIntegers(n: Int): Stream[Int] = {
    unfold(n)(n => Some((n, n+1)))
  }
}
