package chapter4

import scala.collection.Seq

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    // Map and unpack the Option[Option[B]] type.
    this.map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(a) => a
      case _ => default
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this.map(a => Some(a)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    this.flatMap(a => if (f(a)) Some(a) else None)
  }
}

case class Some[A](a: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(a => b.map(b => f(a, b)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((opt, b) => map2(opt, b)(_ :: _))
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    as match {
      case Nil => Some(Nil)
      case h::tail => map2(f(h), traverse(tail)(f))(_ :: _)
    }
  }

  def traverse_fold[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    as.foldRight[Option[List[B]]](Some(Nil))((h, t) => (map2(f(h), t)(_ :: _)))
  }
}
