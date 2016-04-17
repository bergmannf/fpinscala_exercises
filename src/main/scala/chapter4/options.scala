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
  def variance(xs: Seq[Double]): Option[Double] = {
    None
  }
}
