import scala.annotation.tailrec;

package chapter2 {
  object Exercises {
    /**
      * Return the nth fibonacci number.
      *
      * Uses tail-recursion.
      */
    def fibonacci(n: Int): Int = {
      @tailrec
      def go(n: Int, prev: Int, acc: Int): Int = {
        if (n == 0) acc
        else go(n - 1, acc, acc + prev)
      }
      go(n, 1, 0)
    }

    def isSorted[A](as: Array[A], p: (A, A) => Boolean): Boolean = {
      @tailrec
      def go(index: Int): Boolean = {
        if (as.length - 1 == index) true
        else if (p(as(index), as(index + 1))) false
        else go(index + 1)
      }
      go(0)
    }

    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
      (a: A) => (b: B) => f(a, b)
    }

    def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
      (a: A, b: B) => f(a)(b)
    }

    def compose[A, B, C](f: B => C, g: A => B): A => C = {
      a: A => f(g(a))
    }
  }
}
