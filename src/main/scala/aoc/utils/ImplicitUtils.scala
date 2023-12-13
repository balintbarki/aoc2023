package aoc.utils

import scala.annotation.tailrec

object ImplicitUtils {

  // Stolen from https://stackoverflow.com/a/21803339
  implicit class AddMultispanToList[A](val list: List[A]) extends AnyVal {
    def multiSpan(splitOn: A => Boolean): List[List[A]] = {
      @tailrec
      def loop(xs: List[A], acc: List[List[A]]): List[List[A]] = xs match {
        case Nil      => acc
        case x :: Nil => List(x) :: acc
        case h :: t   =>
          val (pre, post) = t.span(!splitOn(_))
          loop(post, (h :: pre) :: acc)
      }

      loop(list, Nil).reverse
    }
  }

}
