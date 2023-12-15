package aoc.utils

import scala.annotation.tailrec

object ImplicitUtils {

  // Stolen from https://stackoverflow.com/a/21803339
  implicit class AddMultispanToList[A](val list: List[A]) extends AnyVal {

    def multiSpan(splitOn: A => Boolean): List[List[A]] = {
      multispan(splitOn, dropDelimiter = false)
    }

    def multiSpanWithoutDelimiter(splitOn: A => Boolean): List[List[A]] = {
      multispan(splitOn, dropDelimiter = true)
    }

    private def multispan(splitOn: A => Boolean, dropDelimiter: Boolean): List[List[A]] = {

      def delimiterDropped(input: List[A]): List[A] = if (dropDelimiter)
        input.dropWhile(splitOn)
      else
        input

      @tailrec
      def loop(xs: List[A], acc: List[List[A]]): List[List[A]] = delimiterDropped(xs) match {
        case Nil      => acc
        case x :: Nil => List(x) :: acc
        case h :: t   =>
          val (pre, post) = delimiterDropped(t).span(!splitOn(_)) match {
            case (head, tail) => (head, delimiterDropped(tail))
          }
          loop(post, (h :: pre) :: acc)
      }

      loop(list, Nil).reverse
    }
  }

}
