package aoc.utils

import scala.annotation.tailrec

object ImplicitUtils {

  // Stolen from https://stackoverflow.com/a/21803339
  implicit class AddMultispanToSeq[A](val seq: Seq[A]) extends AnyVal {

    def multiSpan(splitOn: A => Boolean): Seq[Seq[A]] = {
      multispan(splitOn, dropDelimiter = false)
    }

    def multiSpanWithoutDelimiter(splitOn: A => Boolean): Seq[Seq[A]] = {
      multispan(splitOn, dropDelimiter = true)
    }

    private def multispan(splitOn: A => Boolean, dropDelimiter: Boolean): Seq[Seq[A]] = {

      def delimiterDropped(input: Seq[A]): Seq[A] = if (dropDelimiter)
        input.dropWhile(splitOn)
      else
        input

      @tailrec
      def loop(xs: Seq[A], acc: Seq[Seq[A]]): Seq[Seq[A]] = delimiterDropped(xs) match {
        case seq if seq.isEmpty => acc
        case x if x.length == 1 => Seq(x) ++ acc
        case multiElement       =>
          val h = multiElement.head
          val t = multiElement.tail
          val (pre, post) = delimiterDropped(t).span(!splitOn(_)) match {
            case (head, tail) => (head, delimiterDropped(tail))
          }
          loop(post, Seq(Seq(h) ++ pre) ++ acc)
      }

      loop(seq, Seq.empty).reverse
    }
  }
}
