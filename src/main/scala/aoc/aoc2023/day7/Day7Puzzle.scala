package aoc.aoc2023.day7

import aoc.DailyPuzzle


case object Day7Puzzle extends DailyPuzzle(7, "Camel Cards") {
  override def calculatePart1(
    lines: Seq[String]): String = calculate(lines, useJoker = false)

  override def calculatePart2(
    lines: Seq[String]): String = calculate(lines, useJoker = true)

  private def calculate(lines: Seq[String], useJoker: Boolean): String = {
    val lineRegex = s"([${HandType.codeValues(useJoker).mkString("")}]{5}) (\\d+)".r
    val handsWithBets = lines.map {
      case lineRegex(codes, bet) => (HandType(codes.toCharArray, useJoker = useJoker), bet.toLong)
    }

    val sortedHandsWithBets = handsWithBets.sorted.toIndexedSeq

    sortedHandsWithBets.map { handWithBet => {
      (sortedHandsWithBets.indexOf(handWithBet) + 1) * handWithBet._2
    }
    }.sum.toString
  }
}

abstract class HandType(val codes: Seq[Char], val useJoker: Boolean) extends Ordered[HandType] {

  def value: Int

  def compare(that: HandType): Int = if (value == that.value)
    secondaryValue.compare(that.secondaryValue)
  else value.compare(that.value)

  def secondaryValue: Long = {
    val codeValues = HandType.codeValues(useJoker)
    val values = codes.map(code => codeValues.indexOf(code))
    val base = codeValues.size
    codes.indices.map(index => values(index) * math.pow(base, codes.size - index).toLong).sum
  }
}

object HandType {

  val JOKER = 'J'

  def codeValues(useJoker: Boolean = false): IndexedSeq[Char] = if (useJoker)
    IndexedSeq(JOKER, '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A')
  else
    IndexedSeq('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')

  def apply(codes: Seq[Char], useJoker: Boolean): HandType = {

    val (jokers, notJokers) = if (useJoker)
      (codes.filter(_ == JOKER), codes.filter(_ != JOKER))
    else
      (Seq(), codes)

    val notJokerGroups = notJokers.groupBy(identity).map { case (_, seq) => seq.length }.toSeq.sorted
    val lastGroupSize = if (notJokerGroups.isEmpty)
      0
    else
      notJokerGroups.last

    val groups = notJokerGroups.dropRight(1) ++ Seq(lastGroupSize + jokers.size)

    groups match {
      case Seq(1, 1, 1, 1, 1) => HighCard(codes, useJoker)
      case Seq(1, 1, 1, 2)    => OnePair(codes, useJoker)
      case Seq(1, 1, 3)       => ThreeOfAKind(codes, useJoker)
      case Seq(1, 2, 2)       => TwoPair(codes, useJoker)
      case Seq(1, 4)          => FourOfAKind(codes, useJoker)
      case Seq(2, 3)          => FullHouse(codes, useJoker)
      case Seq(5)             => FiveOfAKind(codes, useJoker)
    }
  }

  case class FiveOfAKind(override val codes: Seq[Char], override val useJoker: Boolean) extends HandType(codes,
    useJoker) {
    override def value: Int = 7
  }

  case class FourOfAKind(override val codes: Seq[Char], override val useJoker: Boolean) extends HandType(codes,
    useJoker) {
    override def value: Int = 6
  }

  case class FullHouse(override val codes: Seq[Char], override val useJoker: Boolean) extends HandType(codes,
    useJoker) {
    override def value: Int = 5
  }

  case class ThreeOfAKind(override val codes: Seq[Char], override val useJoker: Boolean) extends HandType(codes,
    useJoker) {
    override def value: Int = 4
  }

  case class TwoPair(override val codes: Seq[Char], override val useJoker: Boolean) extends HandType(codes, useJoker) {
    override def value: Int = 3
  }

  case class OnePair(override val codes: Seq[Char], override val useJoker: Boolean) extends HandType(codes, useJoker) {
    override def value: Int = 2
  }

  case class HighCard(override val codes: Seq[Char], override val useJoker: Boolean) extends HandType(codes, useJoker) {
    override def value: Int = 1
  }
}
