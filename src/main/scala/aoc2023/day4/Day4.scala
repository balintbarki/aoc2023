package aoc2023.day4

import aoc2023.DailyApp

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Day4App {
  val inputPath = "src/main/scala/aoc2023/day4/input_Day4.txt"

  def getCardData(lines: Seq[String]): IndexedSeq[CardData] = {
    val topRegex = """Card\s+(\d+):([\d\s]+)\|([\d\s]+)""".r
    val numberRegex = """(\d+)""".r
    lines.map {
      case topRegex(cardId, winningNumbersStr, numbersStr) =>
        val winnerNums = numberRegex.findAllIn(winningNumbersStr).matchData.map(_.matched.toInt).toSeq
        val nums = numberRegex.findAllIn(numbersStr).matchData.map(_.matched.toInt).toSeq
        val winningCnt = winnerNums.toSet.intersect(nums.toSet).size
        CardData(cardId.toInt, winnerNums, nums, winningCnt, 1)
    }.toIndexedSeq
  }
}

object Day4_Part1 extends DailyApp {
  override def calculate(inputPath: String): Int = {
    Day4App.getCardData(Files.readAllLines(Paths.get(inputPath)).asScala.toSeq)
      .map(data => math.pow(2, data.winningCnt - 1).floor.toInt).sum
  }

  println(calculate(Day4App.inputPath))
}

object Day4_Part2 extends DailyApp {
  override def calculate(inputPath: String): Int = {
    val initialDeck = Day4App.getCardData(Files.readAllLines(Paths.get(inputPath)).asScala.toSeq)

    initialDeck.foldLeft(initialDeck)((deck, initialCard) => {
      deck.map(card => card.id match {
        case value if initialCard.id + 1 to (initialCard.id + initialCard.winningCnt) contains value =>
          card.copy(cntInDeck = card.cntInDeck + deck(initialCard.id - 1).cntInDeck)
        case _                                                                                       => card
      })
    }).map(_.cntInDeck).sum
  }

  println(calculate(Day4App.inputPath))
}

final case class CardData(id: Int, winnerNums: Seq[Int], numbers: Seq[Int], winningCnt: Int, cntInDeck: Int)
