package aoc.aoc2023.day4

import aoc.aoc2023.DailyPuzzle2023

case object Day4Puzzle extends DailyPuzzle2023(4, "Scratchcards") {

  override def calculatePart1(
    lines: Seq[String]): String = getCardData(lines).map(data => math.pow(2, data.winningCnt - 1).floor.toInt).sum
    .toString

  override def calculatePart2(lines: Seq[String]): String = {
    val initialDeck = getCardData(lines)

    initialDeck.foldLeft(initialDeck)((deck, initialCard) => {
      deck.map(card => card.id match {
        case value if initialCard.id + 1 to (initialCard.id + initialCard.winningCnt) contains value =>
          card.copy(cntInDeck = card.cntInDeck + deck(initialCard.id - 1).cntInDeck)
        case _                                                                                       => card
      })
    }).map(_.cntInDeck).sum.toString
  }

  private def getCardData(lines: Seq[String]): IndexedSeq[CardData] = {
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

  private case class CardData(id: Int, winnerNums: Seq[Int], numbers: Seq[Int], winningCnt: Int, cntInDeck: Int)
}
