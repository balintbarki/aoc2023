package aoc.aoc2023.day3

import aoc.DailyPuzzle

import scala.util.matching.Regex

case object Day3Puzzle extends DailyPuzzle(3, "Gear Ratios") {
  private val symbolRegex: Regex = """[/*&+$\-%=@#]""".r

  override def calculatePart1(
    lines: Seq[String]): String = {
    val schematicsLines = lines.zipWithIndex
    val boxes = getAllBoxes(schematicsLines)

    boxes.map(box => if (boxHasSymbol(box))
      box.number
    else
      0).sum.toString
  }

  override def calculatePart2(
    lines: Seq[String]): String = {
    val schematicsLines = lines.zipWithIndex
    val boxes = getAllBoxes(schematicsLines)
    val asteriskMap: Map[Coordinate, Seq[Int]] = boxes
      .foldLeft[Map[Coordinate, Seq[Int]]](Map())((asteriskMap, box) => {
        val asterisk = """\*""".r
        box.lineFragments.foldLeft(asteriskMap)((asteriskMap, lineFragment) => {
          val asterisks = asterisk.findAllIn(lineFragment._1).matchData
          val result = asterisks.foldLeft(asteriskMap)((map, matcher) => {
            val column = lineFragment._3 + matcher.start
            val coordinate = Coordinate(lineFragment._2, column)
            val newSeq = map.getOrElse(coordinate, Seq()) ++ Seq(box.number)
            map ++ Map(coordinate -> newSeq)
          })
          result
        })
      })

    asteriskMap.filter { case (_, numbers) => numbers.length == 2 }
      .map { case (_, numbers) => numbers.head * numbers.last }.sum.toString
  }

  private def boxHasSymbol(box: Box): Boolean = {
    box.lineFragments.foreach { case (line, _, _) => if (symbolRegex.findFirstIn(line).isDefined) return true }
    false
  }

  private def getAllBoxes(schematicsLines: Seq[(String, Int)]): Seq[Box] = {
    val lineCnt = schematicsLines.length
    val numberRegex = """\d+""".r
    schematicsLines.flatMap { case (line, index) =>
      val numbers = numberRegex.findAllIn(line)
      numbers.matchData.map(matcher => {
        val number = matcher.matched
        val colStart = matcher.start
        val colEnd = colStart + number.length
        val boxColStart = if (colStart == 0) 0 else colStart - 1
        val boxColEnd = if (colEnd + 1 > line.length) line.length else colEnd + 1
        val boxRowStart = if (0 < index) index - 1 else 0
        val boxRowEnd = if (index < (lineCnt - 1)) index + 1 else lineCnt - 1
        val box = Box(number.toInt,
          schematicsLines.filter { case (_, index) => (boxRowStart <= index) && (index <= boxRowEnd) }
            .map { case (line, index) => (line.substring(boxColStart, boxColEnd), index, boxColStart) })
        box
      })
    }
  }

  private case class Box(number: Int, lineFragments: Seq[(String, Int, Int)])

  private case class Coordinate(line: Int, column: Int)
}
