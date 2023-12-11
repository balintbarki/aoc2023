package aoc.aoc2023.day11

import aoc.DailyPuzzle

import scala.math.abs

case object Day11Puzzle extends DailyPuzzle(11, "unknown") {
  override def calculatePart1(
    lines: Seq[String]): String = {
    val expandedUniverse = expandUniverse(lines, 1)
    println(expandedUniverse)

    val galaxies: Seq[(Int, Int)] = for {
      x <- expandedUniverse.head.indices
      y <- expandedUniverse.indices
      if expandedUniverse(y)(x) == '#'
    } yield (x, y)

    val distances = for {
      (first, firstIdx) <- galaxies.zipWithIndex
      (second, secondIdx) <- galaxies.zipWithIndex
      if firstIdx < secondIdx
    } yield abs(first._1 - second._1) + abs(first._2 - second._2)

    distances.sum.toString
  }

  override def calculatePart2(
    lines: Seq[String]): String = {

    val expandedUniverse = expandUniverse(lines, 1)
    println(expandedUniverse)

    val galaxies: Seq[(Int, Int)] = for {
      x <- expandedUniverse.head.indices
      y <- expandedUniverse.indices
      if expandedUniverse(y)(x) == '#'
    } yield (x, y)

    val distances = for {
      (first, firstIdx) <- galaxies.zipWithIndex
      (second, secondIdx) <- galaxies.zipWithIndex
      if firstIdx < secondIdx
    } yield abs(first._1 - second._1) + abs(first._2 - second._2)

    distances.sum.toString
  }


  private def expandUniverse(lines: Seq[String], times: Int): Seq[String] = {
    val expandedLines = lines.flatMap { line => {
      Seq(Some(line)) ++ {
        if (line.forall(_ == '.')) Seq.fill(times)(Some(line)) else Seq()
      }
    }.flatten
    }

    val columnsToExpand = expandedLines.head.indices.filter(index => expandedLines.map(_(index)).forall(_ == '.'))

    expandedLines.map { line =>
      line.indices.flatMap(index =>
        Seq(Some(line(index))) ++ {
          if (columnsToExpand.contains(index)) Seq.fill(times)(Some(line(index))) else Seq()
        }
      ).flatten.mkString
    }
  }
}
