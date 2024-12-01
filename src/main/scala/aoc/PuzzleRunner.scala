package aoc

import aoc.aoc2015.PuzzleCollection2015
import aoc.aoc2023.PuzzleCollection2023
import aoc.aoc2024.PuzzleCollection2024

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}


object PuzzleRunner extends App {

  private val notImplementedPuzzle: DailyPuzzle = new DailyPuzzle(0, 0, "Not implemented") {
    override def calculatePart1(
      lines: Seq[String]): String = "This puzzle is not implemented"

    override def calculatePart2(
      lines: Seq[String]): String = "This puzzle is not implemented"
  }
  private val puzzleCollections: Seq[PuzzleCollection] = Seq(
    //PuzzleCollection2015,
    //PuzzleCollection2023,
    PuzzleCollection2024,
  )

  def runPuzzlePart(puzzle: DailyPuzzle, part: Int, inputFilePath: Path): (String, Long) = {
    val lines = Files.readAllLines(inputFilePath).asScala.toSeq
    val startTime = System.currentTimeMillis()

    val result = Try({
      if (part == 1)
        puzzle.calculatePart1(lines)
      else if (part == 2)
        puzzle.calculatePart2(lines)
      else
        throw new IllegalArgumentException("Unexpected part")
    }) match {
      case Success(value)     => value
      case Failure(exception) => exception.printStackTrace(); exception.getMessage
    }

    val deltaT = System.currentTimeMillis() - startTime

    (result, deltaT)
  }

  def runPuzzle(puzzleCollection: PuzzleCollection, day: Int): Unit = {
    val puzzle = puzzleCollection.puzzles.find(_.day == day).getOrElse(notImplementedPuzzle)
    val inputFileName = puzzle.inputPath

    println(s"    Results of Day $day - ${puzzle.name}:")

    (1 to 2).foreach(part => {
      val (result, time) = runPuzzlePart(puzzle, part, inputFileName)
      println(f"        Part$part: $result%-20s ($time ms)")
    })
  }

  puzzleCollections.foreach(
    collection => {
      println(s"Running puzzles for ${collection.year}:")
      collection.puzzles.map(puzzle => puzzle.day).sorted.foreach { day => runPuzzle(collection, day) }
      println()
    })
}
