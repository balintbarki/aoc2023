package aoctest

import aoc.DailyPuzzle
import aoc.aoc2023.PuzzleRunner
import aoc.utils.FileUtils
import org.junit.Assert

abstract class PuzzleTest(val puzzle: DailyPuzzle) {

  val testInputPath: String = FileUtils.getPuzzleTestInputPath(day)
  val testInputPathPart1: String = FileUtils.getPuzzleTestInputPath(day, Some(1))
  val testInputPathPart1Sub1: String = FileUtils.getPuzzleTestInputPath(day, Some(1), Some(1))
  val testInputPathPart1Sub2: String = FileUtils.getPuzzleTestInputPath(day, Some(1), Some(2))
  val testInputPathPart2: String = FileUtils.getPuzzleTestInputPath(day, Some(2))

  def testPart1Real(expectedValue: String): Unit = {
    testPartReal(expectedValue, 1)
  }

  def testPart1(expectedValue: String, inputPath: Option[String] = None): Unit = {
    testPart(expectedValue, 1, inputPath)
  }

  def testPart2(expectedValue: String, inputPath: Option[String] = None): Unit = {
    testPart(expectedValue, 2, inputPath)
  }

  def testPart2Real(expectedValue: String): Unit = {
    testPartReal(expectedValue, 2)
  }

  private def testPart(expectedValue: String, part: Int, inputPath: Option[String]): Unit = {
    val (result, _) = PuzzleRunner
      .runPuzzlePart(puzzle, part, inputPath.getOrElse(FileUtils.getPuzzleTestInputPath(day, Some(part))))
    Assert.assertEquals(expectedValue, result)
  }

  private def day: Int = puzzle.day

  private def testPartReal(expectedValue: String, part: Int): Unit = {
    testPart(expectedValue, part, inputPath = Some(puzzle.inputPath))
  }
}
