package aoctest

import aoc.{DailyPuzzle, PuzzleRunner}
import aoc.utils.FileUtils
import org.junit.Assert

import java.nio.file.Path

abstract class PuzzleTest(val puzzle: DailyPuzzle) {

  val inputPath: Path = FileUtils.getPuzzleInputPath(year, day)
  val testInputPath: Path = FileUtils.getPuzzleTestInputPath(year, day)
  val testInputPathPart1: Path = FileUtils.getPuzzleTestInputPath(year, day, Some(1))
  val testInputPathPart1Sub1: Path = FileUtils.getPuzzleTestInputPath(year, day, Some(1), Some(1))
  val testInputPathPart1Sub2: Path = FileUtils.getPuzzleTestInputPath(year, day, Some(1), Some(2))
  val testInputPathPart2: Path = FileUtils.getPuzzleTestInputPath(year, day, Some(2))
  val testInputPathPart2Sub1: Path = FileUtils.getPuzzleTestInputPath(year, day, Some(2), Some(1))
  val testInputPathPart2Sub2: Path = FileUtils.getPuzzleTestInputPath(year, day, Some(2), Some(2))
  val testInputPathPart2Sub3: Path = FileUtils.getPuzzleTestInputPath(year, day, Some(2), Some(3))

  def testPart1Real(expectedValue: String): Unit = {
    testPartReal(expectedValue, 1)
  }

  def testPart1(expectedValue: String, inputPath: Option[Path] = None): Unit = {
    testPart(expectedValue, 1, inputPath)
  }

  def testPart2(expectedValue: String, inputPath: Option[Path] = None): Unit = {
    testPart(expectedValue, 2, inputPath)
  }

  def testPart2Real(expectedValue: String): Unit = {
    testPartReal(expectedValue, 2)
  }

  private def testPart(expectedValue: String, part: Int, inputPath: Option[Path]): Unit = {
    val (result, _) = PuzzleRunner
      .runPuzzlePart(puzzle, part, inputPath.getOrElse(FileUtils.getPuzzleTestInputPath(year, day)))
    Assert.assertEquals(expectedValue, result)
  }

  private def day: Int = puzzle.day

  private def year: Int = puzzle.year

  private def testPartReal(expectedValue: String, part: Int): Unit = {
    testPart(expectedValue, part, inputPath = Some(puzzle.inputPath))
  }
}
