package aoc

import aoc.utils.FileUtils

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object FolderStructureGenerator extends App {

  val maxDays: Int = 25

  def generate(year: Int, dayOpt: Option[Int] = None): Unit = {

    // Generate (if not exists):
    // - top level implementation package: "main/scala/aoc/aoc...."
    //    - for each day a subpackage
    //    - for each day an empty puzzle file
    //    - empty file for real input
    //
    // - top level test package: "test/scala/aoctest/aoc....test"
    //    - for each day a subpackage
    //    - for each day an empty test file
    //    - empty file for test input

    val srcRootPath = Paths.get(s"src/main/scala/aoc/aoc$year")
    val testRootPath = Paths.get(s"src/test/scala/aoctest/aoc${year}test")

    val yearSpecificPuzzleClassTemplatePath = srcRootPath.resolve(s"DailyPuzzle$year.scala")
    Files.createDirectories(yearSpecificPuzzleClassTemplatePath.getParent)
    Files.write(yearSpecificPuzzleClassTemplatePath, yearSpecificDailyPuzzleClassTemplateContent(year).getBytes)

    dayOpt.map(day => Seq(day)).getOrElse((1 to maxDays)).foreach(day => {
      val implDirPath = Files.createDirectories(srcRootPath.resolve(s"day$day"))
      val implementationFile = implDirPath.resolve(s"Day${day}Puzzle.scala")
      Files.write(implementationFile, dailyImplementationTemplateContent(year, day).getBytes(StandardCharsets.UTF_8))

      val inputPath = FileUtils.getPuzzleInputPath(year, day)
      Files.createDirectories(inputPath.getParent)
      Files.write(inputPath, "".getBytes)

      val testDirPath = Files.createDirectories(testRootPath.resolve(s"day$day"))
      val testFile = testDirPath.resolve(s"TestDay${day}.scala")
      Files.write(testFile, dailyTestTemplateContent(year, day).getBytes(StandardCharsets.UTF_8))

      val testInputPath = FileUtils.getPuzzleTestInputPath(year, day)
      Files.createDirectories(testInputPath.getParent)
      Files.write(testInputPath, "".getBytes)
    }
    )
  }

  private def yearSpecificDailyPuzzleClassTemplateContent(year: Int): String =
    s"""package aoc.aoc$year
       |
       |import aoc.DailyPuzzle
       |
       |abstract class DailyPuzzle${year}(override val day: Int, override val name: String) extends DailyPuzzle($year, day, name)
       |
       |""".stripMargin

  private def dailyImplementationTemplateContent(year: Int, day: Int): String =
    s"""package aoc.aoc$year.day$day
       |
       |import aoc.aoc${year}.DailyPuzzle$year
       |
       |case object Day${day}Puzzle extends DailyPuzzle$year($day, "unknown") {
       |  override def calculatePart1(lines: Seq[String]): String = ???
       |
       |  override def calculatePart2(lines: Seq[String]): String = ???
       |
       |}
       |
       |""".stripMargin

  private def dailyTestTemplateContent(year: Int, day: Int): String =
    s"""package aoctest.aoc${year}test.day$day
       |
       |import aoc.aoc${year}.day$day.Day${day}Puzzle
       |import aoctest.PuzzleTest
       |import org.junit.{Ignore, Test}
       |
       |@Ignore
       |class TestDay$day extends PuzzleTest(Day${day}Puzzle) {
       |
       |  @Test
       |  def testDay${day}Part1(): Unit = {
       |    testPart1("0")
       |  }
       |
       |  @Test
       |  def testDay${day}Part1real(): Unit = {
       |    testPart1Real("0")
       |  }
       |
       |  @Test
       |  def testDay${day}Part2(): Unit = {
       |    testPart2("0")
       |  }
       |
       |  @Test
       |  def testDay${day}Part2real(): Unit = {
       |    testPart2Real("0")
       |  }
       |}
       |
       |""".stripMargin

  generate(2024)
}
