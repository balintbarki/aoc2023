package aoc.utils

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.CollectionHasAsScala

object FileUtils {
  def fileToLines(filePath: String): Seq[String] = Files.readAllLines(Paths.get(filePath)).asScala.toSeq

  def getPuzzleInputPath(day: Int): String = s"src/main/scala/aoc/aoc2023/inputs/input_Day$day.txt"

  def getPuzzleTestInputPath(day: Int, partOpt: Option[Int] = None, subPartOpt: Option[Int] = None): String = {
    val partTag = partOpt.map(part => s"_Part$part").getOrElse("")
    val subPartTag = subPartOpt.map(part => s"_$part").getOrElse("")
    s"src/test/scala/aoctest/aoc2023test/testinputs/testInput_Day$day$partTag$subPartTag.txt"
  }
}
