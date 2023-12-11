package aoc.utils

import java.nio.file.{Path, Paths}

object FileUtils {

  def getPuzzleInputPath(year: Int, day: Int): Path = Paths.get(s"src/main/scala/aoc/aoc$year/inputs/input_Day$day.txt")

  def getPuzzleTestInputPath(
    year: Int, day: Int, partOpt: Option[Int] = None, subPartOpt: Option[Int] = None): Path = {
    val partTag = partOpt.map(part => s"_Part$part").getOrElse("")
    val subPartTag = subPartOpt.map(part => s"_$part").getOrElse("")
    Paths.get(s"src/test/scala/aoctest/aoc${year}test/testinputs/testInput_Day$day$partTag$subPartTag.txt")
  }
}
