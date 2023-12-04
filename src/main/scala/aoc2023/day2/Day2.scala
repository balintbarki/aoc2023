package aoc2023.day2

import aoc2023.DailyApp

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.CollectionHasAsScala

final case class Day2CubeData(red: Option[Int], green: Option[Int], blue: Option[Int])

final case class Day2GameData(id: Int, cubeData: Seq[Day2CubeData])

object Day2App {

  val inputPath = "src/main/scala/aoc2023/day2/input_Day2.txt"

  val maxRed = 12
  val maxGreen = 13
  val maxBlue = 14

  def getGameData(line: String): Day2GameData = {
    val topRegex = """Game (\d+):(.*)""".r
    line match {
      case topRegex(gameId, cubeDataPart) =>
        val cubeDataParts = cubeDataPart.split(";").toSeq.map { dataPart => {
          val redRegex = """(\d+) red""".r.unanchored
          val greenRegex = """(\d+) green""".r.unanchored
          val blueRegex = """(\d+) blue""".r.unanchored
          val red = dataPart match {
            case redRegex(redCnt) => Some(redCnt.toInt)
            case _                => None
          }
          val green = dataPart match {
            case greenRegex(greenCnt) => Some(greenCnt.toInt)
            case _                    => None
          }
          val blue = dataPart match {
            case blueRegex(blueCnt) => Some(blueCnt.toInt)
            case _                  => None
          }
          Day2CubeData(red, green, blue)
        }
        }

        Day2GameData(gameId.toInt, cubeDataParts)
      case _                              => throw new IllegalArgumentException(s"Error in line processing: $line")
    }
  }

  def getPossibleGames(gameDataSeq: Seq[Day2GameData]): Seq[Day2GameData] = gameDataSeq.filter(gameData => {
    gameData.cubeData.foldLeft(true)((previous, cubeData) => {
      val redResult = cubeData.red.forall(_ < maxRed)
      val greenResult = cubeData.green.forall(_ < maxGreen)
      val blueResult = cubeData.blue.forall(_ < maxBlue)

      redResult && greenResult && blueResult && previous
    })
  })

  def sumGameIds(gameDataSeq: Seq[Day2GameData]): Int = gameDataSeq.map(_.id).sum

  def getMinimumCubeCounts(gameDataSeq: Seq[Day2GameData]): Seq[(Int, Int, Int)] = {
    gameDataSeq.map(gameData => {
      val minRed = gameData.cubeData.flatMap(_.red).max
      val minGreen = gameData.cubeData.flatMap(_.green).max
      val minBlue = gameData.cubeData.flatMap(_.blue).max
      (minRed, minGreen, minBlue)
    })
  }
}

object Day2_Part1 extends DailyApp {

  override def calculate(inputPath: String): Int = {
    val gameData = Files.readAllLines(Paths.get(inputPath)).asScala.toSeq.map(Day2App.getGameData)
    val possibleGames = Day2App.getPossibleGames(gameData)
    Day2App.sumGameIds(possibleGames)
  }

  println(calculate(Day2App.inputPath))
}

object Day2_Part2 extends DailyApp {

  override def calculate(inputPath: String): Int = {
    val gameData = Files.readAllLines(Paths.get(inputPath)).asScala.toSeq.map(Day2App.getGameData)
    val minimumCubeCounts = Day2App.getMinimumCubeCounts(gameData)
    minimumCubeCounts.map { case (red, green, blue) => red * green * blue }.sum
  }

  println(calculate(Day2App.inputPath))
}
