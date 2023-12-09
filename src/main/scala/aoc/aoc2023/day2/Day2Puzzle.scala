package aoc.aoc2023.day2

import aoc.DailyPuzzle


case object Day2Puzzle extends DailyPuzzle(2, "Cube Conundrum") {
  private val maxRed = 12
  private val maxGreen = 13
  private val maxBlue = 14

  override def calculatePart1(
    lines: Seq[String]): String = {

    val possibleGames = getPossibleGames(lines.map(getGameData))
    sumGameIds(possibleGames).toString
  }

  override def calculatePart2(
    lines: Seq[String]): String = {
    getMinimumCubeCounts(lines.map(getGameData)).map { case (red, green, blue) => red * green * blue }.sum.toString
  }

  private def getPossibleGames(gameDataSeq: Seq[GameData]): Seq[GameData] = gameDataSeq.filter(gameData => {
    gameData.cubeData.foldLeft(true)((previous, cubeData) => {
      val redResult = cubeData.red.forall(_ <= maxRed)
      val greenResult = cubeData.green.forall(_ <= maxGreen)
      val blueResult = cubeData.blue.forall(_ <= maxBlue)

      redResult && greenResult && blueResult && previous
    })
  })

  private def sumGameIds(gameDataSeq: Seq[GameData]): Int = gameDataSeq.map(_.id).sum

  private def getGameData(line: String): GameData = {
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
          CubeData(red, green, blue)
        }
        }

        GameData(gameId.toInt, cubeDataParts)
      case _                              => throw new IllegalArgumentException(s"Error in line processing: $line")
    }
  }

  private def getMinimumCubeCounts(gameDataSeq: Seq[GameData]): Seq[(Int, Int, Int)] = {
    gameDataSeq.map(gameData => {
      val minRed = gameData.cubeData.flatMap(_.red).max
      val minGreen = gameData.cubeData.flatMap(_.green).max
      val minBlue = gameData.cubeData.flatMap(_.blue).max
      (minRed, minGreen, minBlue)
    })
  }

  private case class CubeData(red: Option[Int], green: Option[Int], blue: Option[Int])

  private case class GameData(id: Int, cubeData: Seq[CubeData])
}
