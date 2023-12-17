package aoc.aoc2023.day17

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.{Direction, Matrix}

import scala.collection.mutable

case object Day17Puzzle extends DailyPuzzle2023(17, "Clumsy Crucible") {

  val heatLossOfNotVisited = Int.MaxValue
  val maxConsecutiveSteps = 3

  def findPath(
    startX: Int, startY: Int, targetX: Int, targetY: Int, blockMatrix: Matrix[Block],
    lastSteps: Seq[Direction], heatLossSoFar: Int, directions: Seq[Direction],
    jobQueue: mutable.Queue[() => Unit]): Unit = {

    def stepIfPossible(
      from: Block, dir: Direction, heatLossSoFar: Int, xCond: Boolean, yCond: Boolean, newX: Int, newY: Int): Unit = {
      if (xCond && yCond) {

        val stepTarget = blockMatrix.get(newX, newY)

        // Step is possible if there are less than the specified max consecutive steps into this direction
        // and the previous step was not the opposite of this direction
        if ((lastSteps.count(_ == dir) < maxConsecutiveSteps) && !lastSteps.lastOption.contains(dir.opposite)) {

          //println(s"Trying to step $dir to ($newX, $newY)")

          val prevHeatLossFromDir = stepTarget.minHeatLossFrom(dir.opposite)
          val heatLoss = heatLossSoFar + stepTarget.heatLoss
          if (heatLoss < prevHeatLossFromDir) {

            stepTarget.minHeatLossFrom.update(dir.opposite, heatLoss)
            val allowedDirections = dir.getPerpendiculars ++ {
              if (lastSteps.count(_ == dir) == (maxConsecutiveSteps - 1))
                Seq()
              else
                Seq(dir)
            }

            val updatedLastSteps = if (lastSteps.forall(_ == dir))
              lastSteps ++ Seq(dir)
            else
              Seq(dir)

            //println(s"Heat loss ($heatLoss) is smaller than previous, retrigger steps to $allowedDirections")

            jobQueue
              .enqueue(() => findPath(newX, newY, targetX, targetY, blockMatrix, updatedLastSteps, heatLoss,
                allowedDirections, jobQueue))
          }
        }
      }
    }

    val startBlock = blockMatrix.get(startX, startY)
    val targetBlock = blockMatrix.get(targetX, targetY)

    if (startBlock != targetBlock) {
      if (directions.contains(Direction.Up))
        stepIfPossible(
          startBlock, Direction.Up, heatLossSoFar, xCond = true, yCond = 0 < startY, startX, startY - 1)
      if (directions.contains(Direction.Down))
        stepIfPossible(
          startBlock, Direction.Down, heatLossSoFar, xCond = true, yCond = startY < blockMatrix.ySize - 1, startX,
          startY + 1)
      if (directions.contains(Direction.Left))
        stepIfPossible(
          startBlock, Direction.Left, heatLossSoFar, xCond = 0 < startX, yCond = true, startX - 1, startY)
      if (directions.contains(Direction.Right))
        stepIfPossible(
          startBlock, Direction.Right, heatLossSoFar, xCond = startX < blockMatrix.xSize - 1, yCond = true, startX + 1,
          startY)
    }
  }

  override def calculatePart1(lines: Seq[String]): String = {
    val blockMatrix = Matrix.fromStrings(lines).map(c => new Block(c.asDigit))
    val startX = 0
    val startY = 0
    val targetX = blockMatrix.columns.length - 1
    val targetY = blockMatrix.rows.length - 1
    val startBlock = blockMatrix.get(startX, startY)

    val jobQueue: mutable.Queue[() => Unit] = mutable.Queue()

    jobQueue.enqueue(
      () => findPath(startX, startY, targetX, targetY, blockMatrix, Seq(), 0, Seq(Direction.Down, Direction.Right),
        jobQueue))

    while (jobQueue.nonEmpty) {
      jobQueue.dequeue()()
    }
    blockMatrix
      .map(item => s"${item.heatLoss} (${
        item.minHeatLossFrom.map { case (key, value) => s"${key.toString.take(1)}:$value" }.mkString(",")
      })")
      .print(20)
    println()
    blockMatrix.get(targetX, targetY).minHeatLossFrom.map { case (_, value) => value }.min.toString
  }

  override def calculatePart2(lines: Seq[String]): String = ???

  private class Block(val heatLoss: Int) {
    // Minimum heat loss when coming from a direction
    val minHeatLossFrom: mutable.Map[Direction, Int] = mutable.Map(
      Direction.Up -> heatLossOfNotVisited,
      Direction.Down -> heatLossOfNotVisited,
      Direction.Left -> heatLossOfNotVisited,
      Direction.Right -> heatLossOfNotVisited
    )
  }
}
