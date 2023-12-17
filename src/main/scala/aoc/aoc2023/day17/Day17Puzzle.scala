package aoc.aoc2023.day17

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.{Direction, Matrix}

import scala.collection.mutable

case object Day17Puzzle extends DailyPuzzle2023(17, "Clumsy Crucible") {

  val heatLossOfNotVisited = Int.MaxValue
  val maxConsecutiveSteps = 3

  def findPath(
    startX: Int, startY: Int, targetX: Int, targetY: Int, blockMatrix: Matrix[Block],
    lastSteps: Seq[Direction], directions: Seq[Direction], jobQueue: mutable.Queue[() => Unit]): Unit = {

    def stepIfPossible(
      from: Block, dir: Direction, xCond: Boolean, yCond: Boolean, newX: Int, newY: Int): Unit = {
      if (xCond && yCond) {

        val stepTarget = blockMatrix.get(newX, newY)

        // Step is possible if there are less than the specified max consecutive steps into this direction
        // and the previous step was not the opposite of this direction
        if ((lastSteps.count(_ == dir) <= maxConsecutiveSteps) && !lastSteps.lastOption.contains(dir.opposite)) {

          //println(s"Trying to step $dir to ($newX, $newY)")

          val heatLossFromSource = from.minHeatLossWithStepsLeftTo.getOrElse(dir, ???)._1

          val allowedDirectionsWithStepsLeft = dir.getPerpendiculars.map((_, maxConsecutiveSteps)) ++ {
            val stepsAlreadyToDir = lastSteps.count(_ == dir)
            if (stepsAlreadyToDir == maxConsecutiveSteps)
              Seq()
            else
              Seq((dir, maxConsecutiveSteps - stepsAlreadyToDir))
          }

          val directionsToProceed = allowedDirectionsWithStepsLeft.filter { case (newDir, stepsLeft) =>
            val (prevHeatLossToDir, prevStepsLeft) = stepTarget.minHeatLossWithStepsLeftTo(newDir)
            val heatLoss = heatLossFromSource + stepTarget.heatLoss
            if (heatLoss < prevHeatLossToDir) {
              stepTarget.minHeatLossWithStepsLeftTo.update(newDir, (heatLoss, stepsLeft))
              true
            }
            else if ((heatLoss == prevHeatLossToDir) && (stepsLeft > prevStepsLeft)) {
              stepTarget.minHeatLossWithStepsLeftTo.update(newDir, (heatLoss, stepsLeft))
              true
            }
            else
              false
          }

          //printBlockMatrix(blockMatrix)

          directionsToProceed.foreach { case (newDir, _) =>
            val updatedLastSteps = if (lastSteps.forall(_ == newDir))
              lastSteps ++ Seq(newDir)
            else
              Seq(newDir)

            jobQueue
              .enqueue(() => findPath(newX, newY, targetX, targetY, blockMatrix, updatedLastSteps, Seq(newDir),
                jobQueue))
          }

        }
      }
    }

    val startBlock = blockMatrix.get(startX, startY)
    val targetBlock = blockMatrix.get(targetX, targetY)

    if (startBlock != targetBlock) {
      if (directions.contains(Direction.Up))
        stepIfPossible(
          startBlock, Direction.Up, xCond = true, yCond = 0 < startY, startX, startY - 1)
      if (directions.contains(Direction.Down))
        stepIfPossible(
          startBlock, Direction.Down, xCond = true, yCond = startY < blockMatrix.ySize - 1, startX,
          startY + 1)
      if (directions.contains(Direction.Left))
        stepIfPossible(
          startBlock, Direction.Left, xCond = 0 < startX, yCond = true, startX - 1, startY)
      if (directions.contains(Direction.Right))
        stepIfPossible(
          startBlock, Direction.Right, xCond = startX < blockMatrix.xSize - 1, yCond = true, startX + 1,
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
    startBlock.minHeatLossWithStepsLeftTo.update(Direction.Down, (0, maxConsecutiveSteps - 1))
    startBlock.minHeatLossWithStepsLeftTo.update(Direction.Right, (0, maxConsecutiveSteps - 1))

    val jobQueue: mutable.Queue[() => Unit] = mutable.Queue()

    jobQueue.enqueue(
      () => findPath(startX, startY, targetX, targetY, blockMatrix, Seq(), Seq(Direction.Down, Direction.Right),
        jobQueue))

    while (jobQueue.nonEmpty) {
      jobQueue.dequeue()()
    }

    //printBlockMatrix(blockMatrix)

    blockMatrix.get(targetX, targetY).minHeatLossWithStepsLeftTo.map { case (_, (value, _)) => value }.min.toString
  }

  override def calculatePart2(lines: Seq[String]): String = ???


  private def printBlockMatrix(blockMatrix: Matrix[Block]): Unit = {
    blockMatrix
      .map(item => s"${item.heatLoss} (${
        item.minHeatLossWithStepsLeftTo.map { case (key, value) => s"${key.toString.take(1)}:$value" }.mkString(",")
      })")
      .print(20)
    println()
  }

  private class Block(val heatLoss: Int) {
    // Minimum heat loss when going to a direction
    val minHeatLossWithStepsLeftTo: mutable.Map[Direction, (Int, Int)] = mutable.Map(
      Direction.Up -> (heatLossOfNotVisited, maxConsecutiveSteps),
      Direction.Down -> (heatLossOfNotVisited, maxConsecutiveSteps),
      Direction.Left -> (heatLossOfNotVisited, maxConsecutiveSteps),
      Direction.Right -> (heatLossOfNotVisited, maxConsecutiveSteps)
    )


  }
}
