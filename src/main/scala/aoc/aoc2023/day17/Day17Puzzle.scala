package aoc.aoc2023.day17

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.{Direction, ImmutableMatrix}

import scala.collection.mutable

case object Day17Puzzle extends DailyPuzzle2023(17, "Clumsy Crucible") {

  val heatLossOfNotVisited: Int = 99
  val maxConsecutiveSteps = 2

  def findPath(
    startX: Int, startY: Int, targetX: Int, targetY: Int, blockMatrix: ImmutableMatrix[Block],
    lastSteps: Seq[Direction], directions: Seq[Direction], jobQueue: mutable.Queue[() => Unit]): Unit = {

    def stepIfPossible(
      from: Block, dir: Direction, xCond: Boolean, yCond: Boolean, newX: Int, newY: Int): Unit = {
      if (xCond && yCond) {

        val stepTarget = blockMatrix.get(newX, newY)
        val lastSameStepsToDir = lastSteps.count(_ == dir)

        // Step is possible if there are less than the specified max consecutive steps into this direction
        // and the previous step was not the opposite of this direction
        if ((lastSameStepsToDir <= maxConsecutiveSteps) && !lastSteps.lastOption.contains(dir.opposite)) {

          //println(s"Trying to step $dir to ($newX, $newY)")

          // If the heat loss coming from the source is smaller than one of the current minimum heat losses per
          // direction per remaining steps, then update the minimum and trigger next steps
          val heatLossesFromSource = from.minHeatLossPerDirPerStepLeft.getOrElse(dir, ???)

          val allowedDirectionsWithStepsLeft = dir.getPerpendiculars
            .flatMap(pdir => (1 to maxConsecutiveSteps).map(stepLeft => (pdir, stepLeft))) ++ {
            if ((lastSameStepsToDir + 1) == maxConsecutiveSteps)
              Seq()
            else
              (1 to (maxConsecutiveSteps - lastSameStepsToDir - 1)).map(stepLeft => (dir, stepLeft))
          }

          val directionsToProceed = allowedDirectionsWithStepsLeft.filter { case (newDir, stepsLeft) =>

            val heatLoss = heatLossesFromSource(maxConsecutiveSteps - lastSameStepsToDir - 1) + stepTarget.heatLoss
            val prevMinHeatLossesForStepsLeftTo = stepTarget.minHeatLossPerDirPerStepLeft(newDir)
            if (heatLoss < prevMinHeatLossesForStepsLeftTo(stepsLeft - 1)) {
              val newMinHeatLosses = {
                if (newDir == dir) {
                  prevMinHeatLossesForStepsLeftTo.take(stepsLeft - 1) ++ Seq(
                    heatLoss) ++ prevMinHeatLossesForStepsLeftTo.drop(stepsLeft)
                }
                else {
                  Seq.fill(maxConsecutiveSteps)(heatLoss)
                }
              }
              stepTarget.minHeatLossPerDirPerStepLeft.update(newDir, newMinHeatLosses)
              true
            }
            else {
              false
            }
          }

          //printBlockMatrix(blockMatrix)

          directionsToProceed.map { case (newDir, _) => newDir }.distinct.foreach { newDir =>
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

  override def calculatePart1(lines: Seq[String]): Long = {
    val blockMatrix = ImmutableMatrix.fromStrings(lines).map(c => new Block(c.asDigit))
    val startX = 0
    val startY = 0
    val targetX = blockMatrix.columns.length - 1
    val targetY = blockMatrix.rows.length - 1
    val startBlock = blockMatrix.get(startX, startY)
    startBlock.minHeatLossPerDirPerStepLeft
      .update(Direction.Down,
        Seq.fill(maxConsecutiveSteps)(0))
    startBlock.minHeatLossPerDirPerStepLeft
      .update(Direction.Right,
        Seq.fill(maxConsecutiveSteps)(0))

    val jobQueue: mutable.Queue[() => Unit] = mutable.Queue()

    jobQueue.enqueue(
      () => findPath(startX, startY, targetX, targetY, blockMatrix, Seq(), Seq(Direction.Down, Direction.Right),
        jobQueue))

    while (jobQueue.nonEmpty) {
      jobQueue.dequeue()()
    }

    printBlockMatrix(blockMatrix)

    blockMatrix.get(targetX, targetY).minHeatLossPerDirPerStepLeft.map { case (_, values) => values.min }.min
  }

  override def calculatePart2(lines: Seq[String]): Long = ???


  private def printBlockMatrix(blockMatrix: ImmutableMatrix[Block]): Unit = {
    blockMatrix
      .map(item => s"${item.heatLoss} (${
        item.minHeatLossPerDirPerStepLeft.map { case (key, value) => s"${key.toString.take(1)}:${value.mkString(",")}" }
          .mkString(",")
      })")
      .print(35, separator = ';')
    println()
  }

  private class Block(val heatLoss: Int) {
    // Minimum heat loss when going to a direction for remaining steps 0 to 3 (0 is not supposed to change)
    val minHeatLossPerDirPerStepLeft: mutable.Map[Direction, Seq[Int]] = mutable.Map(
      Direction.Up -> Seq.fill(maxConsecutiveSteps)(heatLossOfNotVisited),
      Direction.Down -> Seq.fill(maxConsecutiveSteps)(heatLossOfNotVisited),
      Direction.Left -> Seq.fill(maxConsecutiveSteps)(heatLossOfNotVisited),
      Direction.Right -> Seq.fill(maxConsecutiveSteps)(heatLossOfNotVisited),
    )
  }
}
