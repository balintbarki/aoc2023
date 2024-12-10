package aoc.aoc2024.day9

import aoc.aoc2024.DailyPuzzle2024

case object Day9Puzzle extends DailyPuzzle2024(9, "Disk Fragmenter") {

  override def calculatePart1(lines: Seq[String]): Long = {
    val inputWithIndices = lines.head.map(c => c - '0').zipWithIndex
    val (fileLengthsWithIndices, gapLengthsWithIndices) = inputWithIndices
      .partition { case (_, index) => index % 2 == 0 }
    val totalFileLength = fileLengthsWithIndices.map { case (fileLength, _) => fileLength }.sum
    val fileLengthsWithIndicesAndIds = fileLengthsWithIndices
      .map { case (fileLength, index) =>
        require(fileLength > 0, "File length is 0, this is unexpected")
        require((index % 2) == 0)
        (fileLength, index, index / 2)
      }

    val fileContentsWithIndices = fileLengthsWithIndicesAndIds
      .map { case (fileLength, index, id) => (Seq.fill(fileLength)(id), index) }

    val fileContentsPackedReversed = fileContentsWithIndices
      .flatMap { case (content, _) => content }
      .reverse

    val (filledGapsWithIndices, _) = gapLengthsWithIndices
      .foldLeft((Seq.empty[(Seq[Int], Int)], fileContentsPackedReversed))
      { case ((gapContentsWithIndices, remainingPackedFileContent), (gapLength, gapIndex)) =>
        if (remainingPackedFileContent.length < gapLength)
          throw new IllegalArgumentException(s"Unexpected")
        (gapContentsWithIndices ++ Seq(
          (remainingPackedFileContent.take(gapLength), gapIndex)), remainingPackedFileContent.drop(gapLength))
      }

    val resultSeq = (fileContentsWithIndices ++ filledGapsWithIndices).sortBy { case (_, index) => index }
      .flatMap { case (content, _) => content }
      .take(totalFileLength)

    resultSeq.zipWithIndex.map { case (value, index) => value.toLong * index.toLong }.sum


  }

  override def calculatePart2(lines: Seq[String]): Long = ???

}
