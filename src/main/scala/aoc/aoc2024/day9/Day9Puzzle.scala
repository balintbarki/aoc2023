package aoc.aoc2024.day9

import aoc.aoc2024.DailyPuzzle2024

import scala.collection.mutable.ListBuffer

case object Day9Puzzle extends DailyPuzzle2024(9, "Disk Fragmenter") {

  override def calculatePart1(lines: Seq[String]): Long = {
    val inputWithIndices = lines.head.map(c => c - '0').zipWithIndex
    val (fileLengthsWithIndices, gapLengthsWithIndices) = inputWithIndices
      .partition { case (_, index) => index % 2 == 0 }
    val fileLengthsWithIndicesAndIds = fileLengthsWithIndices
      .map { case (fileLength, index) =>
        require(fileLength > 0, "File length is 0, this is unexpected")
        require((index % 2) == 0)
        (fileLength, index, index / 2)
      }

    val fileContentsWithIndices = fileLengthsWithIndicesAndIds
      .map { case (fileLength, index, id) => (Seq.fill(fileLength)(id), index) }

    val totalFileLength = fileLengthsWithIndices.map { case (fileLength, _) => fileLength }.sum

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

  override def calculatePart2(lines: Seq[String]): Long = {

    val inputWithIndices = lines.head.map(c => c - '0').zipWithIndex

    val items = ListBuffer.from(inputWithIndices.map
    { case (length, index) => if (index % 2 == 0) new File(length = length, id = index / 2) else new Gap(length) })

    val allFiles = items.filter(_.isInstanceOf[File]).map(_.asInstanceOf[File])

    var cnt = 0
    allFiles.reverse.map { file =>
      println(s"Processing $cnt out of ${allFiles.size}, total items: ${items.size}")
      cnt += 1
      val fileIndex = items.indexOf(file)
      val firstSuitableGapOpt = items.filter(_.isInstanceOf[Gap])
        .find(gap => (gap.length >= file.length) && (items.indexOf(gap) < fileIndex))
      firstSuitableGapOpt.map { gap =>
        val gapIndex = items.indexOf(gap)

        //println(s"Removing ${gap.length} long gap at $gapIndex")
        items.remove(gapIndex)
        //println(s"Inserting file ${file.id} at $gapIndex")
        items.insert(gapIndex, file)

        //println(s"Removing file at $fileIndex")
        items.remove(fileIndex)
        //println(s"Inserting ${file.length} long gap at $fileIndex")
        items.insert(fileIndex, new Gap(file.length))

        if (gap.length > file.length) {
          //println(s"Inserting ${gap.length - file.length} long gap at $gapIndex + 1")
          items.insert(gapIndex + 1, new Gap(gap.length - file.length))
        }

        //println()
      }
    }

    /*
    println(items.map {
      case f: File => Seq.fill(f.length)(f.id.toString).mkString
      case g: Gap  => Seq.fill(g.length)(".").mkString
    }.mkString)

     */

    val result: Long = items.flatMap {
      case f: File => Seq.fill(f.length)(f.id.toLong)
      case g: Gap  => Seq.fill(g.length)(0.toLong)
    }.zipWithIndex.map { case (id, index) => id * index }.sum

    result

  }

  private abstract class Item(val length: Int) {
  }

  private class File(override val length: Int, val id: Int) extends Item(length) {

  }

  private class Gap(override val length: Int) extends Item(length)

}
