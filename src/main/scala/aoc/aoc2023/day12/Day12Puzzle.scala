package aoc.aoc2023.day12

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.Combinatory

import scala.util.matching.Regex

case object Day12Puzzle extends DailyPuzzle2023(12, "Hot Springs") {

  val operationalCode = "O"
  val damagedCode = "X"
  val unknownCode = "_"

  override def calculatePart1(lines: Seq[String]): String = calculateUnfolded(lines, 1)

  override def calculatePart2(lines: Seq[String]): String = ??? //calculateUnfolded(lines, 5)


  private def getReducedInputs(lines: Seq[String], unfoldCnt: Int): Seq[(String, String, Seq[Int], Seq[Int])] =
    lines.map(line => {
      val parts = line.split(" ")
      val originalRecord = Seq.fill(unfoldCnt)(parts.head.trim).mkString("?")
      val originalGroups = Seq.fill(unfoldCnt)(parts.last).mkString(",")
        .trim.split(",")
        .map(_.toInt).toSeq
      val initiallyReduced = (Seq.fill(unfoldCnt)(parts.head.trim).mkString("?")
        .replaceAll("\\.+", operationalCode) // replace multiple operational springs with one
        .replaceAll("#", damagedCode)
        .replaceAll("\\?", unknownCode),
        originalGroups)

      var changed = true
      var (workingRecord, workingGroups) = initiallyReduced

      while (changed && workingGroups.nonEmpty) {
        var reducedRecord = workingRecord
        var reducedGroups = workingGroups

        reducedRecord = reducedRecord
          .replaceAll(s"$operationalCode+", operationalCode) // replace multiple operational springs with one
          .replaceAll(s"^$operationalCode", "") // replace starting operational spring with empty
          .replaceAll(s"$operationalCode$$", "") // replace ending operational spring with empty

        val head = reducedGroups.head
        val last = reducedGroups.last
        val leftExactMatchRegex =
          s"""^$damagedCode[$damagedCode$unknownCode]{${head - 1}}[$operationalCode$unknownCode].*"""
            .r
        val rightExactMatchRegex =
          s""".*[$operationalCode$unknownCode][$damagedCode$unknownCode]{${last - 1}}$damagedCode$$""".r
        val countedUnknownRegex = s"""^$unknownCode{$head}$$""".r
        val allUnknownRegex = s"""^$unknownCode+$$""".r
        val potentialMatchRegex = s"""[$damagedCode$unknownCode]{$head}""".r

        reducedRecord match {
          case leftExactMatchRegex()                              =>
            reducedRecord = reducedRecord.drop(reducedGroups.head + 1)
            reducedGroups = reducedGroups.drop(1)
          case rightExactMatchRegex()                             =>
            reducedRecord = reducedRecord.dropRight(reducedGroups.last + 1)
            reducedGroups = reducedGroups.dropRight(1)
          case countedUnknownRegex() if reducedGroups.length == 1 =>
            reducedRecord = reducedRecord.drop(reducedGroups.head)
            reducedGroups = reducedGroups.drop(1)
          case allUnknownRegex() if reducedGroups.isEmpty         =>
            reducedRecord = ""
          case _                                                  =>
            // Find the first appearance of the potential match of the first element in the group and remove everything
            // before it
            potentialMatchRegex.findFirstMatchIn(reducedRecord)
              .foreach { matcher => reducedRecord = reducedRecord.drop(matcher.start) }
        }

        // Create the intersection of two cases: all groups are aligned to the left, all groups are aligned to the right
        // where the intersection contains X, it can be written to the record
        val denseGroups = reducedGroups.foldLeft("")((s, i) => s + s"$damagedCode" * i + s"$operationalCode")


        val leftAligned: String = (denseGroups + s"$operationalCode" * reducedRecord.length)
          .take(reducedRecord.length)
        val rightAligned: String = (s"$operationalCode" * reducedRecord.length + denseGroups.dropRight(1))
          .takeRight(reducedRecord.length)
        //println(s"LeftAligned: $leftAligned, rightAligned: $rightAligned")



        changed = (reducedRecord != workingRecord) || (reducedGroups != workingGroups)

        if (changed) {
          // println(s"$workingRecord -> $reducedRecord, $workingGroups -> $reducedGroups")
          workingRecord = reducedRecord
          workingGroups = reducedGroups
        }
      }

      if (workingRecord.isEmpty) {
        if (workingGroups.nonEmpty)
          require(workingGroups.isEmpty)
      }

      (originalRecord, workingRecord, originalGroups, workingGroups)
    })

  private def calculateUnfolded(lines: Seq[String], unfoldCnt: Int): String = {
    val reducedInputs = getReducedInputs(lines, unfoldCnt)

    // Reduce operationals to 1 -> reduced input
    // Generate group strings
    // Determine number of extra operationals
    // Generate all possible combination and check if it matches with reduced input
    reducedInputs.map { case (originalRecord, record, originalGroups, groups) =>
      println(
        s"OriginalRecord: $originalRecord, record: $record, originalGroups: $originalGroups, groups: $groups")
      val cnt = if (groups.nonEmpty) {
        val recordPattern = new Regex(record.replaceAll(s"$unknownCode", "."))
        val minLength = groups.sum + groups.length - 1
        val recordLength = record.length
        val extraOperationalCnt: Int = recordLength - minLength
        val locationsForOperational = groups.length + 1

        // Determine each combination where "extraOperationalCnt" can be distributed to "locationsForOperation" locations
        val combinations = if (extraOperationalCnt > 0)
          Combinatory.combinations(extraOperationalCnt, locationsForOperational, 0)
        else
          List(List.fill(locationsForOperational)(0))

        combinations
          .flatMap(combination => {
            println(s"Calculating permutations for $combination")
            Combinatory.permutations(combination).map(
              permutation => {
                val adjustedPermutation = Seq(permutation.head) ++ permutation.tail.dropRight(1).map(_ + 1) ++ Seq(
                  permutation.last)

                val zipped = adjustedPermutation.map(operationalCode * _)
                  .zipAll(groups.map(damagedCode * _), "", "").foldLeft("") { case (prev, (first, second)) =>
                  prev + first + second
                }
                if (recordPattern.matches(zipped)) 1 else 0
              })
          }).sum
        /*
                val adjustedCombinations = permutations
                  .map(
                    combination => Seq(combination.head) ++ combination.tail.dropRight(1).map(_ + 1) ++ Seq(combination.last))
        */
        // Check for each combination if they can match with the reduced input
        /*
        val results = permutations.map(combination => {
          combination.map(operationalCode * _)
            .zipAll(groups.map(damagedCode * _), "", "")
        })
          .map(next => next.foldLeft("") { case (prev, (first, second)) =>
            prev + first + second
          })

        results.count(result => {
          recordPattern.matches(result)
        })

         */
      }
      else {
        1
      }

      //println(s"Record: $originalRecord, groups: $originalGroups, cnt: $cnt")

      cnt
    }.sum.toString
  }
}
