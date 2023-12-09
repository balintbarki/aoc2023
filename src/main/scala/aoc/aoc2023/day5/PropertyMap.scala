package aoc.aoc2023.day5

import scala.collection.immutable

trait BoundaryType {
  def pseudoValue: Int
}

object BoundaryType {
  case object BoundaryStart extends BoundaryType {
    override def pseudoValue: Int = 1
  }

  case object BoundaryEnd extends BoundaryType {
    override def pseudoValue: Int = 0
  }
}

abstract class RangeBoundary(val value: Long, val boundaryType: BoundaryType) extends Ordered[RangeBoundary] {
  override def compare(that: RangeBoundary): Int = if (value == that.value) {
    boundaryType.pseudoValue.compare(that.boundaryType.pseudoValue)
  } else {
    value.compare(that.value)
  }
}

abstract class InputRangeBoundary(
  override val value: Long, override val boundaryType: BoundaryType) extends RangeBoundary(value, boundaryType)

abstract class ShifterRangeBoundary(
  override val value: Long, override val boundaryType: BoundaryType) extends RangeBoundary(value, boundaryType)

case class InputRangeStart(override val value: Long) extends InputRangeBoundary(value, BoundaryType.BoundaryStart)

case class InputRangeEnd(override val value: Long) extends InputRangeBoundary(value, BoundaryType.BoundaryEnd)

case class ShifterRangeStart(override val value: Long, shift: Long) extends ShifterRangeBoundary(value,
  BoundaryType.BoundaryStart)

case class ShifterRangeEnd(override val value: Long) extends ShifterRangeBoundary(value, BoundaryType.BoundaryEnd)

class PropertyMap(
  val rangeShiftDefinitions: Seq[RangeShiftDefinition]
) {
  def shift(range: Range): Range = Range(shift(range.start), shift(range.end))

  def shift(srcId: Long): Long = rangeShiftDefinitions
    .find(rangeShiftDef => rangeShiftDef.range.start until rangeShiftDef.range.end contains srcId)
    .map(rangeDef => srcId + rangeDef.shift).getOrElse(srcId)

  def processRanges(inputRanges: Seq[Range]): Seq[Range] = {
    /*
        // Create a sorted seq of range boundaries from the input and the shifter ranges
        val sortedBoundaries: Seq[(RangeBoundary, Boolean)] = inputRanges
          .flatMap(
            inputRange => Seq(InputRangeStart(inputRange.start), InputRangeEnd(inputRange.end))) ++ rangeShiftDefinitions
          .flatMap(rangeShiftDef => Seq(ShifterRangeStart(rangeShiftDef.range.start, rangeShiftDef.shift),
            ShifterRangeEnd(rangeShiftDef.range.end))).sorted


     */
    Seq()
  }

}
