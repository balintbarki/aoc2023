package aoc.aoc2023.day5

import aoc.utils

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

  // This is used so that during sorting boundaries with the same type and value, the order is deterministic
  def priority: Int

  override def compare(that: RangeBoundary): Int = if (value == that.value) {
    if (boundaryType.pseudoValue == that.boundaryType.pseudoValue)
      priority.compare(that.priority)
    else
      boundaryType.pseudoValue.compare(that.boundaryType.pseudoValue)
  } else {
    value.compare(that.value)
  }
}

abstract class InputRangeBoundary(
  override val value: Long, override val boundaryType: BoundaryType) extends RangeBoundary(value, boundaryType) {
  override def priority = 0
}

abstract class ShifterRangeBoundary(
  override val value: Long, override val boundaryType: BoundaryType) extends RangeBoundary(value, boundaryType) {
  override def priority = 1
}

case class InputRangeStart(override val value: Long) extends InputRangeBoundary(value, BoundaryType.BoundaryStart)

case class InputRangeEnd(override val value: Long) extends InputRangeBoundary(value, BoundaryType.BoundaryEnd)

case class ShifterRangeStart(override val value: Long, shift: Long) extends ShifterRangeBoundary(value,
  BoundaryType.BoundaryStart)

case class ShifterRangeEnd(override val value: Long, shift: Long) extends ShifterRangeBoundary(value,
  BoundaryType.BoundaryEnd)

class PropertyMap(
  val rangeShiftDefinitions: Seq[RangeShiftDefinition]
) {
  def shift(range: utils.Range): utils.Range = utils.Range(shift(range.start), shift(range.end))

  def shift(srcId: Long): Long = rangeShiftDefinitions
    .find(rangeShiftDef => rangeShiftDef.range.start until rangeShiftDef.range.end contains srcId)
    .map(rangeDef => srcId + rangeDef.shift).getOrElse(srcId)

  def processRanges(inputRanges: Seq[utils.Range]): Seq[utils.Range] = {

    // Create a sorted seq of range boundaries from the input and the shifter ranges
    val inputBoundaries = inputRanges
      .flatMap(inputRange => Seq(InputRangeStart(inputRange.start), InputRangeEnd(inputRange.end)))

    val shifterBoundaries = rangeShiftDefinitions.flatMap(rangeShiftDefinition => Seq(
      ShifterRangeStart(rangeShiftDefinition.range.start, rangeShiftDefinition.shift),
      ShifterRangeEnd(rangeShiftDefinition.range.end, rangeShiftDefinition.shift)))

    val sortedBoundaries = (inputBoundaries ++ shifterBoundaries).sorted

    val (intersectedRangeOpts, _, _) = sortedBoundaries.sliding(2)
      .foldLeft((Seq[Option[RangeShiftDefinition]](), false, 0L))
      { case ((acc, toInclude, currentShift), Seq(first, second)) =>
        val (nextRangeShiftDefinitionOpt: Option[RangeShiftDefinition], nextToInclude: Boolean, nextShift: Long) = (first, second) match {
          case (InputRangeStart(start), ShifterRangeStart(end, shift)) if start == end =>
            (None, true, shift)
          case (InputRangeStart(start), ShifterRangeStart(end, shift))                 =>
            (Some(RangeShiftDefinition(utils.Range(start, end), currentShift)), true, shift)
          case (InputRangeStart(start), ShifterRangeStart(end, shift))                 =>
            (Some(RangeShiftDefinition(utils.Range(start, end), currentShift)), true, shift)
          case (InputRangeStart(start), ShifterRangeEnd(end, _))                       =>
            (Some(RangeShiftDefinition(utils.Range(start, end), currentShift)), true, 0L)
          case (InputRangeStart(start), InputRangeEnd(end))                            =>
            (Some(RangeShiftDefinition(utils.Range(start, end), currentShift)), false, currentShift)
          case (InputRangeEnd(_), InputRangeStart(_))                                  =>
            (None, true, currentShift)
          case (InputRangeEnd(_), ShifterRangeEnd(_, _))                               =>
            (None, false, 0L)
          case (InputRangeEnd(_), ShifterRangeStart(_, shift))                         =>
            (None, false, shift)
          case (ShifterRangeStart(_, shift), InputRangeStart(_))                       =>
            (None, true, shift)
          case (ShifterRangeStart(start, shift), InputRangeEnd(end))                   =>
            (Some(RangeShiftDefinition(utils.Range(start, end), shift)), false, shift)
          case (ShifterRangeStart(start, shift), ShifterRangeEnd(end, _)) if toInclude =>
            (Some(RangeShiftDefinition(utils.Range(start, end), shift)), true, 0L)
          case (ShifterRangeStart(_, _), ShifterRangeEnd(_, _)) if !toInclude          =>
            (None, false, 0L)
          case (ShifterRangeEnd(_, _), InputRangeStart(_))                             =>
            (None, true, 0L)
          case (ShifterRangeEnd(_, _), ShifterRangeStart(_, shift))                    =>
            (None, toInclude, shift)
          case (ShifterRangeEnd(start, _), InputRangeEnd(end))                         =>
            (Some(RangeShiftDefinition(utils.Range(start, end), 0)), false, 0L)
        }

        (acc ++ Seq(nextRangeShiftDefinitionOpt), nextToInclude, nextShift)
      }

    val intersectedRanges = intersectedRangeOpts.flatten.sorted
    val shiftedRanges = intersectedRanges.map(rangeShiftDefinition => rangeShiftDefinition.shiftRange).sorted
    shiftedRanges
  }

}
