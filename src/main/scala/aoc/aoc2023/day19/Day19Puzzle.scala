package aoc.aoc2023.day19

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.ImplicitUtils.AddMultispanToList

import aoc.utils.Range
import scala.collection.mutable
import scala.util.matching.Regex

case object Day19Puzzle extends DailyPuzzle2023(19, "Aplenty") {

  // Initial default rules for accept and reject
  private val ruleMap: mutable.Map[String, Rule] = mutable.Map(
    "A" -> Rule("A", Condition((partRange: PartRange) => (Some(partRange), None)), None, None),
    "R" -> Rule("R", Condition((partRange: PartRange) => (None, Some(partRange))), None, None)
  )

  override def calculatePart1(
    lines: Seq[String]): String = {
    val (_, parts) = lines.toList.multiSpanWithoutDelimiter(_.isBlank) match {
      case List(first, second) => (first.map(parseRule), second.map(parsePart))
    }

    parts.flatMap(part => evaluateRule("in", part) match {
      case (acceptedRanges, _) => acceptedRanges.map(part => part.x.start + part.m.start + part.a.start + part.s.start)
    }).sum.toString
  }

  override def calculatePart2(
    lines: Seq[String]): String = {

    lines.toList.multiSpanWithoutDelimiter(_.isBlank) match {
      case List(first, second) => (first.map(parseRule), second.map(parsePart))
    }

    val partRange = PartRange(x = Range(1, 4001), m = Range(1, 4001), a = Range(1, 4001), s = Range(1, 4001))

    evaluateRule("in", partRange) match {
      case (acceptedRanges, _) => acceptedRanges
        .map(partRange => partRange.x.length * partRange.m.length * partRange.a.length * partRange.s.length).sum
        .toString
    }
  }

  private def evaluateRule(rule: String, part: PartRange): (Seq[PartRange], Seq[PartRange]) = {
    //println(s"Evaluating rule $rule")
    ruleMap.getOrElse(rule, throw new IllegalArgumentException(s"Rule $rule is not found in rule map")).evaluate(part)
  }

  private def parseRule(s: String): Unit = {

    // Evaluates a condition for a given part range and returns two part range options: the first that satisfy the
    // condition, the second which doesn't
    def createCondition(
      condition: String, property: String, relation: String, limitStr: String): Condition = {

      if (condition != null) {
        Condition((partRange: PartRange) => {
          val valueToCheck = property match {
            case "x" => partRange.x
            case "m" => partRange.m
            case "a" => partRange.a
            case "s" => partRange.s
          }

          val limit = limitStr.toLong

          val (firstRangeOpt, secondRangeOpt) = relation match {
            case "<" => valueToCheck.splitAt(limit)
            case ">" => valueToCheck.splitAt(limit + 1).swap
          }

          val (resultFirstPartOpt, resultSecondPartOpt): (Option[PartRange], Option[PartRange]) = {
            property match {
              case "x" => (firstRangeOpt.map(firstRange => partRange.copy(x = firstRange)), secondRangeOpt
                .map(secondRange => partRange.copy(x = secondRange)))
              case "m" => (firstRangeOpt.map(firstRange => partRange.copy(m = firstRange)), secondRangeOpt
                .map(secondRange => partRange.copy(m = secondRange)))
              case "a" => (firstRangeOpt.map(firstRange => partRange.copy(a = firstRange)), secondRangeOpt
                .map(secondRange => partRange.copy(a = secondRange)))
              case "s" => (firstRangeOpt.map(firstRange => partRange.copy(s = firstRange)), secondRangeOpt
                .map(secondRange => partRange.copy(s = secondRange)))
            }
          }
          (resultFirstPartOpt, resultSecondPartOpt)
        })
      } else {
        Condition((partRange: PartRange) => (Some(partRange), None))
      }
    }

    val ruleTopRegex = """(\w+)\{(.+)}""".r
    val conditionRegex = new Regex("""(([amsx])([<>])(\d+):)?([AR]|\w+)""",
      "condition", "property", "relation", "limit", "targetIfTrue")
    s match {
      case ruleTopRegex(id, subRuleList) =>
        val subRules = subRuleList.split(",").toSeq.zipWithIndex


        val rules: Seq[Rule] = subRules.map { case (subRule, index) =>
          val matcher = conditionRegex.findFirstMatchIn(subRule).getOrElse(???)
          val conditionStr = matcher.group("condition")
          val property = matcher.group("property")
          val relation = matcher.group("relation")
          val limit = matcher.group("limit")
          val ruleIfTrue = matcher.group("targetIfTrue")

          val condition = createCondition(conditionStr, property, relation, limit)
          val ruleId = if (index == 0)
            id
          else
            s"${id}_$index"

          val ruleIfFalse = if (index == subRules.indices.last)
            None
          else
            Some(s"${id}_${index + 1}")

          val rule = Rule(ruleId, condition, Some(ruleIfTrue), ruleIfFalse)

          ruleMap.update(ruleId, rule)

          rule
        }
    }
  }

  private def parsePart(s: String): PartRange = {
    val partRegex = """\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}""".r
    s match {
      case partRegex(x, m, a, s) =>
        val xLong = x.toLong
        val mLong = m.toLong
        val aLong = a.toLong
        val sLong = s.toLong
        PartRange(Range(xLong, xLong + 1), Range(mLong, mLong + 1), Range(aLong, aLong + 1), Range(sLong, sLong + 1))
    }
  }

  private case class PartRange(x: Range, m: Range, a: Range, s: Range)

  private case class Condition(condition: PartRange => (Option[PartRange], Option[PartRange]))

  private case class Rule(
    id: String, condition: Condition, ruleIdForTrueOpt: Option[String], ruleIdForFalseOpt: Option[String]) {
    def evaluate(partRange: PartRange): (Seq[PartRange], Seq[PartRange]) = {

      val (acceptRangeOpt, rejectRangeOpt) = condition.condition(partRange)

      val (acceptRangesFromTrue: Seq[PartRange], rejectRangesFromTrue: Seq[PartRange]) = (acceptRangeOpt, ruleIdForTrueOpt) match {
        case (Some(acceptRange), Some(ruleIdForTrue)) => evaluateRule(ruleIdForTrue, acceptRange)
        case _                                        => (acceptRangeOpt.map(Seq(_)).getOrElse(Seq()), Seq())
      }

      val (acceptRangesFromFalse, rejectRangesFromFalse) = (rejectRangeOpt, ruleIdForFalseOpt) match {
        case (Some(rejectRange), Some(ruleIdForFalse)) => evaluateRule(ruleIdForFalse, rejectRange)
        case _                                         => (Seq(), rejectRangeOpt.map(Seq(_)).getOrElse(Seq()))
      }

      (acceptRangesFromTrue ++ acceptRangesFromFalse, rejectRangesFromTrue ++ rejectRangesFromFalse)
    }
  }
}
