package aoc.aoc2023.day19

import aoc.aoc2023.DailyPuzzle2023
import aoc.utils.ImplicitUtils.AddMultispanToList

import scala.collection.mutable
import scala.util.matching.Regex

case object Day19Puzzle extends DailyPuzzle2023(19, "Aplenty") {

  private val ruleMap: mutable.Map[String, Part => Boolean] = mutable.Map()

  override def calculatePart1(
    lines: Seq[String]): String = {
    val (rules, parts) = lines.toList.multiSpanWithoutDelimiter(_.isBlank) match {
      case List(first, second) => (first.map(parseRule), second.map(parsePart))
    }

    parts.filter(part => evaluateRule("in", part)).map(part => part.a + part.m + part.s + part.x).sum.toString
  }

  override def calculatePart2(
    lines: Seq[String]): String = ???

  private def evaluateRule(rule: String, part: Part): Boolean = {
    //println(s"Evaluating rule $rule")
    ruleMap.getOrElse(rule, throw new IllegalArgumentException(s"Rule $rule is not found in rule map"))(part)
  }

  private def parseRule(s: String): Part => Boolean = {

    def evaluateCondition(
      condition: String, property: String, relation: String, limitStr: String, targetIfTrue: String,
      part: Part): Option[Boolean] = {

      val ruleToExecute: Part => Boolean = targetIfTrue match {
        case "A"    => _ => true
        case "R"    => _ => false
        case ruleId => part => evaluateRule(ruleId, part)
      }

      if (condition != null) {
        val valueToCheck = property match {
          case "a" => part.a
          case "m" => part.m
          case "s" => part.s
          case "x" => part.x
        }

        val limit = limitStr.toLong

        val conditionResult = relation match {
          case "<" => valueToCheck < limit
          case ">" => valueToCheck > limit
        }

        if (conditionResult)
          Some(ruleToExecute(part))
        else
          None

      } else {
        Some(ruleToExecute(part))
      }
    }


    val ruleTopRegex = """(\w+)\{(.+)\}""".r
    val conditionRegex = new Regex("""(([amsx])([<>])(\d+):)?([AR]|\w+)""", "condition", "property", "relation",
      "limit",
      "targetIfTrue")
    s match {
      case ruleTopRegex(id, subRuleList) =>
        val subRules = subRuleList.split(",").toSeq
        val conditionEvaluators = subRules.map { subRule =>
          val matcher = conditionRegex.findFirstMatchIn(subRule).getOrElse(???)
          val condition = matcher.group("condition")
          val property = matcher.group("property")
          val relation = matcher.group("relation")
          val limit = matcher.group("limit")
          val targetIfTrue = matcher.group("targetIfTrue")

          part: Part => evaluateCondition(condition, property, relation, limit, targetIfTrue, part)
        }

        val result = (part: Part) =>
          conditionEvaluators.iterator
            .foldLeft[Option[Boolean]](None)((prev, evaluator) => if (prev.isEmpty) evaluator(part) else prev)
            .getOrElse(throw new IllegalArgumentException("None of the conditions returned a result"))

        ruleMap.update(id, result)
        result
    }
  }

  private def parsePart(s: String): Part = {
    val partRegex = """\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}""".r
    s match {
      case partRegex(x, m, a, s) => Part(x.toLong, m.toLong, a.toLong, s.toLong)
    }
  }

  private case class Part(x: Long, m: Long, a: Long, s: Long)
}
