package services

import model.{RegexRule, RuleMatch, TextBlock}
import utils.Matcher

import scala.concurrent.Future
import scala.util.matching.Regex.Match

class RegexMatcher(category: String, rules: List[RegexRule]) extends Matcher {
  def getId() = "regex-validator"

  override def check(request: MatcherRequest): Future[List[RuleMatch]] = {
    val matches = rules.flatMap { checkRule(request, _) }

    // If matches cover the same range, combine them
    val concatenatedMatches = matches.groupBy(m => (m.fromPos, m.toPos)).flatMap {
      case (_, matches) => combineMatches(matches)
    }.toList.sortBy(_.fromPos)

    Future.successful(concatenatedMatches)
  }

  override def getRules(): List[RegexRule] = rules

  override def getCategory(): String = category

  private def checkRule(request: MatcherRequest, rule: RegexRule): List[RuleMatch] = {
    request.blocks.flatMap { block =>
      rule.regex.findAllMatchIn(block.text).map { ruleMatchFromRule(_, rule, block) }
    }
  }

  private def ruleMatchFromRule(currentMatch: Match, rule: RegexRule, block: TextBlock) = RuleMatch(
    rules = List(rule),
    fromPos = currentMatch.start + block.from,
    toPos = currentMatch.end + block.from,
    matchedText = block.text.substring(currentMatch.start, currentMatch.end),
    message = rule.description,
    shortMessage = Some(rule.description),
    suggestions = rule.suggestions,
    markAsCorrect = rule.replacement.map(_.text).getOrElse("") == block.text.substring(currentMatch.start, currentMatch.end)
  )

  /**
    * Combine matches that cover the same range. Somewhat naive for now, in that we
    * * assume that certain qualities are common to rules which match the same ranges,
    * e.g. correctness.
    */
  private def combineMatches(matches: List[RuleMatch]): Option[RuleMatch] = matches match {
    case Nil => None
    case singleMatch :: Nil => Some(singleMatch)
    case matches =>
      val newMessage = s"_${matches.size} matches -- either_: ${matches.mkString(", _or_: ")}"
      Some(matches.head.copy(
        rules = matches.flatMap(_.rules),
        message = newMessage,
        shortMessage = Some(newMessage),
        suggestions = matches.flatMap(_.suggestions)
      ))
  }
}
