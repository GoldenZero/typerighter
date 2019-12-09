package model

import org.languagetool.rules.{RuleMatch => LTRuleMatch}
import play.api.libs.json.{Json, Writes}

import scala.collection.JavaConverters._

object RuleMatch {
  def fromLT(lt: LTRuleMatch, block: TextBlock): RuleMatch = {
    RuleMatch(
      rules = List(LTRule.fromLT(lt.getRule)),
      fromPos = lt.getFromPos,
      toPos = lt.getToPos,
      matchedText = block.text.substring(lt.getFromPos, lt.getToPos),
      message = lt.getMessage,
      shortMessage = Some(lt.getMessage),
      suggestions = lt.getSuggestedReplacements.asScala.toList.map { TextSuggestion(_) }
    )
  }

  implicit val writes: Writes[RuleMatch] = Writes[RuleMatch]((ruleMatch: RuleMatch) => Json.obj(
      "rules" -> ruleMatch.rules.map(BaseRule.toJson),
      "fromPos"-> ruleMatch.fromPos,
      "toPos" -> ruleMatch.toPos,
      "matchedText" -> ruleMatch.matchedText,
      "message" -> ruleMatch.message,
      "shortMessage" -> ruleMatch.shortMessage,
      "suggestions" -> ruleMatch.suggestions,
      "markAsCorrect" -> ruleMatch.markAsCorrect
    )
  )
}

case class RuleMatch(rules: List[BaseRule],
                     fromPos: Int,
                     toPos: Int,
                     matchedText: String,
                     message: String,
                     shortMessage: Option[String] = None,
                     suggestions: List[Suggestion] = List.empty,
                     markAsCorrect: Boolean = false)

