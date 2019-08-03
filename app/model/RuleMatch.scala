package model

import org.languagetool.rules.{RuleMatch => LTRuleMatch}
import play.api.libs.json.{Json, Writes}
import org.jsoup.Jsoup
import org.jsoup.safety.Whitelist

import scala.collection.JavaConverters._

case class RuleMatch(rule: ResponseRule,
                     fromPos: Int,
                     toPos: Int,
                     message: String,
                     shortMessage: Option[String],
                     suggestedReplacements: List[String])

object RuleMatch {
  def fromLT(lt: LTRuleMatch): RuleMatch = {
    val doc = Jsoup.parse(lt.getMessage)
    doc.select("suggestion").remove()
    val message = doc.text()
    RuleMatch(
      rule = ResponseRule.fromLT(lt.getRule),
      fromPos = lt.getFromPos,
      toPos = lt.getToPos,
      message = message,
      shortMessage = Some(message),
      suggestedReplacements = lt.getSuggestedReplacements.asScala.toList
    )
  }

  implicit val writes: Writes[RuleMatch] = Json.writes[RuleMatch]
}