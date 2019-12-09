package services

import model.{Category, RegexRule, RuleMatch, TextBlock, TextSuggestion}
import org.scalatest._

class RegexMatcherTest extends AsyncFlatSpec with Matchers {
  val rules = List(RegexRule(
    id = "example-rule-1",
    category = Category("new-category", "New Category", "puce"),
    description = "An example rule",
    suggestions = List(TextSuggestion("other text")),
    regex = "text".r
  ), RegexRule(
    id = "example-rule-2-a",
    category = Category("new-category", "New Category", "puce"),
    description = "An example rule 2a",
    suggestions = List(TextSuggestion("other text")),
    regex = "two rules".r
  ), RegexRule(
    id = "example-rule-2-b",
    category = Category("new-category", "New Category", "puce"),
    description = "An example rule 2b",
    suggestions = List(TextSuggestion("other text")),
    regex = "two rules".r
  ))

  val regexValidator = new RegexMatcher("example-category", rules)

  def getBlocks(text: String) = List(TextBlock("text-block-id", text, 0, text.length))

  def getMatch(text: String, fromPos: Int, toPos: Int, ruleIndex: Int = 0) = RuleMatch(
    rules = List(rules(ruleIndex)),
    fromPos = fromPos,
    toPos = toPos,
    matchedText = text,
    message = "An example rule",
    shortMessage = Some("An example rule"),
    suggestions = List(TextSuggestion("other text"))
  )

  "check" should "report single matches" in {
    val eventuallyMatches = regexValidator.check(
      MatcherRequest(getBlocks("example text"), "example-category")
    )
    eventuallyMatches.map { matches =>
      matches shouldEqual List(
        getMatch("text", 8, 12)
      )
    }
  }

  "check" should "report multiple matches" in {
    val eventuallyMatches = regexValidator.check(
      MatcherRequest(getBlocks("text text text"), "example-category")
    )
    eventuallyMatches.map { matches =>
      matches shouldEqual List(
        getMatch("text", 0, 4),
        getMatch("text", 5, 9),
        getMatch("text", 10, 14)
      )
    }
  }

  "check" should "combine matches when they are of the same category" in {
    val eventuallyMatches = regexValidator.check(
      MatcherRequest(getBlocks("a sentence that will match two rules simultaneously"), "example-category")
    )
    eventuallyMatches.map { matches =>
      println(matches)
      matches.size shouldEqual 1
      matches.head.message should include("2a")
      matches.head.message should include("2b")
    }
  }
}