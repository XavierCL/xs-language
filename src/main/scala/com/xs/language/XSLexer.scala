package com.xs.language

import com.xs.language.Parsers.Parser
import com.xs.language.StringParsers.{LiteralParser, RegexParser}
import com.xs.language.XSTokens._

object XSLexer {

  private val indentParser = new LiteralParser("  ")
    .named("indent")

  private val valParser = new LiteralParser("val ")
    .named("val")

  private val identifierRegex = new RegexParser("[a-z][A-Za-z]*")
    .named("identifierRegex")

  private val identifiableParser = identifierRegex
    .named("identifiable")

  private val equalAssignmentParser = new LiteralParser(" = ")
    .named("equal")

  private val numberParser = new RegexParser("""-?[0-9]+(\.[0-9]+)?""")
    .map(stringValue => NumberToken(stringValue.toFloat))
    .named("number")

  private val falseParser = new LiteralParser("""false""")
    .map(_ => FalseToken())
    .named("false")

  private val trueParser = new LiteralParser("""true""")
    .map(_ => TrueToken())
    .named("true")

  private val booleanParser = (trueParser | falseParser)
      .named("boolean")

  private val identifierParser = identifierRegex
    .map(IdentifierToken)
    .named("identifier")

  private val expressionParser =
    (numberParser | booleanParser | identifierParser)
      .named("expression")

  private val assignmentParser = (indentLevel: Int) => (
    (valParser ~> identifiableParser) ~ (equalAssignmentParser ~> multiLineExpressionParser(indentLevel))
    ).map {
    case (identifiable, expression) => AssignmentToken(identifiable, expression)
  }.named("assignment")

  private val instructionParser = (indentLevel: Int) =>
    assignmentParser(indentLevel)
      .named("instruction")

  private val newlineSeparatorParser =
    (indentLevel: Int) => (new LiteralParser("\n") <~ indentParser.rep(indentLevel))
      .named("newlineSeparator")

  private val separatedInstructionParser =
    (indentLevel: Int) => (instructionParser(indentLevel) <~
      newlineSeparatorParser(indentLevel) <~ newlineSeparatorParser(indentLevel).opt)
      .named("separatedInstruction")

  private val multiLineExpressionParser =
    (indentLevel: Int) => ifElseParser(indentLevel) | blockExpressionParser(indentLevel) | expressionParser

  private val instructionListExpression =
    (indentLevel: Int) => (
      separatedInstructionParser(indentLevel).star ~ multiLineExpressionParser(indentLevel) <~
        newlineSeparatorParser(indentLevel - 1)
      ).map {
      case (Seq(), expression) => expression
      case (instructions, expression) => InstructionListExpressionToken(instructions, expression)
    }.named("instructionListExpression")

  private val blockExpressionParser: Int => Parser[String, ExpressionToken] =
    (indentLevel: Int) => (new LiteralParser("{") ~>
      newlineSeparatorParser(indentLevel + 1) ~> instructionListExpression(indentLevel + 1) <~
      indentParser.rep(indentLevel) <~ new LiteralParser("}"))
      .named("blockExpression")

  private val ifElseParser =
    (indentLevel: Int) => (
      (new LiteralParser("if (") ~> expressionParser) ~
        (new LiteralParser(") ") ~> blockExpressionParser(indentLevel)) ~
        (new LiteralParser(" else ") ~> blockExpressionParser(indentLevel))
      ).map {
      case ((booleanExpression, ifExpression), elseExpression) => IfElseExpressionToken(booleanExpression, ifExpression, elseExpression)
    }

  val programParser: Parser[String, ExpressionToken] =
    instructionListExpression(0).named("program")

}
