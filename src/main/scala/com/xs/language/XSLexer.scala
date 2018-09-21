package com.xs.language

import com.xs.language.Parsers.{ConcatParser, OrParser, Parser, ParsingDefinitionException}
import com.xs.language.StringParsers.{LiteralParser, RegexParser}
import com.xs.language.XSTokens._

object XSLexer {

  private val valParser = new LiteralParser("val ")

  private val identifierRegex = new RegexParser("[a-z][A-Za-z]*")

  private val identifiableParser = identifierRegex
    .map(IdentifiableToken)

  private val equalAssignmentParser = new LiteralParser(" = ")

  private val numberParser = new RegexParser("""-?[0-9]+(\.[0-9]+)?""")
    .map(NumberToken)

  private val identifierParser = identifierRegex
    .map(IdentifierToken)

  private val newlineSeparatorParser = new LiteralParser("\n")

  private val expressionParser: Parser[String, ExpressionToken] = new OrParser(
    numberParser.toFun,
    identifierParser.toFun
  )

  private val assignmentParser: Parser[String, AssignmentToken] = new ConcatParser(
    valParser.toFun,
    identifiableParser.toFun,
    equalAssignmentParser.toFun,
    expressionParser.toFun
  ).map {
    case Seq(_, identifiable: IdentifiableToken, _, expression: ExpressionToken) => AssignmentToken(identifiable, expression)
    case other => throw ParsingDefinitionException(s"$this returned $other")
  }

  private val instructionParser: Parser[String, InstructionToken] = new OrParser(
    assignmentParser.toFun
  )

  private val separatedInstructionParser: Parser[String, InstructionToken] = new ConcatParser(
    instructionParser.toFun,
    newlineSeparatorParser.toFun,
    newlineSeparatorParser.opt.toFun
  ).map {
    case Seq(instruction: InstructionToken, _, _) => instruction
  }

  val programParser: Parser[String, Seq[InstructionToken]] = separatedInstructionParser.star

}
