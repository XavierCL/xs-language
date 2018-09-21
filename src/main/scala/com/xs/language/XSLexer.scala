package com.xs.language

import com.xs.language.Definitions.{ConcatDef, Definition, DefinitionException, OrDef}
import com.xs.language.StringDefinitions.{LiteralDefinition, RegexDefinition}
import com.xs.language.XSTokens._

object XSLexer {

  private val valDefinition = new LiteralDefinition("val ")

  private val identifierRegex = new RegexDefinition("[a-z][A-Za-z]*")

  private val identifiableDefinition = identifierRegex
    .map(IdentifiableToken)

  private val equalAssignmentDefinition = new LiteralDefinition(" = ")

  private val numberDefinition = new RegexDefinition("""-?[0-9]+(\.[0-9]+)?""")
    .map(NumberToken)

  private val identifierDefinition = identifierRegex
    .map(IdentifierToken)

  private val newlineSeparatorDefinition = new LiteralDefinition("\n")

  private val expressionDefinition: Definition[String, ExpressionToken] = new OrDef(
    numberDefinition.toFun,
    identifierDefinition.toFun
  )

  private val assignmentDefinition: Definition[String, AssignmentToken] = new ConcatDef(
    valDefinition.toFun,
    identifiableDefinition.toFun,
    equalAssignmentDefinition.toFun,
    expressionDefinition.toFun
  ).map {
    case Seq(_, identifiable: IdentifiableToken, _, expression: ExpressionToken) => AssignmentToken(identifiable, expression)
    case other => throw DefinitionException(s"$this returned $other")
  }

  private val instructionDefinition: Definition[String, InstructionToken] = new OrDef(
    assignmentDefinition.toFun
  )

  private val separatedInstructionDefinition: Definition[String, InstructionToken] = new ConcatDef(
    instructionDefinition.toFun,
    newlineSeparatorDefinition.toFun,
    newlineSeparatorDefinition.opt.toFun
  ).map {
    case Seq(instruction: InstructionToken, _, _) => instruction
  }

  val programDefinition: Definition[String, Seq[InstructionToken]] = separatedInstructionDefinition.star

}
