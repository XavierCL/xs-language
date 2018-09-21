package com.xs.language

object XSTokens {

  sealed trait XSToken

  trait ExpressionToken
    extends XSToken

  trait InstructionToken
    extends ExpressionToken

  case class NumberToken(number: String)
    extends ExpressionToken {
    override def toString: String =
      "\"" + number + "\""
  }

  case class IdentifierToken(identifier: String)
    extends ExpressionToken {
    override def toString: String =
      "\"" + identifier + "\""
  }

  case class IdentifiableToken(identifier: String)
    extends XSToken {
    override def toString: String =
      "\"" + identifier + "\""
  }

  case class AssignmentToken(identifier: IdentifiableToken, expression: ExpressionToken)
    extends InstructionToken {
    override def toString: String =
      s"""{"assignment": {"val": $identifier, "expr": $expression}}"""
  }

}
