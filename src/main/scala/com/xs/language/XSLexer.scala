package com.xs.language

import com.xs.language.Definitions.OrDef
import com.xs.language.XSTokens._

object XSLexer {

  abstract class XSLexerDefinition(stringRegex: String)
    extends StringDefinitions.RegexDefinition(stringRegex)

  class ValDefinition
    extends XSLexerDefinition("""val """) {
  }

  class IdentifiableDefinition
    extends XSLexerDefinition("""[a-z][A-Za-z]*""") {
    override def specializeString: PartialFunction[StringDefinitions.StringToken, Definitions.Token] = {
      case StringDefinitions.StringToken(identifiable) => IdentifiableToken(identifiable)
    }
  }

  class EqualAssignmentDefinition
    extends XSLexerDefinition(""" = """) {
  }

  class NumberDefinition
    extends XSLexerDefinition("""-?[0-9]+(\.[0-9]+)?""") {

    override def specializeString: PartialFunction[StringDefinitions.StringToken, Definitions.Token] = {
      case StringDefinitions.StringToken(number) => NumberToken(number)
    }
  }

  class IdentifierDefinition
    extends XSLexerDefinition("""[a-z][A-Za-z]*""") {

    override def specializeString: PartialFunction[StringDefinitions.StringToken, Definitions.Token] = {
      case StringDefinitions.StringToken(identifier) => IdentifierToken(identifier)
    }
  }

  class NewlineSeparatorDefinition
    extends XSLexerDefinition("""\n""")

  class AssignmentDefinition
    extends Definitions.ConcatDef[String](
      new ValDefinition(),
      new IdentifiableDefinition(),
      new EqualAssignmentDefinition(),
      new ExpressionDefinition()
    ) {

    override def specialize: PartialFunction[Definitions.Token, Definitions.Token] = {
      case Definitions.ManyTokens(_, identifier@IdentifiableToken(_), _, expression: ExpressionToken) =>
        XSTokens.AssignmentToken(identifier, expression)
    }
  }

  class ExpressionDefinition
    extends OrDef[String](
      new NumberDefinition(),
      new IdentifierDefinition(),
    )

  class InstructionDefinition
    extends Definitions.OrDef[String](
      new AssignmentDefinition()
    )

  class SeparatedInstructionDefinition
    extends Definitions.ConcatDef[String](
      new InstructionDefinition(),
      new NewlineSeparatorDefinition(),
      new Definitions.OptionDef[String](new NewlineSeparatorDefinition())
    ) {
    override def specialize: PartialFunction[Definitions.Token, Definitions.Token] = {
      case Definitions.ManyTokens(instruction: InstructionToken, _, _) => instruction
    }
  }

  class ProgramDefinition
    extends Definitions.StarDef[String](new SeparatedInstructionDefinition()) {
  }

}
