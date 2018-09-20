package com.xs.language

import scala.util.{Success, Try}

object StringDefinitions {

  case class StringToken(value: String) extends Definitions.Token {
    override def toString: String =
      "\"" + value.replace("\\", "\\\\") + "\""
  }

  trait StringDefinition extends Definitions.Definition[String]

  abstract class RegexDefinition(stringRegex: String)
    extends StringDefinition {

    private val compiledRegex = ("""^(""" + stringRegex + """)([\s\S]*)$""").r

    def specializeString: PartialFunction[StringToken, Definitions.Token] = {
      case token => token
    }

    override def tryRawPartialParse(input: String): Try[Definitions.PartialParse[String]] =
      compiledRegex.findFirstMatchIn(input)
        .map { matched =>
          val parsed = matched.group(1)
          val remainingInput = matched.group(matched.groupCount)
          Success(Definitions.PartialParse(StringToken(parsed), remainingInput))
        }.getOrElse(parsingFailure(input))

    override def specialize: PartialFunction[Definitions.Token, Definitions.Token] = {
      case stringToken@StringToken(_) => specializeString(stringToken)
    }

    override def toString: String =
      "{\"regex\": \"" + stringRegex + "\"}"
  }

}
