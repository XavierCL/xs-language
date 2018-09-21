package com.xs.language

import com.xs.language.Definitions.{ParsingException, PartialParse}

import scala.util.{Failure, Success, Try}

object StringDefinitions {

  trait StringDefinition extends Definitions.Definition[String, String]

  class RegexDefinition(stringRegex: String)
    extends StringDefinition {

    private val compiledRegex = stringRegex.r

    override def apply(input: String): Try[PartialParse[String, String]] =
      compiledRegex.findPrefixMatchOf(input)
        .map { matched =>
          val parsed = matched.group(0)
          val remainingInput = input.drop(matched.end)
          Success(Definitions.PartialParse(parsed, remainingInput))
        }.getOrElse(parsingFailure(input))

    override def toString: String =
      s"""{"regex": "$stringRegex"}"""
  }

  class LiteralDefinition(stringValue: String)
    extends StringDefinition {

    override def apply(input: String): Try[PartialParse[String, String]] =
      if (input.startsWith(stringValue)) {
        Success(PartialParse(stringValue, input.drop(stringValue.length)))
      } else {
        Failure(ParsingException(s"Could not parse $input using $toString"))
      }

    override def toString: String =
      "\"" + stringValue + "\""
  }
}