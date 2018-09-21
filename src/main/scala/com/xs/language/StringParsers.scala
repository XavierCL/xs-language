package com.xs.language

import com.xs.language.Parsers.{ProgramParsingException, PartialParse}

import scala.util.{Failure, Success, Try}

object StringParsers {

  trait StringParser extends Parsers.Parser[String, String]

  class RegexParser(stringRegex: String)
    extends StringParser {

    private val compiledRegex = stringRegex.r

    override def apply(input: String): Try[PartialParse[String, String]] =
      compiledRegex.findPrefixMatchOf(input)
        .map { matched =>
          val parsed = matched.group(0)
          val remainingInput = input.drop(matched.end)
          Success(Parsers.PartialParse(parsed, remainingInput))
        }.getOrElse(parsingFailure(input))

    override def toString: String =
      s"""{"regex": "$stringRegex"}"""
  }

  class LiteralParser(stringValue: String)
    extends StringParser {

    override def apply(input: String): Try[PartialParse[String, String]] =
      if (input.startsWith(stringValue)) {
        Success(PartialParse(stringValue, input.drop(stringValue.length)))
      } else {
        Failure(ProgramParsingException(s"Could not parse $input using $toString"))
      }

    override def toString: String =
      "\"" + stringValue + "\""
  }
}