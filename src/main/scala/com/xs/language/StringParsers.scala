package com.xs.language

import com.xs.language.Parsers.{PartialParse, ProgramParsingException}

import scala.collection.mutable
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

    override def getRepresentation(alreadyPrinted: mutable.Set[String]): String =
      s"""{"regex": "${stringRegex.replace("\\", "\\\\").replace("\"", "\\\"")}"}"""
  }

  class LiteralParser(stringValue: String)
    extends StringParser {

    override def apply(input: String): Try[PartialParse[String, String]] =
      if (input.startsWith(stringValue)) {
        Success(PartialParse(stringValue, input.drop(stringValue.length)))
      } else {
        parsingFailure(input)
      }

    override def getRepresentation(alreadyPrinted: mutable.Set[String]): String =
      "\"" + stringValue.replace("\n", "\\n").replace("\\", "\\\\").replace("\"", "\\\"") + "\""
  }
}