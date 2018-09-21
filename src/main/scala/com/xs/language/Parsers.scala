package com.xs.language

import scala.util.{Failure, Success, Try}

object Parsers {

  abstract class ParsingException(message: String,
                                  cause: ParsingException = None.orNull)
    extends Exception(message, cause) {
    def getCauseMessages: String =
      message + Option(cause)
        .map(cause => s" caused by:\n${cause.getCauseMessages}")
        .getOrElse("")
  }

  case class ProgramParsingException(private val message: String,
                                     private val cause: ParsingException = None.orNull
                             ) extends ParsingException(message, cause)

  case class ParsingDefinitionException(private val message: String,
                                        private val cause: ParsingException = None.orNull
                                ) extends ParsingException(message, cause)

  case class PartialParse[InputType, +OutputType](output: OutputType, remainingInput: InputType)

  abstract class Parser[InputType <: {def isEmpty() : Boolean}, +OutputType]
    extends (InputType => Try[PartialParse[InputType, OutputType]]) {

    outer =>

    lazy val toFun: () => Parser[InputType, OutputType] =
      () => this

    def parseAll(input: InputType): Try[OutputType] =
      this (input).flatMap {
        case PartialParse(_, remainingInput) if !remainingInput.isEmpty() =>
          Failure(ProgramParsingException(s"$remainingInput remained when parsing $input using $toString"))
        case PartialParse(output, _) => Success(output)
      }

    def map[U](action: OutputType => U): Parser[InputType, U] =
      new Parser[InputType, U] {
        override def apply(input: InputType): Try[PartialParse[InputType, U]] =
          outer(input).map(parse =>
            PartialParse(action(parse.output), parse.remainingInput)
          )

        override def toString: String =
          outer.toString
      }

    def opt: Parser[InputType, Option[OutputType]] =
      new Parser[InputType, Option[OutputType]] {
        override def apply(input: InputType): Try[PartialParse[InputType, Option[OutputType]]] =
          Success(
            outer(input).map(parse =>
              PartialParse(Some(parse.output), parse.remainingInput)
            ).getOrElse(PartialParse(None, input))
          )

        override def toString: String =
          s"""{"opt": ${outer.toString}}"""
      }

    def star: Parser[InputType, Seq[OutputType]] =
      new Parser[InputType, Seq[OutputType]] {
        private def tryPartialParseStarParser(outputs: Seq[OutputType], executingInput: InputType): Try[(PartialParse[InputType, Seq[OutputType]], ParsingException)] =
          outer(executingInput) match {
            case Failure(exception: ParsingException) =>
              Success((PartialParse(outputs, executingInput), exception))
            case Success(PartialParse(_, remainingInput)) if remainingInput == executingInput =>
              Failure(ParsingDefinitionException(s"Infinite match when parsing $executingInput using $toString"))
            case Success(PartialParse(output, remainingInput)) =>
              tryPartialParseStarParser(outputs :+ output, remainingInput)
          }

        override def apply(input: InputType): Try[PartialParse[InputType, Seq[OutputType]]] =
          tryPartialParseStarParser(Seq(), input).map(_._1)

        override def parseAll(input: InputType): Try[Seq[OutputType]] =
          tryPartialParseStarParser(Seq(), input).flatMap {
            case (PartialParse(_, remainingInput), exception) if !remainingInput.isEmpty() =>
              Failure(ProgramParsingException(s"Could not fully parse $remainingInput using $toString", exception))
            case (PartialParse(output, _), _) =>
              Success(output)
          }

        override def toString: String =
          s"""{"star": ${outer.toString}}"""
      }

    def toString: String

    protected def parsingFailure(input: InputType, cause: ParsingException = None.orNull): Failure[PartialParse[InputType, OutputType]] =
      Failure(ProgramParsingException(toString + " could not parse " + input, cause))
  }

  class ConcatParser[InputType <: {def isEmpty() : Boolean}, +OutputType](_parsers: (() => Parser[InputType, OutputType])*)
    extends Parser[InputType, Seq[OutputType]] {

    lazy val parsers: Seq[Parser[InputType, OutputType]] = _parsers.map(_())

    override def apply(input: InputType): Try[PartialParse[InputType, Seq[OutputType]]] = {
      def tryPartialParseConcatParser(executingParsers: Seq[Parser[InputType, OutputType]], parsed: Seq[OutputType], executingInput: InputType): Try[PartialParse[InputType, Seq[OutputType]]] =
        executingParsers match {
          case headParser +: tailParsers =>
            headParser(executingInput) match {
              case Success(PartialParse(output, remainingInput)) =>
                tryPartialParseConcatParser(tailParsers, parsed :+ output, remainingInput)
              case Failure(causeException: ParsingException) => parsingFailure(input, causeException)
              case Failure(otherException) => Failure(new Exception("Unknown exception", otherException))
            }
          case _ => Success(PartialParse(parsed, executingInput))
        }

      tryPartialParseConcatParser(parsers, Seq(), input)
    }

    override def toString: String =
      """{"concat": [""" + parsers.map(_.toString).mkString(",") + """]}"""
  }

  class OrParser[InputType <: {def isEmpty() : Boolean}, +OutputType](_parsers: (() => Parser[InputType, OutputType])*)
    extends Parser[InputType, OutputType] {

    lazy val parsers: Seq[Parser[InputType, OutputType]] = _parsers.map(_())

    override def apply(input: InputType): Try[PartialParse[InputType, OutputType]] = {
      def tryPartialParseOrParser(executingParsers: Seq[Parser[InputType, OutputType]]): Try[PartialParse[InputType, OutputType]] =
        executingParsers match {
          case headParser +: tailParsers =>
            headParser(input) match {
              case success@Success(_) => success
              case _ => tryPartialParseOrParser(tailParsers)
            }
          case _ => parsingFailure(input)
        }

      tryPartialParseOrParser(parsers)
    }

    override def toString: String =
      """{"or": [""" + parsers.map(_.toString()).mkString(",") + """]}"""
  }

}
