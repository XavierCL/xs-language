package com.xs.language

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object Parsers {

  abstract class CausedException(message: String,
                                 cause: Throwable = None.orNull)
    extends Exception(message, cause) {
    def getCauseMessages: String =
      message + (Option(cause) match {
        case Some(caused: CausedException) => s" caused by:\n${caused.getCauseMessages}"
        case Some(other) => s" caused by:\n${other.getMessage}"
        case None => ""
      })
  }

  sealed abstract class ParsingException(message: String,
                                         cause: Throwable = None.orNull)
    extends CausedException(message, cause)

  case class ProgramParsingException(message: String,
                                     cause: Throwable = None.orNull
                                    )
    extends ParsingException(message, cause)

  case class ParsingDefinitionException(message: String,
                                        cause: Throwable = None.orNull
                                       )
    extends ParsingException(message, cause)

  case class PartialParse[InputType, +OutputType](output: OutputType, remainingInput: InputType)

  abstract class Parser[InputType <: {def isEmpty() : Boolean}, +OutputType]
    extends (InputType => Try[PartialParse[InputType, OutputType]]) {

    outer =>

    def parseAll(input: InputType): Try[OutputType] =
      outer(input).flatMap {
        case PartialParse(_, remainingInput) if !remainingInput.isEmpty() =>
          Failure(ProgramParsingException(s"$remainingInput remained when parsing $input using $id"))
        case PartialParse(output, _) => Success(output)
      }

    def map[U](action: OutputType => U): Parser[InputType, U] =
      new Parser[InputType, U] {
        override def apply(input: InputType): Try[PartialParse[InputType, U]] =
          outer(input).map(parse =>
            PartialParse(action(parse.output), parse.remainingInput)
          )

        override def getRepresentation(alreadyPrinted: mutable.Set[String]): String =
          outer.displayIfNotDisplayed(alreadyPrinted)
      }

    def ~[OtherOutputType](_other: =>Parser[InputType, OtherOutputType]): Parser[InputType, (OutputType, OtherOutputType)] = {
      lazy val other = _other
      new Parser[InputType, (OutputType, OtherOutputType)] {
        override def apply(input: InputType): Try[PartialParse[InputType, (OutputType, OtherOutputType)]] =
          outer(input)
            .flatMap(parsed => other(parsed.remainingInput)
              .map(otherParsed =>
                PartialParse((parsed.output, otherParsed.output), otherParsed.remainingInput)
              )
            )

        override def getRepresentation(alreadyPrinted: mutable.Set[String]): String =
          s"""{"and": [${outer.displayIfNotDisplayed(alreadyPrinted)}, ${other.displayIfNotDisplayed(alreadyPrinted)}]}"""
      }
    }

    def <~[OtherOutputType](_other: =>Parser[InputType, OtherOutputType]): Parser[InputType, OutputType] = {
      lazy val other = _other
      new Parser[InputType, OutputType] {
        override def apply(input: InputType): Try[PartialParse[InputType, OutputType]] =
          outer(input)
            .flatMap(parsed => other(parsed.remainingInput)
              .map(otherParsed =>
                PartialParse(parsed.output, otherParsed.remainingInput)
              )
            )

        override def getRepresentation(alreadyPrinted: mutable.Set[String]): String =
          s"""{"andLeft": [${outer.displayIfNotDisplayed(alreadyPrinted)}, ${other.displayIfNotDisplayed(alreadyPrinted)}]}"""
      }
    }

    def ~>[OtherOutputType](_other: =>Parser[InputType, OtherOutputType]): Parser[InputType, OtherOutputType] = {
      lazy val other = _other
      new Parser[InputType, OtherOutputType] {
        override def apply(input: InputType): Try[PartialParse[InputType, OtherOutputType]] =
          outer(input)
            .flatMap(parsed => other(parsed.remainingInput)
              .map(otherParsed =>
                PartialParse(otherParsed.output, otherParsed.remainingInput)
              )
            )

        override def getRepresentation(alreadyPrinted: mutable.Set[String]): String =
          s"""{"andRight": [${outer.displayIfNotDisplayed(alreadyPrinted)}, ${other.displayIfNotDisplayed(alreadyPrinted)}]}"""
      }
    }

    def |[OtherOutputType >: OutputType](_other: =>Parser[InputType, OtherOutputType]): Parser[InputType, OtherOutputType] = {
      lazy val other = _other
      new Parser[InputType, OtherOutputType] {
        override def apply(input: InputType): Try[PartialParse[InputType, OtherOutputType]] =
          outer(input) match {
            case success@Success(_) => success
            case Failure(_) => other(input) match {
              case success@Success(_) => success
              case Failure(_) => Failure(ProgramParsingException(s"Could not parse $input using $display"))
            }
          }

        override def getRepresentation(alreadyPrinted: mutable.Set[String]): String =
          s"""{"or": [${outer.displayIfNotDisplayed(alreadyPrinted)}, ${other.displayIfNotDisplayed(alreadyPrinted)}]}"""
      }
    }

    def opt: Parser[InputType, Option[OutputType]] =
      new Parser[InputType, Option[OutputType]] {
        override def apply(input: InputType): Try[PartialParse[InputType, Option[OutputType]]] =
          Success(
            outer(input).map(parse =>
              PartialParse(Some(parse.output), parse.remainingInput)
            ).getOrElse(PartialParse(None, input))
          )

        override def getRepresentation(alreadyPrinted: mutable.Set[String]): String =
          s"""{"opt": ${outer.displayIfNotDisplayed(alreadyPrinted)}}"""
      }

    def star: Parser[InputType, Seq[OutputType]] =
      new Parser[InputType, Seq[OutputType]] {
        private def tryPartialParseStarParser(outputs: Seq[OutputType], executingInput: InputType): Try[(PartialParse[InputType, Seq[OutputType]], ParsingException)] =
          outer(executingInput) match {
            case Failure(exception: ParsingException) =>
              Success((PartialParse(outputs, executingInput), exception))
            case Success(PartialParse(_, remainingInput)) if remainingInput == executingInput =>
              Failure(ParsingDefinitionException(s"Infinite match when parsing $executingInput using $id"))
            case Success(PartialParse(output, remainingInput)) =>
              tryPartialParseStarParser(outputs :+ output, remainingInput)
          }

        override def apply(input: InputType): Try[PartialParse[InputType, Seq[OutputType]]] =
          tryPartialParseStarParser(Seq(), input).map(_._1)

        override def parseAll(input: InputType): Try[Seq[OutputType]] =
          tryPartialParseStarParser(Seq(), input).flatMap {
            case (PartialParse(_, remainingInput), exception) if !remainingInput.isEmpty() =>
              Failure(ProgramParsingException(s"Could not fully parse $remainingInput using $id", exception))
            case (PartialParse(output, _), _) =>
              Success(output)
          }

        override def getRepresentation(alreadyPrinted: mutable.Set[String]): String =
          s"""{"star": ${outer.displayIfNotDisplayed(alreadyPrinted)}}"""
      }

    def rep(count: Int): Parser[InputType, Seq[OutputType]] =
      new Parser[InputType, Seq[OutputType]] {
        override def apply(input: InputType): Try[PartialParse[InputType, Seq[OutputType]]] = {
          def applyRepParser(executingCount: Int, executingInput: InputType, outputs: Seq[OutputType]): Try[PartialParse[InputType, Seq[OutputType]]] =
            executingCount match {
              case zeroOrLower if zeroOrLower <= 0 => Success(PartialParse(outputs, executingInput))
              case nonZeroCount => outer(executingInput) match {
                case Failure(error) => Failure(ProgramParsingException(s"Could not parse $input $count times using ${outer.id}. Failed at the $executingCount time.", error))
                case Success(PartialParse(output, remainingInput)) => applyRepParser(nonZeroCount - 1, remainingInput, outputs :+ output)
              }
            }
          applyRepParser(count, input, Seq.empty[OutputType])
        }

        override def getRepresentation(alreadyPrinted: mutable.Set[String]): String =
          s"""{"rep": {"count": $count, "inner": ${outer.displayIfNotDisplayed(alreadyPrinted)}}}"""
      }

    def named(name: String): Parser[InputType, OutputType] =
      new Parser[InputType, OutputType] {
        override def apply(input: InputType): Try[PartialParse[InputType, OutputType]] =
          outer(input)

        override def getRepresentation(alreadyPrinted: mutable.Set[String]): String =
          outer.getRepresentation(alreadyPrinted)

        override def displayIfNotDisplayed(alreadyPrinted: mutable.Set[String]): String =
          if(alreadyPrinted.contains(id)) {
            s"""{"oldRef": $id}"""
          } else {
            s"""{"newRef": $id, "value": ${getRepresentation(alreadyPrinted += id)}}"""
          }

        override val id: String =
          "\"" + name + "\""
      }

    def getRepresentation(alreadyPrinted: mutable.Set[String]): String

    def displayIfNotDisplayed(alreadyPrinted: mutable.Set[String]): String =
      getRepresentation(alreadyPrinted += outer.id)

    def display: String =
      displayIfNotDisplayed(mutable.Set.empty[String])

    def id: String =
      s"${outer.hashCode()}"

    protected def parsingFailure(input: InputType, cause: Throwable = None.orNull): Failure[PartialParse[InputType, OutputType]] =
      Failure(ProgramParsingException(s"""${outer.id} could not parse "$input" as $display""", cause))
  }

}
