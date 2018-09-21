package com.xs.language

import scala.util.{Failure, Success, Try}

object Definitions {

  abstract class CausedException(message: String,
                                 cause: CausedException = None.orNull)
    extends Exception(message, cause) {
    def getCauseMessages: String =
      message + Option(cause)
        .map(cause => s" caused by:\n${cause.getCauseMessages}")
        .getOrElse("")
  }

  case class ParsingException(private val message: String,
                              private val cause: CausedException = None.orNull
                             ) extends CausedException(message, cause)

  case class DefinitionException(private val message: String,
                                 private val cause: CausedException = None.orNull
                                ) extends CausedException(message, cause)

  case class PartialParse[InputType, +OutputType](output: OutputType, remainingInput: InputType)

  abstract class Definition[InputType <: {def isEmpty() : Boolean}, +OutputType]
    extends (InputType => Try[PartialParse[InputType, OutputType]]) {

    outer =>

    lazy val toFun: () => Definition[InputType, OutputType] =
      () => this

    def parseAll(input: InputType): Try[OutputType] =
      this (input).flatMap {
        case PartialParse(_, remainingInput) if !remainingInput.isEmpty() =>
          Failure(ParsingException(s"$remainingInput remained when parsing $input using $toString"))
        case PartialParse(output, _) => Success(output)
      }

    def map[U](action: OutputType => U): Definition[InputType, U] =
      new Definition[InputType, U] {
        override def apply(input: InputType): Try[PartialParse[InputType, U]] =
          outer(input).map(parse =>
            PartialParse(action(parse.output), parse.remainingInput)
          )

        override def toString: String =
          outer.toString
      }

    def opt: Definition[InputType, Option[OutputType]] =
      new Definition[InputType, Option[OutputType]] {
        override def apply(input: InputType): Try[PartialParse[InputType, Option[OutputType]]] =
          Success(
            outer(input).map(parse =>
              PartialParse(Some(parse.output), parse.remainingInput)
            ).getOrElse(PartialParse(None, input))
          )

        override def toString: String =
          s"""{"opt": ${outer.toString}}"""
      }

    def star: Definition[InputType, Seq[OutputType]] =
      new Definition[InputType, Seq[OutputType]] {
        private def tryPartialParseStarDefinition(outputs: Seq[OutputType], executingInput: InputType): Try[(PartialParse[InputType, Seq[OutputType]], CausedException)] =
          outer(executingInput) match {
            case Failure(exception: CausedException) =>
              Success((PartialParse(outputs, executingInput), exception))
            case Success(PartialParse(_, remainingInput)) if remainingInput == executingInput =>
              Failure(DefinitionException(s"Infinite match when parsing $executingInput using $toString"))
            case Success(PartialParse(output, remainingInput)) =>
              tryPartialParseStarDefinition(outputs :+ output, remainingInput)
          }

        override def apply(input: InputType): Try[PartialParse[InputType, Seq[OutputType]]] =
          tryPartialParseStarDefinition(Seq(), input).map(_._1)

        override def parseAll(input: InputType): Try[Seq[OutputType]] =
          tryPartialParseStarDefinition(Seq(), input).flatMap {
            case (PartialParse(_, remainingInput), exception) if !remainingInput.isEmpty() =>
              Failure(ParsingException(s"Could not fully parse $remainingInput using $toString", exception))
            case (PartialParse(output, _), _) =>
              Success(output)
          }

        override def toString: String =
          s"""{"star": ${outer.toString}}"""
      }

    def toString: String

    protected def parsingFailure(input: InputType, cause: CausedException = None.orNull): Failure[PartialParse[InputType, OutputType]] =
      Failure(ParsingException(toString + " could not parse " + input, cause))
  }

  class ConcatDef[InputType <: {def isEmpty() : Boolean}, +OutputType](_definitions: (() => Definition[InputType, OutputType])*)
    extends Definition[InputType, Seq[OutputType]] {

    lazy val definitions: Seq[Definition[InputType, OutputType]] = _definitions.map(_())

    override def apply(input: InputType): Try[PartialParse[InputType, Seq[OutputType]]] = {
      def tryPartialParseConcatDefinitions(executingDefinitions: Seq[Definition[InputType, OutputType]], parsed: Seq[OutputType], executingInput: InputType): Try[PartialParse[InputType, Seq[OutputType]]] =
        executingDefinitions match {
          case headDefinition +: tailDefinitions =>
            headDefinition(executingInput) match {
              case Success(PartialParse(output, remainingInput)) =>
                tryPartialParseConcatDefinitions(tailDefinitions, parsed :+ output, remainingInput)
              case Failure(causeException: CausedException) => parsingFailure(input, causeException)
              case Failure(otherException) => Failure(new Exception("Unknown exception", otherException))
            }
          case _ => Success(PartialParse(parsed, executingInput))
        }

      tryPartialParseConcatDefinitions(definitions, Seq(), input)
    }

    override def toString: String =
      """{"concat": [""" + definitions.map(_.toString).mkString(",") + """]}"""
  }

  class OrDef[InputType <: {def isEmpty() : Boolean}, +OutputType](_definitions: (() => Definition[InputType, OutputType])*)
    extends Definition[InputType, OutputType] {

    lazy val definitions: Seq[Definition[InputType, OutputType]] = _definitions.map(_())

    override def apply(input: InputType): Try[PartialParse[InputType, OutputType]] = {
      def tryPartialParseOrDefinition(executingDefinitions: Seq[Definition[InputType, OutputType]]): Try[PartialParse[InputType, OutputType]] =
        executingDefinitions match {
          case headDefinition +: tailDefinitions =>
            headDefinition(input) match {
              case success@Success(_) => success
              case _ => tryPartialParseOrDefinition(tailDefinitions)
            }
          case _ => parsingFailure(input)
        }

      tryPartialParseOrDefinition(definitions)
    }

    override def toString: String =
      """{"or": [""" + definitions.map(_.toString()).mkString(",") + """]}"""
  }

}
