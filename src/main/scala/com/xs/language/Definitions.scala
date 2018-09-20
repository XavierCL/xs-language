package com.xs.language

import scala.util.{Failure, Success, Try}

object Definitions {

  trait Token {
    def toString: String

    def optimize: Token =
      this
  }

  case class EpsilonToken() extends Token {
    override def toString: String =
      "\"epsilon\""
  }

  case class ManyTokens(tokens: Token*) extends Token {
    override def toString: String =
      """{"many": [""" + tokens.map(_.toString).mkString(",") + """]}"""

    override def optimize: Token =
      tokens.map(_.optimize).flatMap {
        case EpsilonToken() => None
        case other => Some(other)
      } match {
        case Seq() => EpsilonToken()
        case Seq(singleToken) => singleToken
        case many => ManyTokens(many: _*)
      }
  }

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

  case class PartialParse[InputType](output: Token, remainingInput: InputType)

  trait Definition[InputType <: {def isEmpty() : Boolean}] {

    def tryRawPartialParse(input: InputType): Try[PartialParse[InputType]]

    def specialize: PartialFunction[Token, Token] = {
      case token => token
    }

    final def tryPartialParse(input: InputType): Try[PartialParse[InputType]] =
      tryRawPartialParse(input).flatMap(parse =>
        specialize.lift(parse.output).map(specializedOutput =>
          Success(PartialParse(specializedOutput, parse.remainingInput))
        ).getOrElse(
          Failure(DefinitionException(s"Could not specialize ${parse.output.toString} using ${this.toString}"))
        )
      )

    def tryParse(input: InputType): Try[Token] =
      tryPartialParse(input).flatMap {
        case PartialParse(_, remainingInput) if !remainingInput.isEmpty() =>
          Failure(ParsingException(s"$remainingInput remained when parsing $input using $toString"))
        case PartialParse(output, _) => Success(output)
      }

    def toString: String

    def parsingFailure(input: InputType, cause: CausedException = None.orNull): Failure[PartialParse[InputType]] =
      Failure(ParsingException(toString + " could not parse " + input, cause))
  }

  abstract class ConcatDef[InputType <: {def isEmpty() : Boolean}](_definitions: (=> Definition[InputType])*)
    extends Definition[InputType] {

    lazy val definitions: Seq[Definition[InputType]] = _definitions

    override def tryRawPartialParse(input: InputType): Try[PartialParse[InputType]] = {
      def tryPartialParseConcatDefinitions(executingDefinitions: Seq[Definition[InputType]], parsed: Seq[Token], executingInput: InputType): Try[PartialParse[InputType]] =
        executingDefinitions match {
          case headDefinition +: tailDefinitions =>
            headDefinition.tryPartialParse(executingInput) match {
              case Success(PartialParse(output, remainingInput)) =>
                tryPartialParseConcatDefinitions(tailDefinitions, parsed :+ output, remainingInput)
              case Failure(causeException: CausedException) => parsingFailure(input, causeException)
              case otherFailure => otherFailure
            }
          case _ => Success(PartialParse(ManyTokens(parsed: _*), executingInput))
        }

      tryPartialParseConcatDefinitions(definitions, Seq(), input)
    }

    override def toString: String =
      """{"concat": [""" + definitions.map(_.toString).mkString(",") + """]}"""
  }

  abstract class OrDef[InputType <: {def isEmpty() : Boolean}](_definitions: (=> Definition[InputType])*)
    extends Definition[InputType] {

    lazy val definitions: Seq[Definition[InputType]] = _definitions

    override def tryRawPartialParse(input: InputType): Try[PartialParse[InputType]] = {
      def tryPartialParseOrDefinition(executingDefinitions: Seq[Definition[InputType]]): Try[PartialParse[InputType]] =
        executingDefinitions match {
          case headDefinition +: tailDefinitions =>
            headDefinition.tryPartialParse(input) match {
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

  abstract class StarDef[InputType <: {def isEmpty() : Boolean}](_definition: => Definition[InputType])
    extends Definition[InputType] {

    lazy val definition: Definition[InputType] = _definition

    override def tryRawPartialParse(input: InputType): Try[PartialParse[InputType]] = {
      def tryPartialParseStarDefinition(outputs: Seq[Token], executingInput: InputType): Try[PartialParse[InputType]] =
        definition.tryPartialParse(executingInput) match {
          case Failure(_) =>
            Success(PartialParse(ManyTokens(outputs: _*), executingInput))
          case Success(PartialParse(_, remainingInput)) if remainingInput == executingInput =>
            Failure(DefinitionException(s"Infinite match when parsing $input using $toString"))
          case Success(PartialParse(output, remainingInput)) =>
            tryPartialParseStarDefinition(outputs :+ output, remainingInput)
        }

      tryPartialParseStarDefinition(Seq(), input)
    }

    override def tryParse(input: InputType): Try[Token] = {
      def tryPartialParseStarDefinition(outputs: Seq[Token], executingInput: InputType): Try[Token] =
        executingInput match {
          case emptyInput if emptyInput.isEmpty() =>
            Success(ManyTokens(outputs:_*))
          case nonEmptyInput => definition.tryPartialParse(nonEmptyInput) match {
            case Failure(cause: CausedException) =>
              Failure(ParsingException(s"$nonEmptyInput remained when parsing $input using $toString", cause))
            case Success(PartialParse(_, remainingInput)) if remainingInput == executingInput =>
              Failure(DefinitionException(s"Infinite match when parsing $input using $toString"))
            case Success(PartialParse(output, remainingInput)) =>
              tryPartialParseStarDefinition(outputs :+ output, remainingInput)
          }
        }

      tryPartialParseStarDefinition(Seq(), input)
    }

    override def toString: String =
      """{"star": """ + definition.toString() + """}"""
  }

  class EpsilonDef[InputType <: {def isEmpty() : Boolean}]
    extends Definition[InputType] {
    override def tryRawPartialParse(input: InputType): Try[PartialParse[InputType]] =
      Success(PartialParse(EpsilonToken(), input))

    override def specialize: PartialFunction[Token, Token] = {
      case epsilon@EpsilonToken() => epsilon
    }

    override def toString: String =
      "\"epsilon\""
  }

  class OptionDef[InputType <: {def isEmpty() : Boolean}](optionalDefinition: Definition[InputType])
    extends OrDef[InputType](optionalDefinition, new EpsilonDef)
}