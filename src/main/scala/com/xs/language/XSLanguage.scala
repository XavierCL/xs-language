package com.xs.language

import scala.io.Source
import scala.util.{Failure, Success}
import XSLexer.programParser

object XSLanguage {
  private def using[A <: {def close() : Unit}, B](resource: A)(action: A => B) =
    try {
      action(resource)
    } finally {
      resource.close()
    }

  def main(args: Array[String]): Unit = {
    using(Source.fromFile("src/main/resources/program.xs")) { file =>
      val programContent = file.mkString
      print("\n\nSOF")
      print(programContent)
      print("EOF\n\n")

      println(programParser.toString)

      programParser.parseAll(programContent) match {
        case Success(token) =>
          println("Success")
          println(token.toString)
        case Failure(exception@Parsers.ProgramParsingException(_, _)) =>
          println("Parsing Failure:")
          println(exception.getCauseMessages)
        case Failure(exception@Parsers.ParsingDefinitionException(_, _)) =>
          println("Definition Failure:")
          println(exception.getCauseMessages)
        case Failure(exception) =>
          println("Unknown error:")
          println(exception.getStackTrace.mkString("\n"))
      }
    }
  }
}