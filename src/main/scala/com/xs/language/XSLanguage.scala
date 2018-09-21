package com.xs.language

import scala.io.Source
import scala.util.{Failure, Success}
import XSLexer.programDefinition

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

      println(programDefinition.toString.replace("\\", "\\\\"))

      programDefinition.parseAll(programContent) match {
        case Success(token) =>
          println("Success")
          println(token.toString)
        case Failure(exception@Definitions.ParsingException(_, _)) =>
          println("Parsing Failure:")
          println(exception.getCauseMessages)
        case Failure(exception@Definitions.DefinitionException(_, _)) =>
          println("Definition Failure:")
          println(exception.getCauseMessages)
        case Failure(exception) =>
          println("Unknown error:")
          println(exception.getStackTrace.mkString("\n"))
      }
    }
  }
}