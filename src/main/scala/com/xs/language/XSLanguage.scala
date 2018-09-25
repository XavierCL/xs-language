package com.xs.language

import scala.io.Source
import scala.util.{Failure, Success}
import XSLexer.programParser
import com.xs.language.XSSymbols.ExecutionContext
import com.xs.language.XSTokens.CompilationContext

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

      println(programParser.display)

      programParser.parseAll(programContent) match {
        case Success(token) =>
          println("Success")
          println(token.toString)
          val (returnedClass, program) = token.compile(CompilationContext(Map.empty, Map.empty))
          println("Returned class: ")
          println(returnedClass)
          println("Returned program: ")
          println(program)

          println("program execution:BOP\\n")
          val returnedObject = program.execute(ExecutionContext(Map.empty, Map.empty))
          println("EOP")
          println("Returned Object:")
          println(returnedObject)
          println("End of execution")
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