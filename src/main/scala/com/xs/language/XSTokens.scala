package com.xs.language

import com.xs.language.Parsers.CausedException
import com.xs.language.XSSymbols._

object XSTokens {

  case class CompilationException(message: String,
                                  cause: CausedException = None.orNull)
    extends CausedException(message, cause)

  sealed trait ReturnTypeToken

  sealed trait ClassToken
    extends ReturnTypeToken {
    def attributes: Map[String, ClassToken]
  }

  case class NumberClassToken()
    extends ClassToken {
    override def attributes: Map[String, ClassToken] = Map.empty
  }

  sealed trait GenericBooleanClassToken
    extends ClassToken {
    override def attributes: Map[String, ClassToken] = Map.empty
  }

  case class BooleanClassToken()
    extends GenericBooleanClassToken

  case class FalseClassToken()
    extends GenericBooleanClassToken

  case class TrueClassToken()
    extends GenericBooleanClassToken

  case class StringClassToken()
    extends ClassToken {
    override def attributes: Map[String, ClassToken] = Map.empty
  }

  case class CompoundClassToken(attributes: Map[String, ClassToken])
    extends ClassToken

  case class CompilationContext(classTokens: Map[String, ClassToken], objectTokens: Map[String, ClassToken])
    extends ReturnTypeToken {
    def appendedClass(identifiable: String, newClass: ClassToken): CompilationContext =
      CompilationContext(classTokens + (identifiable -> newClass), objectTokens)

    def appendedObject(identifiable: String, newObject: ClassToken): CompilationContext =
      CompilationContext(classTokens, objectTokens + (identifiable -> newObject))
  }

  sealed trait XSToken {
    def compile(context: CompilationContext): (ReturnTypeToken, XSSymbol)
  }

  sealed trait InstructionToken
    extends XSToken {
    def compile(context: CompilationContext): (CompilationContext, InstructionSymbol)
  }

  sealed trait ExpressionToken
    extends XSToken {
    def compile(context: CompilationContext): (ClassToken, ExpressionSymbol)
  }

  case class NumberToken(number: Float)
    extends ExpressionToken {

    override def compile(context: CompilationContext): (ClassToken, NumberSymbol) =
      (NumberClassToken(), NumberSymbol(number))

    override def toString: String =
      number.toString
  }

  sealed trait BooleanToken
    extends ExpressionToken {
    override def compile(context: CompilationContext): (GenericBooleanClassToken, BooleanSymbol)
  }

  case class FalseToken()
    extends BooleanToken {
    override def compile(context: CompilationContext): (FalseClassToken, FalseSymbol) =
      (FalseClassToken(), FalseSymbol())
  }

  case class TrueToken()
    extends BooleanToken {
    override def compile(context: CompilationContext): (TrueClassToken, TrueSymbol) =
      (TrueClassToken(), TrueSymbol())
  }

  case class IdentifierToken(identifier: String)
    extends ExpressionToken {

    override def compile(context: CompilationContext): (ClassToken, IdentifierSymbol) =
      context.objectTokens.get(identifier) match {
        case Some(objectFound) => (objectFound, IdentifierSymbol(identifier))
        case None => throw CompilationException(s"Identifier $identifier was not found in object context ${context.objectTokens}")
      }

    override def toString: String =
      s"""{"id": "$identifier"}"""
  }

  case class AssignmentToken(identifiable: String, expression: ExpressionToken)
    extends InstructionToken {

    override def compile(context: CompilationContext): (CompilationContext, AssignmentSymbol) =
      context.objectTokens.get(identifiable) match {
        case None =>
          val (classToken, symbol) = expression.compile(context)
          (context.appendedObject(identifiable, classToken), AssignmentSymbol(identifiable, symbol))
        case Some(_) => throw CompilationException(s"Identifier $identifiable already defined in object context ${context.objectTokens}")
      }

    override def toString: String =
      s"""{"assignment": {"val": $identifiable, "expr": $expression}}"""
  }

  case class InstructionListExpressionToken(instructionList: Seq[InstructionToken], expressionToken: ExpressionToken)
    extends ExpressionToken {
    override def compile(context: CompilationContext): (ClassToken, InstructionListExpressionSymbol) = {
      val (newContext, instructionSymbols) = instructionList.foldLeft(context, Seq.empty[InstructionSymbol]) {
        (newContextAndInstructions: (CompilationContext, Seq[InstructionSymbol]),
         instruction: InstructionToken
        ) =>
          val (oldContext, compiledInstructions) = newContextAndInstructions
          val (newContext, newCompiledInstruction) = instruction.compile(oldContext)
          (newContext, compiledInstructions :+ newCompiledInstruction)
      }
      val (classToken, expressionSymbol) = expressionToken.compile(newContext)
      (classToken, InstructionListExpressionSymbol(instructionSymbols, expressionSymbol))
    }
  }

  case class IfElseExpressionToken(booleanExpression: ExpressionToken, ifExpression: ExpressionToken, elseExpression: ExpressionToken)
    extends ExpressionToken {
    override def compile(context: CompilationContext): (ClassToken, ExpressionSymbol) = {
      booleanExpression.compile(context) match {
        case (FalseClassToken(), _) => elseExpression.compile(context)
        case (TrueClassToken(), _) => ifExpression.compile(context)
        case (BooleanClassToken(), booleanSymbol: ExpressionSymbol) =>
          val (ifClassType, ifSymbol) = ifExpression.compile(context)
          val (elseClassType, elseSymbol) = elseExpression.compile(context)
          if (ifClassType == elseClassType) {
            (ifClassType, IfElseExpressionSymbol(booleanSymbol, ifSymbol, elseSymbol))
          } else {
            throw CompilationException(s"If expression $ifExpression has return class type $ifClassType, which must equal else expression $elseExpression class type $elseClassType")
          }
        case (otherClass, _) => throw CompilationException(s"If condition expression $booleanExpression evaluated to $otherClass class type. Should be boolean")
      }
    }
  }

}
