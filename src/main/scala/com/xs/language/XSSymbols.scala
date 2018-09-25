package com.xs.language

import com.xs.language.Parsers.CausedException

object XSSymbols {

  case class ExecutionException(message: String,
                                cause: CausedException = None.orNull)
    extends CausedException(message, cause)

  sealed trait ReturnTypeSymbol

  sealed trait ClassSymbol
    extends ReturnTypeSymbol {
    def attributes: Map[String, ClassSymbol]
  }

  case class NumberClassSymbol()
    extends ClassSymbol {
    override val attributes: Map[String, ClassSymbol] = Map.empty
  }

  case class BooleanClassSymbol()
    extends ClassSymbol {
    override val attributes: Map[String, ClassSymbol] = Map.empty
  }

  case class StringClassSymbol()
    extends ClassSymbol {
    override val attributes: Map[String, ClassSymbol] = Map.empty
  }

  case class CompoundClassSymbol(attributes: Map[String, ClassSymbol])
    extends ClassSymbol

  sealed trait ObjectSymbol
    extends ReturnTypeSymbol {
    def attributes: Map[String, ObjectSymbol]

    def classSymbol: ClassSymbol
  }

  case class NumberObjectSymbol(value: Float)
    extends ObjectSymbol {
    override val attributes: Map[String, ObjectSymbol] = Map.empty

    override val classSymbol: ClassSymbol = NumberClassSymbol()
  }

  sealed trait BooleanObjectSymbol
    extends ObjectSymbol

  case class FalseObjectSymbol()
    extends BooleanObjectSymbol {
    override def attributes: Map[String, ObjectSymbol] = Map.empty

    override def classSymbol: ClassSymbol = BooleanClassSymbol()
  }

  case class TrueObjectSymbol()
    extends BooleanObjectSymbol {
    override def attributes: Map[String, ObjectSymbol] = Map.empty

    override def classSymbol: ClassSymbol = BooleanClassSymbol()
  }

  case class StringObjectSymbol(value: String)
    extends ObjectSymbol {
    override val attributes: Map[String, ObjectSymbol] = Map.empty

    override val classSymbol: ClassSymbol = StringClassSymbol()
  }

  case class CompoundObjectSymbol(attributes: Map[String, ObjectSymbol])
    extends ObjectSymbol {
    override val classSymbol: ClassSymbol =
      CompoundClassSymbol(attributes.mapValues(_.classSymbol))
  }

  case class ExecutionContext(classSymbols: Map[String, ClassSymbol], objectSymbols: Map[String, ObjectSymbol])
    extends ReturnTypeSymbol {
    def appendedClass(identifiable: String, newClass: ClassSymbol): ExecutionContext =
      ExecutionContext(classSymbols + (identifiable -> newClass), objectSymbols)

    def appendedObject(identifiable: String, newObject: ObjectSymbol): ExecutionContext =
      ExecutionContext(classSymbols, objectSymbols + (identifiable -> newObject))
  }

  sealed trait XSSymbol {
    def execute(executionContext: ExecutionContext): ReturnTypeSymbol
  }

  sealed trait InstructionSymbol
    extends XSSymbol {
    def execute(executionContext: ExecutionContext): ExecutionContext
  }

  sealed trait ExpressionSymbol
    extends XSSymbol {
    def execute(executionContext: ExecutionContext): ObjectSymbol
  }

  case class NumberSymbol(number: Float) extends ExpressionSymbol {
    override def execute(executionContext: ExecutionContext): NumberObjectSymbol =
      NumberObjectSymbol(number)
  }

  sealed trait BooleanSymbol extends ExpressionSymbol {
    override def execute(executionContext: ExecutionContext): BooleanObjectSymbol
  }

  case class FalseSymbol() extends BooleanSymbol {
    override def execute(executionContext: ExecutionContext): FalseObjectSymbol =
      FalseObjectSymbol()
  }

  case class TrueSymbol() extends BooleanSymbol {
    override def execute(executionContext: ExecutionContext): TrueObjectSymbol =
      TrueObjectSymbol()
  }

  case class IdentifierSymbol(identifier: String) extends ExpressionSymbol {
    override def execute(executionContext: ExecutionContext): ObjectSymbol =
      executionContext.objectSymbols.get(identifier) match {
        case Some(objectSymbol) => objectSymbol
        case None => throw ExecutionException(s"identifier $identifier not found in object context ${executionContext.objectSymbols}")
      }
  }

  case class AssignmentSymbol(identifiable: String, expressionSymbol: ExpressionSymbol) extends InstructionSymbol {
    override def execute(executionContext: ExecutionContext): ExecutionContext =
      executionContext.objectSymbols.get(identifiable) match {
        case None =>
          executionContext.appendedObject(identifiable, expressionSymbol.execute(executionContext))
        case Some(_) =>
          throw ExecutionException(s"Identifier $identifiable already present in object context ${executionContext.objectSymbols}")
      }
  }

  case class InstructionListExpressionSymbol(instructions: Seq[InstructionSymbol], expression: ExpressionSymbol) extends ExpressionSymbol {
    override def execute(executionContext: ExecutionContext): ObjectSymbol =
      expression.execute(instructions.foldLeft(executionContext)((context, instruction) =>
        instruction.execute(context)
      ))
  }

  case class IfElseExpressionSymbol(booleanExpression: ExpressionSymbol, ifExpression: ExpressionSymbol, elseExpression: ExpressionSymbol) extends ExpressionSymbol {
    override def execute(executionContext: ExecutionContext): ObjectSymbol =
      booleanExpression.execute(executionContext) match {
        case FalseObjectSymbol() => elseExpression.execute(executionContext)
        case TrueObjectSymbol() => ifExpression.execute(executionContext)
        case returnedObject => throw ExecutionException(s"If else boolean condition returned $returnedObject")
      }
  }

}
