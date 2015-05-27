package org.monalang.monac.front

import scala.collection.mutable.ArrayBuffer
import org.monalang.monac.types.ConcreteType

abstract class ASTNode

class Expression extends ASTNode {

}

class Statement extends ASTNode {

}

/**
 * Represents a binding statement that has taken place.
 */
class Binding extends Statement {

}

case class ExpressionStatement(expression: Expression) extends Statement

/**
 * Establishes or checks a type of an expression
 */
case class TypeStatement(expression: Expression, typee: ConcreteType) extends Statement

class StatementSequence extends ASTNode {
  val statementNodes = new ArrayBuffer[Statement]()

  def addStatement(node: Statement) {
    statementNodes += node
  }
}
