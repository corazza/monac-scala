package org.monalang.monac.front

import scala.collection.mutable.ArrayBuffer
import org.monalang.monac.types.ConcreteType

abstract class ASTNode

/**
  * Phrases with value
  */
abstract class Expression extends ASTNode

/**
  * Establishes or checks a type of an expression
  *
  * Has the value of <expression>
  */
case class TypeExpression(expression: Expression, hasType: ConcreteType) extends Expression

/**
  * Valueless phrases
  */
abstract class Statement extends ASTNode

/**
  * Represents a definition statement that has taken place.
  */
case class Definition() extends Statement

case class EmptyStatement() extends Statement

case class ExpressionStatement(expression: Expression) extends Statement

abstract class AbstractStatementSequence extends Statement
case class EmptyStatementSequence() extends AbstractStatementSequence
case class StatementSequence(head: Statement, tail: AbstractStatementSequence) extends AbstractStatementSequence