package org.monalang.monac.parsing

import org.monalang.monac.lexing.ValueLexeme
import org.monalang.monac.symbol.Scope

abstract class ASTNode
case class EmptyNode() extends ASTNode

abstract class LiteralNode(val lexeme: ValueLexeme) extends ASTNode
case class Num(override val lexeme: ValueLexeme) extends LiteralNode(lexeme)
case class Char(override val lexeme: ValueLexeme) extends LiteralNode(lexeme)
case class StringNode(override val lexeme: ValueLexeme) extends LiteralNode(lexeme)

abstract class Identifier(val lexeme: ValueLexeme) extends ASTNode
case class LowerId(override val lexeme: ValueLexeme) extends Identifier(lexeme)
case class UpperId(override val lexeme: ValueLexeme) extends Identifier(lexeme)
case class Operator(override val lexeme: ValueLexeme) extends Identifier(lexeme)

case class InfixRight(operator: Operator, expression: Expression) extends ASTNode

case class FLHS(identifier: LowerId, arguments: ArgumentList) extends ASTNode

case class ArgumentList(arguments: List[LowerId]) extends ASTNode
case class IdList(ids: List[Identifier]) extends ASTNode
case class SimpleContinuation(simpleArgument: Expression, continuation: ASTNode) extends ASTNode


case class DefinitionSequence(scope: Scope, definitions: List[Definition]) extends ASTNode

class Statement extends ASTNode
case class Definition(scope: Scope, name: String, symbol: org.monalang.monac.symbol.Symbol) extends Statement
case class Let(name: String, symbol: org.monalang.monac.symbol.Symbol) extends Statement
case class ExpressionStatement(expression: Expression) extends Statement

abstract class Expression extends ASTNode
object UnitExpression extends Expression
case class BindingExpression(identifier: Identifier) extends Expression
case class LiteralExpression(literal: LiteralNode) extends Expression
case class ScopedExpression(scope: Scope, statements: List[Statement]) extends Expression
case class FunctionApplication(function: Expression, argument: Expression) extends Expression
case class IfExpression(conditional: Expression, branch1: Expression, branch2: Expression) extends Expression