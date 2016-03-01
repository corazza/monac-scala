package org.monalang.monac.parsing

import org.monalang.monac.lexing.ValueLexeme
import org.monalang.monac.symbol.SymbolTable

abstract class ASTNode
case class EmptyNode() extends ASTNode

abstract class LiteralNode(value: ValueLexeme) extends ASTNode
case class Num(lexeme: ValueLexeme) extends LiteralNode(lexeme)
case class Char(lexeme: ValueLexeme) extends LiteralNode(lexeme)
case class StringNode(lexeme: ValueLexeme) extends LiteralNode(lexeme)

abstract class Identifier(lexeme: ValueLexeme) extends ASTNode
case class LowerId(lexeme: ValueLexeme) extends Identifier(lexeme)
case class UpperId(lexeme: ValueLexeme) extends Identifier(lexeme)
case class Operator(lexeme: ValueLexeme) extends Identifier(lexeme)

case class InfixRight(operator: Operator, expression: Expression) extends ASTNode

case class FLHS(identifier: LowerId, arguments: ArgumentList) extends ASTNode

abstract class Statement extends ASTNode
case class Definition(name: String, symbol: org.monalang.monac.symbol.Symbol) extends Statement
case class ExpressionStatement(expression: Expression) extends Statement

abstract class ScopedNode(val scope: SymbolTable) extends ASTNode
case class DefinitionSequence(override val scope: SymbolTable, definitions: List[Definition]) extends ScopedNode(scope)

object ParentScopeConnector {
  def getChildScope(parentScope: SymbolTable) = {
    val scope = new SymbolTable()
    scope.parent = Some(parentScope)
    scope
  }
}

abstract class Expression(scope: SymbolTable) extends ScopedNode(scope)
case class StatementSequence(override val scope: SymbolTable, statements: List[Statement]) extends Expression(scope)
case class UnitExpression() extends Expression(new SymbolTable())
case class FunctionApplication(parentScope: SymbolTable, function: Expression, argument: Expression) extends Expression(ParentScopeConnector.getChildScope(parentScope))
case class BindingExpression(parentScope: SymbolTable, identifier: Identifier) extends Expression(ParentScopeConnector.getChildScope(parentScope))
case class LiteralExpression(parentScope: SymbolTable, literal: LiteralNode) extends Expression(ParentScopeConnector.getChildScope(parentScope))

// obvious sugar
case class IfExpression(parentScope: SymbolTable, conditional: Expression, branch1: Expression, branch2: Expression) extends Expression(ParentScopeConnector.getChildScope(parentScope))

// structural
case class ArgumentList(arguments: List[LowerId]) extends ASTNode
case class IdList(ids: List[Identifier]) extends ASTNode
case class SimpleContinuation(simpleArgument: Expression, continuation: ASTNode) extends ASTNode