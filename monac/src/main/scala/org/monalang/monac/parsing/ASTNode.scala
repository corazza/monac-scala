package org.monalang.monac.parsing

import org.monalang.monac.lexing.ValueLexeme
import org.monalang.monac.symbol.SymbolTable

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

case class DefinitionSequence(scope: SymbolTable, definitions: List[Definition]) extends ASTNode

class Statement(val parentScopeCarrier: SymbolTable) extends ASTNode
case class Definition(override val parentScopeCarrier: SymbolTable, name: String, symbol: org.monalang.monac.symbol.Symbol) extends Statement(parentScopeCarrier)
case class ExpressionStatement(override val parentScopeCarrier: SymbolTable, expression: Expression) extends Statement(parentScopeCarrier)

abstract class Expression(val parentScope: SymbolTable) extends ASTNode

case class Block(override val parentScope: SymbolTable, scope: SymbolTable, statements: List[Statement]) extends Expression(parentScope)
case class UnitExpression(override val parentScope: SymbolTable) extends Expression(parentScope)
case class FunctionApplication(override val parentScope: SymbolTable, function: Expression, argument: Expression) extends Expression(parentScope)
case class BindingExpression(override val parentScope: SymbolTable, identifier: Identifier) extends Expression(parentScope)
case class LiteralExpression(override val parentScope: SymbolTable, literal: LiteralNode) extends Expression(parentScope)

case class IfExpression(override val parentScope: SymbolTable, conditional: Expression, branch1: Expression, branch2: Expression) extends Expression(parentScope)

case class ArgumentList(arguments: List[LowerId]) extends ASTNode
case class IdList(ids: List[Identifier]) extends ASTNode
case class SimpleContinuation(simpleArgument: Expression, continuation: ASTNode) extends ASTNode