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

// arguments: List[PatternNode]
case class FLHS(identifier: String, arguments: ArgumentList) extends ASTNode
case class ArgumentList(arguments: List[String]) extends ASTNode

abstract class Statement extends ASTNode
case class Definition(name: String) extends Statement

case class DefinitionSequence(definitions: List[Definition]) extends ASTNode

abstract class ScopedNode(val scope: SymbolTable) extends ASTNode

abstract class Expression(scope: SymbolTable) extends ScopedNode(scope)
case class StatementSequence(override val scope: SymbolTable, statements: List[Statement]) extends Expression(scope)
case class ExpressionStatement(override val scope: SymbolTable) extends Expression(scope)

case class UnitExpression() extends Expression(new SymbolTable(None))