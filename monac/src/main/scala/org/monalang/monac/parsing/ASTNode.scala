package org.monalang.monac.parsing

import org.monalang.monac.lexing.ValueLexeme
import org.monalang.monac.symbol.{InitialSymbolTable, SymbolTable}

abstract class ASTNode()
abstract class SymbolNode(scope: SymbolTable)

case class EmptyNode() extends ASTNode

abstract class LiteralNode(value: ValueLexeme) extends ASTNode

case class NumNode(lexeme: ValueLexeme) extends LiteralNode(lexeme)
case class CharNode(lexeme: ValueLexeme) extends LiteralNode(lexeme)
case class StringNode(lexeme: ValueLexeme) extends LiteralNode(lexeme)

case class LowerIdNode(lexeme: ValueLexeme) extends ASTNode
case class UpperIdNode(lexeme: ValueLexeme) extends ASTNode

case class ArgumentListNode(arguments: List[LowerIdNode]) extends ASTNode

case class StartNode() extends SymbolNode(InitialSymbolTable)