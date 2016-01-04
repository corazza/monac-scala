package org.monalang.monac.parsing

import org.monalang.monac.lexing.{SyntacticLexeme, ValueLexeme}
import org.monalang.monac.symbol.{InitialSymbolTable, SymbolTable}

abstract class ASTNode
case class EmptyNode() extends ASTNode

abstract class LiteralNode(value: ValueLexeme) extends ASTNode
case class Num(lexeme: ValueLexeme) extends LiteralNode(lexeme)
case class Char(lexeme: ValueLexeme) extends LiteralNode(lexeme)
case class StringNode(lexeme: ValueLexeme) extends LiteralNode(lexeme)

abstract class Identifier(lexeme: ValueLexeme) extends ASTNode
case class LowerIdNode(lexeme: ValueLexeme) extends Identifier(lexeme)
case class UpperIdNode(lexeme: ValueLexeme) extends Identifier(lexeme)
case class OperatorNode(lexeme: ValueLexeme) extends Identifier(lexeme)