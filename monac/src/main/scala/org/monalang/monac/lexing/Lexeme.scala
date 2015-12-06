package org.monalang.monac.lexing

/**
 * Stores lexical data about a token.
 */
abstract class Lexeme(row: Int, column: Int)

case class ValueLexeme(row: Int, column: Int, data: String) extends Lexeme(row, column) {
  def toSyntactic = new SyntacticLexeme(row, column)
}

case class SyntacticLexeme(row: Int, column: Int) extends Lexeme(row, column)