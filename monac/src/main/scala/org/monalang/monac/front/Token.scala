package org.monalang.monac.front

/**
 * A constituent single unit of source code.
 *
 * Each token is associated with a single instance of the Lexeme class, which
 * holds lexical information such as the initial raw string and coordinates in
 * the source code.
 */
abstract class Token(lexeme: Lexeme)

case class Numerical(lexeme: Lexeme) extends Token(lexeme) {
  val value = Lexer.lexemeToNumerical(lexeme.data)
}

case class Literal(lexeme: Lexeme) extends Token(lexeme) {
  val value = Lexer.lexemeToLiteral(lexeme.data)
}

case class Identifier(lexeme: Lexeme) extends Token(lexeme) {
  val value = lexeme.data
}
