package org.monalang.monac.front

/**
 * A single unit of source code.
 *
 * Each token is associated with a single instance of the Lexeme class, which
 * holds lexical information such as the initial raw string and coordinates in
 * the source code.
 */
abstract class Token(lexeme: Lexeme)

case class IntegerNumeral(lexeme: Lexeme) extends Token(lexeme) {
  val value = LexerConversions.lexemeToIntegerNumeral(lexeme.data)
}

case class FloatNumeral(lexeme: Lexeme) extends Token(lexeme) {
  val value = LexerConversions.lexemeToFloatNumeral(lexeme.data)
}

case class StringLiteral(lexeme: Lexeme) extends Token(lexeme) {
  val value = LexerConversions.lexemeToStringLiteral(lexeme.data)
}

case class CharacterLiteral(lexeme: Lexeme) extends Token(lexeme) {
  val value = LexerConversions.lexemeToCharacterLiteral(lexeme.data)
}

case class Identifier(lexeme: Lexeme) extends Token(lexeme) {
  val value = lexeme.data
}

case class Break(lexeme: Lexeme) extends Token(lexeme) {
  val value = lexeme.data // newline or ;
}

case object EndOfSource extends Token(new Lexeme("", 0, 0))
