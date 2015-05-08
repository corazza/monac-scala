package org.monalang.monac.front

abstract class Token
/**
 * Each lexical token is associated with a single instance of the Lexeme class, which
 * holds lexical information such as the initial raw string and coordinates in
 * the source code.
 */
abstract class LexicalToken[ValueType](lexeme: Lexeme, value: ValueType) extends Token

case class IntegerNumeral(lexeme: Lexeme)
  extends LexicalToken(lexeme, LexerConversions.lexemeToIntegerNumeral(lexeme.data))

case class FloatNumeral(lexeme: Lexeme)
  extends LexicalToken(lexeme, LexerConversions.lexemeToFloatNumeral(lexeme.data))

case class StringLiteral(lexeme: Lexeme)
  extends LexicalToken(lexeme, LexerConversions.lexemeToStringLiteral(lexeme.data))

case class CharacterLiteral(lexeme: Lexeme)
  extends LexicalToken(lexeme, LexerConversions.lexemeToCharacterLiteral(lexeme.data))

case class Identifier(lexeme: Lexeme)
  extends LexicalToken(lexeme, lexeme.data)

case object BreakStatement extends Token
case object EndOfSource extends Token
