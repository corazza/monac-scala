package org.monalang.monac.front

abstract class Token

/**
 * Each value token is associated with a single instance of the Lexeme class, which
 * holds lexical information such as the initial raw string and coordinates in
 * the source code.
 */
abstract class ValueToken[ValueType](lexeme: Lexeme, value: ValueType) extends Token

/**
 * Lexical tokens are associated with predefined lexical entities.
 */
abstract class LexicalToken(row: Int, column: Int) extends Token

case class IntegerNumeral(lexeme: Lexeme)
  extends ValueToken(lexeme, LexerConversions.lexemeToIntegerNumeral(lexeme.data))

case class FloatNumeral(lexeme: Lexeme)
  extends ValueToken(lexeme, LexerConversions.lexemeToFloatNumeral(lexeme.data))

case class StringLiteral(lexeme: Lexeme)
  extends ValueToken(lexeme, LexerConversions.lexemeToStringLiteral(lexeme.data))

case class CharacterLiteral(lexeme: Lexeme)
  extends ValueToken(lexeme, LexerConversions.lexemeToCharacterLiteral(lexeme.data))

case class Identifier(lexeme: Lexeme)
  extends ValueToken(lexeme, lexeme.data)

case class OpenBlock(lexeme: Lexeme)
  extends LexicalToken(lexeme.row, lexeme.column)

case class CloseBlock(lexeme: Lexeme)
  extends LexicalToken(lexeme.row, lexeme.column)

case class OpenList(lexeme: Lexeme)
  extends LexicalToken(lexeme.row, lexeme.column)

case class CloseList(lexeme: Lexeme)
  extends LexicalToken(lexeme.row, lexeme.column)

case class StatementType(lexeme: Lexeme)
  extends LexicalToken(lexeme.row, lexeme.column)

case class FunctionArrow(lexeme: Lexeme)
  extends LexicalToken(lexeme.row, lexeme.column)

// virtual tokens (inserted)
case object BreakStatement extends Token
case object EndOfSource extends Token
