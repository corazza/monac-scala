package org.monalang.monac.front

abstract class Token

/**
 * Each value token is associated with a single instance of the Lexeme class, which
 * holds lexical information such as the initial raw string and coordinates in
 * the source code.
 */
abstract class ValueToken[ValueType](lexeme: ValueLexeme, value: ValueType) extends Token

/**
 * Syntax tokens are associated with purely syntactic elements.
 */
abstract class SyntacticToken(lexeme: SyntacticLexeme) extends Token

/**
 * Tokens unassociated with the characters in the input.
 */
abstract class VirtualToken extends Token

// constants
case class IntegerNumeral(lexeme: ValueLexeme) extends ValueToken(lexeme, LexerConversions.lexemeToIntegerNumeral(lexeme.data))
case class FloatNumeral(lexeme: ValueLexeme) extends ValueToken(lexeme, LexerConversions.lexemeToFloatNumeral(lexeme.data))
case class StringLiteral(lexeme: ValueLexeme) extends ValueToken(lexeme, LexerConversions.lexemeToStringLiteral(lexeme.data))
case class CharacterLiteral(lexeme: ValueLexeme) extends ValueToken(lexeme, LexerConversions.lexemeToCharacterLiteral(lexeme.data))

// purely syntactic elements
case class OpenParens(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class CloseParens(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class OpenBlock(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class CloseBlock(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class OpenList(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class CloseList(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class PeriodToken(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class Comma(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class SemiColon(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class DoubleColon(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class FunctionArrow(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class EqualsSign(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class Underscore(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class Newlines(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)

// identifiers
// starts with lower or is special
case class LowerId(lexeme: ValueLexeme) extends ValueToken(lexeme, lexeme.data)
// starts with upper or _ or $
case class UpperId(lexeme: ValueLexeme) extends ValueToken(lexeme, lexeme.data)

// virtual tokens (inserted)
case object EndOfSource extends VirtualToken
