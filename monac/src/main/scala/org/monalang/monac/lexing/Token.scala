package org.monalang.monac.lexing

abstract class Token

/**
 * Each value token is associated with a single instance of the Lexeme class, which
 * holds lexical information such as the initial raw string and coordinates in
 * the source code.
 */
abstract class ValueToken(val lexeme: ValueLexeme) extends Token

/**
 * Syntax tokens are associated with purely syntactic elements.
 */
abstract class SyntacticToken(val lexeme: SyntacticLexeme) extends Token

/**
 * Tokens unassociated with the characters in the input.
 */
abstract class VirtualToken extends Token

// constants
case class NumLiteral(override val lexeme: ValueLexeme) extends ValueToken(lexeme)
case class CharLiteral(override val lexeme: ValueLexeme) extends ValueToken(lexeme)
case class StringLiteral(override val lexeme: ValueLexeme) extends ValueToken(lexeme)

// purely syntactic elements
case class OpenParens(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class CloseParens(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class OpenBlock(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class CloseBlock(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class OpenList(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class CloseList(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class Period(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class DoublePeriod(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class Comma(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class SemiColon(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class At(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class DoubleColon(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class FunctionArrow(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class BeginLambda(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class EqualsSign(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class Underscore(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class Ampersand(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class Vertical(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)
case class Newlines(override val lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)

// identifiers
// starts with lower or is special
case class LowerId(override val lexeme: ValueLexeme) extends ValueToken(lexeme)
// starts with upper or _ or $
case class UpperId(override val lexeme: ValueLexeme) extends ValueToken(lexeme)

// virtual tokens (inserted)
case object EndOfSource extends VirtualToken

// keyword
abstract class Keyword(lexeme: SyntacticLexeme) extends SyntacticToken(lexeme)

case class KeywordIf(override val lexeme: SyntacticLexeme) extends Keyword(lexeme)
case class KeywordThen(override val lexeme: SyntacticLexeme) extends Keyword(lexeme)
case class KeywordElse(override val lexeme: SyntacticLexeme) extends Keyword(lexeme)
case class KeywordData(override val lexeme: SyntacticLexeme) extends Keyword(lexeme)
case class KeywordClass(override val lexeme: SyntacticLexeme) extends Keyword(lexeme)
case class KeywordLet(override val lexeme: SyntacticLexeme) extends Keyword(lexeme)