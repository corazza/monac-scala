package org.monalang.monac.parsing

import org.monalang.monac.lexing._
import scala.reflect._

/*

TODO:

1. Basic function declarations with n non-pattern parameters and expression blocks (inner declarations allowed)
2. Parsing with context functionality and IR capability (AST's for now, stored in scope)
3. Rest of the grammar

 */

// 1. finish nt literal
// 2. left-factor

object MonaGrammar extends Grammar("mona", List(
  Start -> List(Declaration, DeclarationSeparator, Start) -> Fragments.start,

  DeclarationSeparator -> List(Terminal(classTag[Newlines])) -> Fragments.emptyNode,

  Declaration -> List(FLHS, RHS) -> Fragments.functionDeclaration,

  FLHS -> List(Terminal(classTag[LowerId]), ArgumentListHead) -> Fragments.FLHS,
  ArgumentListHead -> List(Terminal(classTag[LowerId]), ArgumentList) -> Fragments.argumentListHead,
  ArgumentList -> List(ArgumentListHead) -> Fragments.argumentList,
  ArgumentList -> List(Eta) -> Fragments.emptyNode,

  RHS -> List(Terminal(classTag[EqualsSign]), Expression) -> Fragments.RHS,

  Expression -> List(FunctionExpression) -> ((c)=>{ EmptyNode() /* extract FunctionExpression AST */ }),
  Expression -> List(Terminal(classTag[KeywordIf]), Expression, Terminal(classTag[KeywordThen]), Expression, Terminal(classTag[KeywordElse]), Expression) -> Fragments.ifExpression,

  FunctionExpression -> List(Argument, FunctionExpressionPrime) -> Fragments.functionExpression,
  FunctionExpressionPrime -> List(FunctionExpression) -> Fragments.functionExpression,
  FunctionExpressionPrime -> List(Eta) -> Fragments.emptyNode,

  Argument -> List(Terminal(classTag[LowerId])) -> Fragments.bindingExpression,
  Argument -> List(Literal) -> Fragments.literalExpression,
  Argument -> List(Terminal(classTag[OpenParens]), Expression, Terminal(classTag[CloseParens])) -> ((c)=>{ EmptyNode() /* extract Expression AST */ }),
  Argument -> List(Terminal(classTag[OpenBlock]), ExpressionSequence, Terminal(classTag[CloseBlock])) -> ((c)=>{ EmptyNode() /* extract Expression AST */ }),

  // Evaluates to ()
  ExpressionSequence -> List(Eta) -> Fragments.emptyNode,
  ExpressionSequence -> List(Expression, ExpressionSequencePrime) -> Fragments.emptyNode,
  ExpressionSequencePrime -> List(Eta) -> Fragments.emptyNode,
  ExpressionSequencePrime -> List(ExpressionSeparator, ExpressionSequence) -> Fragments.emptyNode,
  ExpressionSeparator  -> List(Terminal(classTag[Newlines])) -> Fragments.emptyNode,
  ExpressionSeparator  -> List(Terminal(classTag[SemiColon])) -> Fragments.emptyNode,

  Literal -> List(Terminal(classTag[NumLiteral])) -> Fragments.literalExpression,
  Literal -> List(Terminal(classTag[CharLiteral])) -> Fragments.literalExpression,
  Literal -> List(Terminal(classTag[StringLiteral])) -> Fragments.literalExpression
))

object Start extends NonTerminal
object DeclarationSeparator extends NonTerminal
object Declaration extends NonTerminal
object FLHS extends NonTerminal
object ArgumentListHead extends NonTerminal
object ArgumentList extends NonTerminal
object RHS extends NonTerminal
object Expression extends NonTerminal
object ExpressionSequence extends NonTerminal
object ExpressionSequencePrime extends NonTerminal
object ExpressionSeparator extends NonTerminal
object Block extends NonTerminal
object FunctionExpression extends NonTerminal
object FunctionExpressionPrime extends NonTerminal
object Argument extends NonTerminal
// TODO fix literals in the lexer
object Literal extends NonTerminal