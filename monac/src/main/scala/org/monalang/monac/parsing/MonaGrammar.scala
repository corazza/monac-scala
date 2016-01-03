package org.monalang.monac.parsing

import org.monalang.monac.lexing._
import scala.reflect._

object MonaGrammar extends Grammar("mona", List(
  Start -> List(Declaration, StartPrime) -> Fragments.start,
  StartPrime -> List(DeclarationSeparator, Start) -> Fragments.extract(2),
  StartPrime -> List(Eta) -> Fragments.emptyNode,

  OptionalNewlines -> List(Eta) -> Fragments.emptyNode,
  OptionalNewlines -> List(Terminal(classTag[Newlines])) -> Fragments.emptyNode,

  DeclarationSeparator -> List(Terminal(classTag[Newlines])) -> Fragments.emptyNode,

  Declaration -> List(FLHS, RHS) -> Fragments.functionDeclaration,

  FLHS -> List(Terminal(classTag[LowerId]), ArgumentListHead) -> Fragments.FLHS,
  ArgumentListHead -> List(Terminal(classTag[LowerId]), ArgumentList) -> Fragments.argumentListHead,
  ArgumentList -> List(ArgumentListHead) -> Fragments.argumentList,
  ArgumentList -> List(Eta) -> Fragments.emptyNode,

  RHS -> List(Terminal(classTag[EqualsSign]), OptionalNewlines, Expression) -> Fragments.RHS,

  Expression -> List(FunctionExpression) -> Fragments.extract(1),
  Expression -> List(ExpressionOther) -> Fragments.extract(1),
  ExpressionOther -> List(Terminal(classTag[KeywordIf]), Expression, Terminal(classTag[KeywordThen]), Expression, Terminal(classTag[KeywordElse]), Expression) -> Fragments.ifExpression,

  FunctionExpression -> List(Argument, FunctionExpressionPrime) -> Fragments.functionExpression,
  FunctionExpressionPrime -> List(FunctionExpression) -> Fragments.functionExpression,
  FunctionExpressionPrime -> List(Eta) -> Fragments.emptyNode,

  Argument -> List(Terminal(classTag[LowerId])) -> Fragments.bindingExpression,
  Argument -> List(ArgumentOther) -> Fragments.extract(1),
  ArgumentOther -> List(Literal) -> Fragments.literalExpression,
  ArgumentOther -> List(Terminal(classTag[OpenParens]), OptionalNewlines, Expression, OptionalNewlines, Terminal(classTag[CloseParens])) -> ((c)=>{ EmptyNode() /* extract Expression AST */ }),
  ArgumentOther -> List(Terminal(classTag[OpenBlock]), OptionalNewlines, StatementSequence, Terminal(classTag[CloseBlock])) -> ((c)=>{ EmptyNode() /* extract Expression AST */ }),

  // Evaluates to ()
  StatementSequence -> List(Statement, StatementSequencePrime) -> Fragments.emptyNode,
  StatementSequencePrime -> List(Eta) -> Fragments.emptyNode,
  StatementSequencePrime -> List(StatementSeparator, StatementSequence) -> Fragments.emptyNode,

  Statement -> List(Eta) -> Fragments.emptyNode,
  Statement -> List(DeclarationOrFE) -> Fragments.extract(1),
  Statement -> List(ExpressionOther) -> Fragments.extract(1),

  DeclarationOrFE -> List(ArgumentOther) -> Fragments.extract(1),

  DeclarationOrFE -> List(Terminal(classTag[LowerId]), DeclarationOrFERepeatId, DeclarationOrFEPrime) -> Fragments.emptyNode,
  DeclarationOrFERepeatId -> List(Eta) -> Fragments.emptyNode,
  DeclarationOrFERepeatId -> List(Terminal(classTag[LowerId]), DeclarationOrFERepeatId) -> Fragments.repeatId,
  DeclarationOrFEPrime -> List(Eta) -> Fragments.emptyNode,
  DeclarationOrFEPrime -> List(ArgumentOther, FunctionExpressionPrime) -> Fragments.emptyNode, // wrap in a special node
  DeclarationOrFEPrime -> List(RHS) -> Fragments.RHS,

  StatementSeparator  -> List(Terminal(classTag[Newlines])) -> Fragments.emptyNode,
  StatementSeparator  -> List(Terminal(classTag[SemiColon])) -> Fragments.emptyNode,

  Literal -> List(Terminal(classTag[NumLiteral])) -> Fragments.literalExpression,
  Literal -> List(Terminal(classTag[CharLiteral])) -> Fragments.literalExpression,
  Literal -> List(Terminal(classTag[StringLiteral])) -> Fragments.literalExpression
))

object Start extends NonTerminal
object StartPrime extends NonTerminal
object OptionalNewlines extends NonTerminal
object DeclarationSeparator extends NonTerminal
object Declaration extends NonTerminal
object FLHS extends NonTerminal
object ArgumentListHead extends NonTerminal
object ArgumentList extends NonTerminal
object RHS extends NonTerminal
object Expression extends NonTerminal
object ExpressionOther extends NonTerminal
object Statement extends NonTerminal
object StatementOther extends NonTerminal
object StatementSequence extends NonTerminal
object StatementSequencePrime extends NonTerminal
object StatementSeparator extends NonTerminal
object DeclarationOrFE extends NonTerminal
object DeclarationOrFERepeatId extends NonTerminal
object DeclarationOrFEPrime extends NonTerminal
object Block extends NonTerminal
object FunctionExpression extends NonTerminal
object FunctionExpressionPrime extends NonTerminal
object Argument extends NonTerminal
object ArgumentOther extends NonTerminal
object Literal extends NonTerminal