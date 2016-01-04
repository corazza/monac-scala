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

  Expression -> List(FunctionExpression, ExpressionPrime) -> Fragments.emptyNode,
  Expression -> List(SimpleExpression, ExpressionPrime) -> Fragments.emptyNode,
  ExpressionPrime -> List(Eta) -> Fragments.emptyNode,
  ExpressionPrime -> List(Operator, Expression) -> Fragments.infix,

  Operator -> List(Terminal(classTag[OperatorId])) -> Fragments.matched,
  Operator -> List(Terminal(classTag[Backtick]), Terminal(classTag[LowerId]), Terminal(classTag[Backtick])) -> Fragments.extract(2),

  SimpleExpression -> List(Terminal(classTag[KeywordIf]), Expression, Terminal(classTag[KeywordThen]), Expression, Terminal(classTag[KeywordElse]), Expression) -> Fragments.ifExpression,

  FunctionExpression -> List(Argument, FunctionExpressionPrime) -> Fragments.functionExpression,
  FunctionExpressionPrime -> List(FunctionExpression) -> Fragments.functionExpression,
  FunctionExpressionPrime -> List(Eta) -> Fragments.emptyNode,

  Argument -> List(Terminal(classTag[LowerId])) -> Fragments.bindingExpression,
  Argument -> List(SimpleArgument) -> Fragments.extract(1),
  SimpleArgument -> List(Literal) -> Fragments.extract(1),
  SimpleArgument -> List(Terminal(classTag[OpenParens]), OptionalNewlines, Expression, OptionalNewlines, Terminal(classTag[CloseParens])) -> ((c)=>{ EmptyNode() /* extract Expression AST */ }),
  SimpleArgument -> List(Terminal(classTag[OpenBlock]), OptionalNewlines, StatementSequence, Terminal(classTag[CloseBlock])) -> ((c)=>{ EmptyNode() /* extract Expression AST */ }),

  StatementSequence -> List(Statement, StatementSequencePrime) -> Fragments.emptyNode,
  StatementSequencePrime -> List(Eta) -> Fragments.emptyNode,
  StatementSequencePrime -> List(StatementSeparator, StatementSequence) -> Fragments.emptyNode,

  Statement -> List(Eta) -> Fragments.emptyNode,
  Statement -> List(DeclarationOrExpression) -> Fragments.extract(1),
  Statement -> List(SimpleExpression) -> Fragments.extract(1),

  DeclarationOrExpression -> List(SimpleArgument) -> Fragments.extract(1),
  DeclarationOrExpression -> List(Terminal(classTag[LowerId]), DeclarationOrExpressionRepeatId, DeclarationOrExpressionPrime) -> Fragments.emptyNode,
  DeclarationOrExpressionRepeatId -> List(Eta) -> Fragments.emptyNode,
  DeclarationOrExpressionRepeatId -> List(Terminal(classTag[LowerId]), DeclarationOrExpressionRepeatId) -> Fragments.repeatId,
  DeclarationOrExpressionPrime -> List(Eta) -> Fragments.emptyNode,
  DeclarationOrExpressionPrime -> List(SimpleArgument, DeclarationOrExpressionPrimePrime) -> Fragments.emptyNode, // wrap in a special node
  DeclarationOrExpressionPrimePrime -> List(FunctionExpressionPrime) -> Fragments.emptyNode,
  DeclarationOrExpressionPrimePrime -> List(Operator, Expression) -> Fragments.emptyNode,
  DeclarationOrExpressionPrime -> List(RHS) -> Fragments.RHS,

  StatementSeparator  -> List(Terminal(classTag[Newlines])) -> Fragments.emptyNode,
  StatementSeparator  -> List(Terminal(classTag[SemiColon])) -> Fragments.emptyNode,

  Literal -> List(Terminal(classTag[NumLiteral])) -> Fragments.matched,
  Literal -> List(Terminal(classTag[CharLiteral])) -> Fragments.matched,
  Literal -> List(Terminal(classTag[StringLiteral])) -> Fragments.matched
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
object ExpressionPrime extends NonTerminal
object SimpleExpression extends NonTerminal
object Operator extends NonTerminal
object Statement extends NonTerminal
object StatementOther extends NonTerminal
object StatementSequence extends NonTerminal
object StatementSequencePrime extends NonTerminal
object StatementSeparator extends NonTerminal
object DeclarationOrExpression extends NonTerminal
object DeclarationOrExpressionRepeatId extends NonTerminal
object DeclarationOrExpressionPrime extends NonTerminal
object DeclarationOrExpressionPrimePrime extends NonTerminal
object Block extends NonTerminal
object FunctionExpression extends NonTerminal
object FunctionExpressionPrime extends NonTerminal
object Argument extends NonTerminal
object SimpleArgument extends NonTerminal
object Literal extends NonTerminal