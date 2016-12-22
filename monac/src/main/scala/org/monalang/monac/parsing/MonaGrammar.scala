package org.monalang.monac.parsing

import org.monalang.monac.lexing._

import scala.reflect._

object MonaGrammar extends Grammar("mona", List(
  StartNT -> List(DefinitionNT, StartPrimeNT) -> Fragments.start,
  StartPrimeNT -> List(DefinitionSeparatorNT, StartNT) -> Fragments.extract(2),
  StartPrimeNT -> List(Eta) -> Fragments.emptyNode,

  OptionalNewlinesNT -> List(Eta) -> Fragments.emptyNode,
  OptionalNewlinesNT -> List(Terminal(classTag[Newlines])) -> Fragments.emptyNode,

  DefinitionSeparatorNT -> List(Terminal(classTag[Newlines])) -> Fragments.emptyNode,

  DefinitionNT -> List(FLHSNT, RHSNT) -> Fragments.functionDefinition,

  FLHSNT -> List(Terminal(classTag[LowerIdToken]), ArgumentListHeadNT) -> Fragments.FLHSNT,
  ArgumentListHeadNT -> List(Terminal(classTag[LowerIdToken]), ArgumentListNT) -> Fragments.argumentListHead,
  ArgumentListNT -> List(ArgumentListHeadNT) -> Fragments.extract(1),
  ArgumentListNT -> List(Eta) -> Fragments.emptyNode,

  RHSNT -> List(Terminal(classTag[EqualsSign]), OptionalNewlinesNT, ExpressionNT) -> Fragments.extract(3),

  ExpressionNT -> List(SimpleExpressionNT) -> Fragments.expression,
  ExpressionNT -> List(FunctionExpressionNT, ExpressionPrimeNT) -> Fragments.expression,
  ExpressionPrimeNT -> List(Eta) -> Fragments.emptyNode,
  ExpressionPrimeNT -> List(OperatorNT, ExpressionNT) -> Fragments.infix,

  SimpleExpressionNT -> List(Terminal(classTag[KeywordIf]), ExpressionNT, Terminal(classTag[KeywordThen]), ExpressionNT, Terminal(classTag[KeywordElse]), ExpressionNT) -> Fragments.ifExpression,

  OperatorNT -> List(Terminal(classTag[OperatorId])) -> Fragments.matched,
  OperatorNT -> List(Terminal(classTag[Backtick]), Terminal(classTag[LowerIdToken]), Terminal(classTag[Backtick])) -> Fragments.extract(2),

  FunctionExpressionNT -> List(ArgumentNT, FunctionExpressionPrimeNT) -> Fragments.functionExpression,
  FunctionExpressionPrimeNT -> List(FunctionExpressionNT) -> Fragments.functionExpression,
  FunctionExpressionPrimeNT -> List(Eta) -> Fragments.emptyNode,

  ArgumentNT -> List(Terminal(classTag[LowerIdToken])) -> Fragments.extract(1),
  ArgumentNT -> List(SimpleArgumentNT) -> Fragments.extract(1),
  SimpleArgumentNT -> List(LiteralNT) -> Fragments.extract(1),
  SimpleArgumentNT -> List(Terminal(classTag[OpenParens]), OptionalNewlinesNT, SimpleArgumentPrimeNT) -> Fragments.extract(3),
  SimpleArgumentPrimeNT -> List(ExpressionNT, OptionalNewlinesNT, Terminal(classTag[CloseParens])) -> Fragments.extract(1),
  SimpleArgumentPrimeNT -> List(Terminal(classTag[CloseParens])) -> Fragments.emptyNode,
  SimpleArgumentNT -> List(Terminal(classTag[OpenBlock]), OptionalNewlinesNT, StatementSequenceNT, Terminal(classTag[CloseBlock])) -> Fragments.extract(3),

  StatementSequenceNT -> List(Eta) -> Fragments.emptyNode,
  StatementSequenceNT -> List(StatementNT, StatementSequencePrimeNT) -> Fragments.statementSequence,
  StatementSequencePrimeNT -> List(Eta) -> Fragments.emptyNode,
  StatementSequencePrimeNT -> List(StatementSeparatorNT, StatementSequenceNT) -> Fragments.extract(2),

  StatementSeparatorNT  -> List(Terminal(classTag[Newlines])) -> Fragments.emptyNode,
  StatementSeparatorNT  -> List(Terminal(classTag[SemiColon])) -> Fragments.emptyNode,

  StatementNT -> List(DefinitionOrExpressionNT) -> Fragments.extract(1),
  StatementNT -> List(Terminal(classTag[KeywordLet]), Terminal(classTag[LowerIdToken]), Terminal(classTag[EqualsSign]), ExpressionNT) -> Fragments.letStatement,
  StatementNT -> List(SimpleExpressionNT) -> Fragments.extract(1),

  DefinitionOrExpressionNT -> List(DefinitionOrExpressionPrimeNT) -> Fragments.extract(1),
  DefinitionOrExpressionNT -> List(Terminal(classTag[LowerIdToken]), DefinitionOrExpressionRepeatIdNT, DefinitionOrExpressionPrimeNT) -> Fragments.definitionOrFE,

  DefinitionOrExpressionRepeatIdNT -> List(Eta) -> Fragments.repeatId,
  DefinitionOrExpressionRepeatIdNT -> List(Terminal(classTag[LowerIdToken]), DefinitionOrExpressionRepeatIdNT) -> Fragments.repeatId,

  DefinitionOrExpressionPrimeNT -> List(Eta) -> Fragments.emptyNode,
  DefinitionOrExpressionPrimeNT -> List(SimpleArgumentNT, DefinitionOrExpressionPrimePrimeNT) -> Fragments.simpleContinuation,
  DefinitionOrExpressionPrimeNT -> List(OperatorNT, ExpressionNT) -> Fragments.infix,

  DefinitionOrExpressionPrimePrimeNT -> List(FunctionExpressionPrimeNT) -> Fragments.extract(1),
  DefinitionOrExpressionPrimePrimeNT -> List(OperatorNT, ExpressionNT) -> Fragments.infix,

  DefinitionOrExpressionPrimeNT -> List(RHSNT) -> Fragments.extract(1),

  LiteralNT -> List(Terminal(classTag[NumLiteral])) -> Fragments.literalExpression,
  LiteralNT -> List(Terminal(classTag[CharLiteral])) -> Fragments.literalExpression,
  LiteralNT -> List(Terminal(classTag[StringLiteral])) -> Fragments.literalExpression
))

object StartNT extends NonTerminal
object StartPrimeNT extends NonTerminal
object OptionalNewlinesNT extends NonTerminal
object DefinitionSeparatorNT extends NonTerminal
object DefinitionNT extends NonTerminal
object FLHSNT extends NonTerminal
object ArgumentListHeadNT extends NonTerminal
object ArgumentListNT extends NonTerminal
object RHSNT extends NonTerminal
object ExpressionNT extends NonTerminal
object ExpressionPrimeNT extends NonTerminal
object SimpleExpressionNT extends NonTerminal
object OperatorNT extends NonTerminal
object StatementNT extends NonTerminal
object LetNT extends NonTerminal
object StatementSequenceNT extends NonTerminal
object StatementSequencePrimeNT extends NonTerminal
object StatementSeparatorNT extends NonTerminal
object DefinitionOrExpressionNT extends NonTerminal
object DefinitionOrExpressionRepeatIdNT extends NonTerminal
object DefinitionOrExpressionPrimeNT extends NonTerminal
object DefinitionOrExpressionPrimePrimeNT extends NonTerminal
object BlockNT extends NonTerminal
object FunctionExpressionNT extends NonTerminal
object FunctionExpressionPrimeNT extends NonTerminal
object ArgumentNT extends NonTerminal
object SimpleArgumentNT extends NonTerminal
object SimpleArgumentPrimeNT extends NonTerminal
object LiteralNT extends NonTerminal