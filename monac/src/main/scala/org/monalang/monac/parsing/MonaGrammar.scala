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

  ExpressionNT -> List(FunctionExpressionNT, ExpressionPrimeNT) -> Fragments.unitExpression,
  ExpressionNT -> List(SimpleExpressionNT, ExpressionPrimeNT) -> Fragments.emptyNode,
  ExpressionPrimeNT -> List(Eta) -> Fragments.emptyNode,
  ExpressionPrimeNT -> List(OperatorNT, ExpressionNT) -> Fragments.infix,

  OperatorNT -> List(Terminal(classTag[OperatorId])) -> Fragments.matched,
  OperatorNT -> List(Terminal(classTag[Backtick]), Terminal(classTag[LowerIdToken]), Terminal(classTag[Backtick])) -> Fragments.extract(2),

  SimpleExpressionNT -> List(Terminal(classTag[KeywordIf]), ExpressionNT, Terminal(classTag[KeywordThen]), ExpressionNT, Terminal(classTag[KeywordElse]), ExpressionNT) -> Fragments.ifExpression,

  FunctionExpressionNT -> List(ArgumentNT, FunctionExpressionPrimeNT) -> Fragments.functionExpression,
  FunctionExpressionPrimeNT -> List(FunctionExpressionNT) -> Fragments.functionExpression,
  FunctionExpressionPrimeNT -> List(Eta) -> Fragments.emptyNode,

  ArgumentNT -> List(Terminal(classTag[LowerIdToken])) -> Fragments.bindingExpression,
  ArgumentNT -> List(SimpleArgumentNT) -> Fragments.extract(1),
  SimpleArgumentNT -> List(LiteralNT) -> Fragments.extract(1),
  SimpleArgumentNT -> List(Terminal(classTag[OpenParens]), OptionalNewlinesNT, ExpressionNT, OptionalNewlinesNT, Terminal(classTag[CloseParens])) -> ((c)=>{ EmptyNode() /* extract Expression AST */ }),
  SimpleArgumentNT -> List(Terminal(classTag[OpenBlock]), OptionalNewlinesNT, StatementSequenceNT, Terminal(classTag[CloseBlock])) -> ((c)=>{ EmptyNode() /* extract Expression AST */ }),

  StatementSequenceNT -> List(Eta) -> Fragments.emptyNode,
  StatementSequenceNT -> List(StatementNT, StatementSequencePrimeNT) -> Fragments.emptyNode,
  StatementSequencePrimeNT -> List(Eta) -> Fragments.emptyNode,
  StatementSequencePrimeNT -> List(StatementSeparatorNT, StatementSequenceNT) -> Fragments.emptyNode,

  StatementSeparatorNT  -> List(Terminal(classTag[Newlines])) -> Fragments.emptyNode,
  StatementSeparatorNT  -> List(Terminal(classTag[SemiColon])) -> Fragments.emptyNode,

  StatementNT -> List(DefinitionOrExpressionNT) -> Fragments.extract(1),
  StatementNT -> List(SimpleExpressionNT) -> Fragments.extract(1),

  DefinitionOrExpressionNT -> List(SimpleArgumentNT) -> Fragments.extract(1),
  DefinitionOrExpressionNT -> List(Terminal(classTag[LowerIdToken]), DefinitionOrExpressionRepeatIdNT, DefinitionOrExpressionPrimeNT) -> Fragments.emptyNode,
  DefinitionOrExpressionRepeatIdNT -> List(Eta) -> Fragments.emptyNode,
  DefinitionOrExpressionRepeatIdNT -> List(Terminal(classTag[LowerIdToken]), DefinitionOrExpressionRepeatIdNT) -> Fragments.repeatId,
  DefinitionOrExpressionPrimeNT -> List(Eta) -> Fragments.emptyNode,
  DefinitionOrExpressionPrimeNT -> List(SimpleArgumentNT, DefinitionOrExpressionPrimePrimeNT) -> Fragments.emptyNode, // wrap in a special node
  DefinitionOrExpressionPrimePrimeNT -> List(FunctionExpressionPrimeNT) -> Fragments.emptyNode,
  DefinitionOrExpressionPrimePrimeNT -> List(OperatorNT, ExpressionNT) -> Fragments.emptyNode,
  DefinitionOrExpressionPrimeNT -> List(RHSNT) -> Fragments.extract(1),

  LiteralNT -> List(Terminal(classTag[NumLiteral])) -> Fragments.matched,
  LiteralNT -> List(Terminal(classTag[CharLiteral])) -> Fragments.matched,
  LiteralNT -> List(Terminal(classTag[StringLiteral])) -> Fragments.matched
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
object StatementOtherNT extends NonTerminal
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
object LiteralNT extends NonTerminal