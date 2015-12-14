package org.monalang.monac.parsing

import org.monalang.monac.lexing._
import scala.reflect._

/*

TODO:

1. Basic function declarations with n non-pattern parameters and expression blocks (inner declarations allowed)
2. Parsing with context functionality and IR capability (AST's for now, stored in scope)
3. Rest of the grammar

 */

object MonaGrammar extends Grammar(List(
  Start -> List(Repeat(Declaration)) -> Fragments.start,

  Declaration -> List(FLHS, RHS) -> Fragments.functionDeclaration,

  FLHS -> List(Terminal(classTag[LowerId]), ArgumentListHead) -> Fragments.FLHS,
  ArgumentListHead -> List(Terminal(classTag[LowerId]), ArgumentList) -> Fragments.argumentListHead,
  ArgumentList -> List(ArgumentListHead) -> Fragments.argumentList,
  ArgumentList -> List(EtaProduction)  -> Fragments.argumentList,

  RHS -> List(Terminal(classTag[EqualsSign]), Expression) -> Fragments.RHS,

  Expression -> List(FunctionExpression) -> ((c)=>{ EmptyNode() /* extract FunctionExpression AST */ }),
  Expression -> List(Terminal(classTag[KeywordIf]), Expression, Terminal(classTag[KeywordThen]), Expression, Terminal(classTag[KeywordElse]), Expression) -> Fragments.ifExpression,

  FunctionExpression -> List(Optional(List(FunctionExpression)), Argument) -> Fragments.functionExpression,
  Argument -> List(Terminal(classTag[LowerId])) -> Fragments.bindingExpression,
  Argument -> List(Literal) -> Fragments.literalExpression,
  Argument -> List(Terminal(classTag[OpenParens]), Expression, Terminal(classTag[CloseParens])) -> ((c)=>{ EmptyNode() /* extract Expression AST */ }),
  Argument -> List(Terminal(classTag[OpenBlock]), Repeat(Expression), Terminal(classTag[CloseBlock])) -> ((c)=>{ EmptyNode() /* extract Expression AST */ })
))

object Start extends NonTerminal
object Declaration extends NonTerminal
object FLHS extends NonTerminal
object ArgumentListHead extends NonTerminal
object ArgumentList extends NonTerminal
object RHS extends NonTerminal
object Expression extends NonTerminal
object Block extends NonTerminal
object FunctionExpression extends NonTerminal
object Argument extends NonTerminal
// TODO fix literals in the lexer
object Literal extends NonTerminal