package org.monalang.monac.parsing

import org.monalang.monac.lexing._
import org.monalang.monac.parsing

/*

TODO:

1. Basic function declarations with n non-pattern parameters and expression blocks (inner declarations allowed)
2. Parsing with context functionality and IR capability (AST's for now, stored in scope)
3. Rest of the grammar

 */

object MonaGrammar extends Grammar(List(
  Start -> List(Repeat(Declaration)) -> Fragments.start,

  Declaration -> List(FLHS, RHS) -> Fragments.functionDeclaration,

  FLHS -> List(Terminal[LowerId](), Terminal[LowerId](), Repeat(Terminal[LowerId]())) -> Fragments.FLHS,

  RHS -> List(Terminal[EqualsSign](), Expression) -> Fragments.RHS,

  Expression -> List(FunctionExpression) -> ((c)=>{ EmptyNode() /* extract FunctionExpression AST */ }),
  Expression -> List(Terminal[KeywordIf](), Expression, Terminal[KeywordThen](), Expression, Terminal[KeywordElse](), Expression) -> Fragments.ifExpression,

  FunctionExpression -> List(Optional(List(FunctionExpression)), Argument) -> Fragments.functionExpression,
  Argument -> List(Terminal[LowerId]()) -> Fragments.bindingExpression,
  Argument -> List(Literal) -> Fragments.literalExpression,
  Argument -> List(Terminal[OpenParens](), Expression, Terminal[CloseParens]()) -> ((c)=>{ EmptyNode() /* extract Expression AST */ }),
  Argument -> List(Terminal[OpenBlock](), Repeat(Expression), Terminal[CloseBlock]()) -> ((c)=>{ EmptyNode() /* extract Expression AST */ })

//  Expression -> List(Terminal[BeginLambda](), Terminal[LowerId](), Repeat(Terminal[LowerId]()), Terminal[FunctionArrow](), Expression) -> Fragments.lambda,
//  Argument -> List(Terminal[OpenParens](), Expression, Terminal[CloseParens]()) -> ((c)=>EmptyNode()),
//  Argument -> List(Literal) -> ((c)=>EmptyNode()),
//
//  Constraints -> List() -> Fragments.constraints,
//
//  // as-pattern
//  Pattern -> List(Terminal[LowerId], Optional(List(Terminal[At](), Pattern))) -> Fragments.asPattern,
//
//  Pattern -> List(Literal) -> Fragments.literal,
//  Pattern -> List(Terminal[Underscore]()) -> Fragments.underscore,
//  Pattern -> List(Terminal[OpenParens](), Pattern, Terminal[CloseParens]()) -> ((c)=>EmptyNode()),
//
//  // binding
//  Assignment -> List(Pattern, RHS) -> Fragments.functionDeclaration,
//
//  RHS -> List(GDRHS) -> Fragments.RHS,
//
//  GDRHS -> List(Guard, Terminal[EqualsSign](), Expression, Repeat(GDRHS)) -> Fragments.RHS,
//  Guard -> List(Terminal[Vertical](), Expression),
))

object Fragments {
  def start(c: Context) = {
    EmptyNode()
  }

  def functionDeclaration(c: Context) = {
    EmptyNode()
  }

  def FLHS(c: Context) = {
    EmptyNode()
  }

  def RHS(c: Context) = {
    EmptyNode()
  }

  def functionExpression(c: Context) = {
    EmptyNode()
  }

  def ifExpression(c: Context) = {
    EmptyNode()
  }

  def expressionBlock(c: Context) = {
    EmptyNode()
  }

  def bindingExpression(c: Context) = {
    EmptyNode()
  }

  def literalExpression(c: Context) = {
    EmptyNode()
  }

//  def lambda(c: Context) = {
//    println("Lambda production")
//    EmptyNode()
//  }
//
//
//  def constraints(c: Context) = {
//    println("Constraints production")
//    EmptyNode()
//  }
//
//  def asPattern(c: Context) = {
//    EmptyNode()
//  }
//
//  def literal(c: Context) = {
//    EmptyNode()
//  }
//
//  def underscore(c: Context) = {
//    EmptyNode()
//  }
}

object Start extends NonTerminal
object Declaration extends NonTerminal
object FLHS extends NonTerminal
object RHS extends NonTerminal
object Expression extends NonTerminal
object Block extends NonTerminal
object FunctionExpression extends NonTerminal
object Argument extends NonTerminal
// TODO fix literals in the lexer
object Literal extends NonTerminal

object GDRHS extends NonTerminal
object Guard extends NonTerminal
object Assignment extends NonTerminal
object Pattern extends NonTerminal
object Constraints extends NonTerminal
object Class extends NonTerminal
