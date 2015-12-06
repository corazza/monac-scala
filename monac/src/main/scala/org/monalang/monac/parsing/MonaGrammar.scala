package org.monalang.monac.parsing

import org.monalang.monac.lexing._

/*

TODO:

1. Basic function declarations with n non-pattern parameters and expression blocks (inner declarations allowed)
2. Parsing with context functionality and IR capability (AST's for now, stored in scope)
3. Rest of the grammar

 */

object MonaGrammar extends Grammar(List(
  Variable -> List(Terminal[LowerId]()) -> ((c)=>EmptyNode()),

  Expression -> List(Terminal[BeginLambda](), Terminal[LowerId](), Repeat(Terminal[LowerId]()), Terminal[FunctionArrow](), Expression) -> Fragments.lambda,
  Expression -> List(Call) -> Fragments.call,

  Call -> List(Optional(List(Call)), Argument) -> Fragments.call,

  Argument -> List(Terminal[OpenParens](), Expression, Terminal[CloseParens]()) -> ((c)=>EmptyNode()),
  Argument -> List(Literal) -> ((c)=>EmptyNode()),

  Constraints -> List() -> Fragments.constraints,

  // as-pattern
  Pattern -> List(Variable, Optional(List(Terminal[At](), Pattern))) -> Fragments.asPattern,

  Pattern -> List(Literal) -> Fragments.literal,
  Pattern -> List(Terminal[Underscore]()) -> Fragments.underscore,
  Pattern -> List(Terminal[OpenParens](), Pattern, Terminal[CloseParens]()) -> ((c)=>EmptyNode()),

  // function declaration
  Declaration -> List(FLHS, RHS) -> Fragments.functionDeclaration,

  // binding

  Assignment -> List(Pattern, RHS) -> Fragments.functionDeclaration,

  FLHS -> List(Variable, Pattern, Repeat(Pattern)) -> Fragments.FLHS
))

object Fragments {
  def functionDeclaration(c: Context) = {
    EmptyNode()
  }

  def asPattern(c: Context) = {
    EmptyNode()
  }

  def literal(c: Context) = {
    EmptyNode()
  }

  def underscore(c: Context) = {
    EmptyNode()
  }

  def FLHS(c: Context) = {
    // process patterns
    // connect arguments to expression bindings
    // emit IR for the expression, discard tree
    // add id to parent symtable

    EmptyNode()
  }

  def lambda(c: Context) = {
    println("Lambda production")
    EmptyNode()
  }

  def call(c: Context) = {
    println("Expression production")
    EmptyNode()
  }

  def constraints(c: Context) = {
    println("Constraints production")
    EmptyNode()
  }
}


object Declaration extends NonTerminal
object Assignment extends NonTerminal
object Pattern extends NonTerminal
object FLHS extends NonTerminal
object RHS extends NonTerminal

object Variable extends NonTerminal
object Expression extends NonTerminal
object Call extends NonTerminal
object Argument extends NonTerminal
object Constraints extends NonTerminal
object Class extends NonTerminal

object Literal extends NonTerminal