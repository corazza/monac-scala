package org.monalang.monac.parsing

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