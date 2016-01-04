package org.monalang.monac.parsing

object Fragments {
  // special
  def emptyNode(c: Context) = EmptyNode()
  def extract(n: Int)(c: Context) = c.elements(n-1)
  val matched = extract(1)_

  // production-specific
  def start(c: Context) = {
    EmptyNode()
  }

  def functionDeclaration(c: Context) = {

    // create map of what to do with every symbol in c.elements?

    EmptyNode()
  }

  def FLHS(c: Context) = {
    EmptyNode()
  }

  def argumentListHead(c: Context) = {
    EmptyNode()
  }

  def argumentList(c: Context) = {
    EmptyNode()
  }

  def RHS(c: Context) = {
    EmptyNode()
  }

  def infix(c: Context) = {
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

  def declarationOrFE(c: Context) = {
    // return combine PRIME and repeat
    EmptyNode()
  }

  def repeatId(c: Context) = {
    EmptyNode()
  }
}