package org.monalang.monac.parsing

import org.monalang.monac.symbol._

object Fragments {
  def emptyNode(c: Context) = EmptyNode()
  def unitExpression(c: Context) = UnitExpression()
  def extract(n: Int)(c: Context) = c.elements(n-1)
  val matched = extract(1) _

  def start(c: Context) = {
    val definition = c.elements(0).asInstanceOf[Definition]
    if (c.elements(1).isInstanceOf[EmptyNode]) {
      val scope = new SymbolTable()
      scope.addSymbol(definition.name, definition.symbol)
      DefinitionSequence(scope, List(definition))
    }
    else {
      val sequence = c.elements(1).asInstanceOf[DefinitionSequence]
      val scope = sequence.scope
      scope.addSymbol(definition.name, definition.symbol)
      DefinitionSequence(scope, definition +: sequence.definitions)
    }
  }

  def functionDefinition(c: Context) = {
    val flhs = c.elements(0).asInstanceOf[FLHS]
    val expression = c.elements(1).asInstanceOf[Expression]
    for (id <- flhs.arguments.arguments) expression.scope.addSymbol(id, ArgumentMarker())
    Definition(flhs.identifier, SymbolToAST(expression))
  }

  def FLHSNT(c: Context) = FLHS(c.elements(0).asInstanceOf[LowerId].lexeme.data, c.elements(1).asInstanceOf[ArgumentList])

  def argumentListHead(c: Context) = if (c.elements(1).isInstanceOf[EmptyNode]) ArgumentList(List(c.elements(0).asInstanceOf[LowerId].lexeme.data))
                                     else ArgumentList(c.elements(0).asInstanceOf[LowerId].lexeme.data +: c.elements(1).asInstanceOf[ArgumentList].arguments)
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