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

  def FLHSNT(c: Context) =
    FLHS(c.elements(0).asInstanceOf[LowerId].lexeme.data, c.elements(1).asInstanceOf[ArgumentList])

  def argumentListHead(c: Context) =
    if (c.elements(1).isInstanceOf[EmptyNode]) ArgumentList(List(c.elements(0).asInstanceOf[LowerId].lexeme.data))
    else ArgumentList(c.elements(0).asInstanceOf[LowerId].lexeme.data +: c.elements(1).asInstanceOf[ArgumentList].arguments)

  def expression(c: Context) =
    if (c.elements(1).isInstanceOf[EmptyNode]) c.elements(0)
    else {
      val expression1 = c.elements(0).asInstanceOf[Expression]
      val InfixRight(operator, expression2) = c.elements(1).asInstanceOf[InfixRight]
      // TODO operator fixity
      // TODO scoping
      val firstCall = FunctionApplication(new SymbolTable(), BindingExpression(new SymbolTable(), operator), expression1)
      FunctionApplication(new SymbolTable(), firstCall, expression2)
    }

  def infix(c: Context) =
    InfixRight(c.elements(0).asInstanceOf[Operator], c.elements(1).asInstanceOf[Expression])

  def functionExpression(c: Context) = {
    UnitExpression()
  }

  def ifExpression(c: Context) = {
    UnitExpression()
  }

  def expressionBlock(c: Context) = {
    UnitExpression()
  }

  def declarationOrFE(c: Context) = {
    // return combine PRIME and repeat
    EmptyNode()
  }

  def repeatId(c: Context) = {
    EmptyNode()
  }
}