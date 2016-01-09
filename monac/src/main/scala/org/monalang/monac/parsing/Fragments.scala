package org.monalang.monac.parsing

import org.monalang.monac.symbol.{ArgumentMarker, FunctionSymbol, SymbolTable, SymbolToAST}

object ExtraFragments {
  def functionDefinition(outerScope: SymbolTable, flhs: FLHS, expression: Expression) = {
    val name = flhs.identifier
    val function = FunctionSymbol(expression)
    // TODO has to be some update
    for (id <- flhs.arguments.arguments) expression.scope.addSymbol(id, ArgumentMarker())
    outerScope.addSymbol(name, function)
  }
}

object Fragments {
  // special
  def emptyNode(c: Context) = EmptyNode()
  def unitExpression(c: Context) = UnitExpression()
  def extract(n: Int)(c: Context) = c.elements(n-1)
  val matched = extract(1) _

  // the symbol table of the next start has to be merged into this one's, the children of the next start's
  // symbol table should have their new parent set to the this one's scope.
  // get symbol tables through children AST nodes. always propagate symbol tables upwards <-- probably in AST structure?
  def start(c: Context) = {
    val definition = List(c.elements(0).asInstanceOf[Definition])
    if (c.elements(1).isInstanceOf[EmptyNode]) DefinitionSequence(definition)
    else DefinitionSequence(c.elements(0).asInstanceOf[Definition] +: c.elements(1).asInstanceOf[DefinitionSequence].definitions)
  }

  def functionDefinition(c: Context) = {
    val flhs = c.elements(0).asInstanceOf[FLHS]
    val expression = c.elements(1).asInstanceOf[Expression]
    ExtraFragments.functionDefinition(c.parentScope, flhs, expression)
    Definition(flhs.identifier)
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