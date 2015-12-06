package org.monalang.monac.parsing

import org.monalang.monac.lexing.Token

/**
  * Contains information about the context of a production.
  *
  *  - elements of production (for creating the AST / further processing)
  *  - parent symbol table (for inserting definitions)
  */
class Context {

}

class Grammar(rules: List[((NonTerminal, List[Symbol]), Context=>ASTNode)]) {
  def processProduction(production: ((NonTerminal, List[Symbol]), Context=>ASTNode)) = production match {
    case ((nt, to), fragment) => to -> fragment
  }

  val ntmp = (rules groupBy { case ((from, _), _) => from }) map { case (nt, productions) => nt -> productions.map(processProduction _) }


}

abstract class Symbol
case class Terminal[A <: Token]() extends Symbol
class NonTerminal extends Symbol
case class Keyword(name: String) extends NonTerminal

// operators

case class Optional(symbols: List[Symbol]) extends Symbol
case class Repeat(symbol: Symbol) extends Symbol