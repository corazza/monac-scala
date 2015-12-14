package org.monalang.monac.parsing

import org.monalang.monac.lexing.Token
import org.monalang.monac.symbol.SymbolTable

import scala.reflect.ClassTag

/**
  * Contains information about the context of a production.
  *
  *  - elements of production (for creating the AST / further processing)
  */
class Context(parentScope: SymbolTable, elements: List[ASTNode]) {

}

// TODO precompute FSA's

class Grammar(rules: List[((NonTerminal, List[Symbol]), Context=>ASTNode)]) {
  def processProduction(production: ((NonTerminal, List[Symbol]), Context=>ASTNode)) = production match {
    case ((nt, to), fragment) => to -> fragment
  }

  val ntmp = (rules groupBy { case ((from, _), _) => from }) map { case (nt, productions) => nt -> productions.map(processProduction _) }
}

abstract class Symbol
object EtaProduction extends Symbol
case class Terminal[A <: Token](token: ClassTag[A]) extends Symbol

abstract class NonTerminal extends Symbol

// operators

case class Optional(symbols: List[Symbol]) extends Symbol
case class Repeat(symbol: Symbol) extends Symbol
case class RepeatP(symbol: Symbol) extends Symbol