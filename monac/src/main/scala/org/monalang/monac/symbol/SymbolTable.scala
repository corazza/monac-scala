package org.monalang.monac.symbol

class SymbolTable() {
  var parent: Option[SymbolTable] = None
  val lookupTable = collection.mutable.Map[String, Symbol]()

  def lookup(name: String): Option[Symbol] = lookupTable.get(name) match {
    case None => if (parent != None) parent.get.lookup(name) else None
    case Some(symbol) => Some(symbol)
  }

  def addSymbol(name: String, symbol: Symbol) {
    lookupTable += name -> symbol

    println("adding " + name + ", " + symbol)
  }
}