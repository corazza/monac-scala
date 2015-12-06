package org.monalang.monac.symbol

import org.monalang.monac.types.{CharAT, IntATunsigned32}

object InitialSymbolTable extends SymbolTable(None) {
  addSymbol("Int", ConcreteTypeName(IntATunsigned32))
  addSymbol("Char", ConcreteTypeName(CharAT))
}