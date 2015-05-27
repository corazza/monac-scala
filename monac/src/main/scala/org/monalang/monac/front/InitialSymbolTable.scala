package org.monalang.monac.front

import org.monalang.monac.types.IntATunsigned32
import org.monalang.monac.types.CharAT

object InitialSymbolTable extends SymbolTable(None) {
  addSymbol("Int", ConcreteTypeName(IntATunsigned32))
  addSymbol("Char", ConcreteTypeName(CharAT))
}