package org.monalang.monac.parsing

import org.monalang.monac.lexing.Token
import org.monalang.monac.symbol.SymbolTable

class Parser() {
  def parse(tokens: Stream[Token]) = {
    val rd = new RecursiveDescent(MonaGrammar, tokens)
    val fileScope = new SymbolTable(None)
    rd.parse(Start, fileScope)
  }
}