package org.monalang.monac.front

class Parser() {
  def getC(tokens: Stream[Token]): String = {
    val fileScopeSymbolTable = new SymbolTable(Some(InitialSymbolTable))
    // val fileStatements = new StatementSequence()
    // create file-scope
    // create file sequence
    // takeStatement until EndOfSource
    ""
  }
}