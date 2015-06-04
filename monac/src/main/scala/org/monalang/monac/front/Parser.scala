package org.monalang.monac.front

// HERE
// problem - definition syntax (functions - shorthand for values, values, variables)
// look in the notebook

class Parser() {
  def getC(tokens: Stream[Token]): String = {
    val fileScopeSymbolTable = new SymbolTable(Some(InitialSymbolTable))
    val fileStatements = new StatementSequence()
    // create file-scope
    // create file sequence
    // takeStatement until EndOfSource
    ""
  }
}