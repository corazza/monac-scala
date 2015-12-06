package org.monalang.monac.parsing

import org.monalang.monac.lexing.Token

/**
  * Engine for parsing grammars recursively.
  */
class RecursiveDescent(grammar: Grammar, tokens: Stream[Token]) {
  // approach: parsing functions manage ASTs, which can then be compiled
  // and discarded as needed (if independent)
}