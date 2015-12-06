package org.monalang.monac.parsing

import org.monalang.monac.lexing.Token

class Parser() {
  def parse(tokens: Stream[Token]) = {
    val rd = new RecursiveDescent(MonaGrammar, tokens)
  }
}