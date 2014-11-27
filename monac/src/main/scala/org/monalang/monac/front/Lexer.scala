package org.monalang.monac.front

import java.io.BufferedReader

class Lexer(inputStream: BufferedReader) {
  // several different regexps can lead to the same token class being created
  // e.g. "[aZ]*" -> Literal, """ """ -> Literal (scanned differently, but have
  // the same internal representation

  private def getNextToken(): Token = {
    new Numerical(new Lexeme("123", (0, 0)))
  }

  def tokenStream = Stream.continually(getNextToken)
}

object Lexer {
  // Conversions
  // TODO handle different formats

  /**
   * Implements the conversion between Mona numerical lexeme format to internal
   * compiler representation (BigInt).
   *
   * Restrictions are imposed later by machine-specific code generation phase.
   */
  def lexemeToNumerical(lexemeData: String): BigInt = BigInt(lexemeData)

  /**
   * Implements the conversion between Mona literal format to internal compiler
   * representation (String).
   */
  def lexemeToLiteral(lexemeData: String): String = {
    // All characters treated literally, functions with newlines etc.
    if (lexemeData.startsWith("\"\"\""))
      lexemeData.stripPrefix("\"\"\"").stripSuffix("\"\"\"")
    else
      //TODO convert control characters etc
      lexemeData.stripPrefix("\"").stripSuffix("\"")
  }
}