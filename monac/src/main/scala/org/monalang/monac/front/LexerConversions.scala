package org.monalang.monac.front

object LexerConversions {
  // Conversions
  // TODO handle different formats

  /**
   * Implements the conversion between Mona numerical  lexeme format to internal
   * compiler representation (BigInt).
   *
   * Restrictions are imposed later by machine-specific code generation phase.
   */
  def lexemeToNumeral(lexemeData: String): BigInt = BigInt(lexemeData)

  /**
   * Implements the conversion between Mona literal format to internal compiler
   * representation (String).
   */
  // TODO handle escaping symbols
  def lexemeToLiteral(lexemeData: String): String = {
    // All characters treated literally, functions with newlines etc.
    if (lexemeData.startsWith("\"\"\""))
      lexemeData.stripPrefix("\"\"\"").stripSuffix("\"\"\"")
    else
      //TODO convert control characters etc
      lexemeData.stripPrefix("\"").stripSuffix("\"")
  }
}