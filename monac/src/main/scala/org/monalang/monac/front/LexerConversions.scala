package org.monalang.monac.front

object LexerConversions {
  // Conversions

  /**
   * Implements the conversion between Mona integer lexeme format to internal
   * compiler representation (BigInt).
   *
   * Restrictions are imposed later by machine-specific code generation phase.
   */
  def lexemeToIntegerNumeral(lexemeData: String): BigInt = BigInt(lexemeData)

  // TODO some float representation formal (from multiple expressions -> universal)
  def lexemeToFloatNumeral(lexemeData: String): String = lexemeData
  
  /**
   * Implements the conversion between Mona literal format to internal compiler
   * representation (String).
   */
  // TODO handle escaping symbols (only in single-quoted strings)
  def lexemeToStringLiteral(lexemeData: String): String = {
    // All characters treated literally, functions with newlines etc.
    if (lexemeData.startsWith("\"\"\""))
      lexemeData.stripPrefix("\"\"\"").stripSuffix("\"\"\"")
    else
      //TODO convert control characters etc
      lexemeData.stripPrefix("\"").stripSuffix("\"")
  }
  
  def lexemeToCharacterLiteral(lexemeData: String): Char = lexemeData.toCharArray()(1)
}