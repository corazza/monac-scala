package org.monalang.monac.front

import java.io.BufferedReader
import org.monalang.monac.common.util.CharUtil

class Lexer(inputStream: BufferedReader) {
  private var rows = 0
  private var columns = 0

  // token construction functions

  private def identifierMatched(lexeme: Lexeme): Token = {
    new Identifier(lexeme)
  }

  /**
   * A map of finite state automatons constructed from regular expressions that
   * analyze the input and identify a corresponding token construction function.
   */
  // TODO (1) create LexerMeta with name->regex pairs used both by the 
  // lexer preprocessor and compiler
  private val recognizers = Map(FSA("(C|S)(C|S|D)*") -> identifierMatched _)

  /**
   * Returns the next token from the inputStream.
   *
   * On match calls the appropriate token-generating function with the lexeme as
   * the only argument.
   */
  private def getNextToken(): Token = {
    var result: Token = null

    var advancing = true
    var buffer: StringBuilder = new StringBuilder("")

    while (advancing) {
      inputStream.mark(5) // usually only a single character will have to be redacted

      var c = inputStream.read().asInstanceOf[Char]
      buffer.append(c)

      if (c == '\n') {
        rows += 1
        columns = 0
      } else {
        columns += 1
      }

      // all whitespace characters are treated the same (except for tracking)
      if (CharUtil.isWhitespace(c)) c = ' '

      recognizers.keys.foreach(_.advance(c))

      try {
        val accepted = recognizers.keys.filter(_.accepting).head
        val lexeme = new Lexeme(buffer.substring(0, buffer.length - 1), rows, columns)
        val construction = recognizers.get(accepted).get
        result = construction(lexeme)
        advancing = false
        inputStream.reset()
      } catch {
        case e: Exception => {}
      }

      // check returns
      // check retraction
    }

    result
  }

  /**
   * A stream object containing the tokens from the source code.
   */
  val tokenStream = Stream.continually(getNextToken)
}
