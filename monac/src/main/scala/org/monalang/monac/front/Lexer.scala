package org.monalang.monac.front

import java.io.BufferedReader
import org.monalang.monac.common.util.CharUtil

class Lexer(inputStream: BufferedReader) {
  private var rows = 0
  private var columns = 0

  /**
   * A map of finite state automatons constructed from regular expressions that
   * analyze the input and identify a corresponding token construction function.
   * 
   * special characters:
   * 
   * L - letters
   * D - digits
   * S - special characters
   * A - any character
   * P - period
   */
  private val recognizers = Map(
    FSA("(L)(L|D)*") -> Identifier,    
    FSA("SS*") -> Identifier,
    FSA("DD*") -> Numeral,
    FSA("DD*PDD*") -> Numeral,
    FSA("\"A*\"") -> Literal,
    FSA("\"\"\"A*\"\"\"") -> Literal
  )

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
      inputStream.mark(5) // usually only a single character will have to be retracted

      var c = inputStream.read().asInstanceOf[Char]
      buffer.append(c)

      // track position
      if (c == '\n') {
        rows += 1
        columns = 0
      } else {
        columns += 1
      }

      // all whitespace characters are treated the same (except for tracking)
      if (c.isWhitespace) c = ' '

      recognizers.keys.foreach(_.advance(c))

      try {
        val notBroken = recognizers.keys.filter(_.notBroken)
        val accepting = recognizers.keys.filter(_.accepting)
        if (notBroken.length == 0 && accepting.length != 0) { 
          val accepted = accepting.head
          val lexeme = new Lexeme(buffer.toString.init, rows, columns)
          val construction = recognizers.get(accepted).get
          result = construction(lexeme)
          advancing = false
          inputStream.reset()
          recognizers.keys.foreach(_.reset)
        }
      } catch {
        case e: Exception => {}
      }
    }

    result
  }

  /**
   * A stream object containing the tokens from the source code.
   */
  val tokenStream = Stream.continually(getNextToken)
}
