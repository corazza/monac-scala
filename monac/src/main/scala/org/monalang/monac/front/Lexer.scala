package org.monalang.monac.front

import java.io.BufferedReader
import org.monalang.monac.common.util.CharUtil

class Lexer(inputStream: BufferedReader) {
  import Lexer._

  private var rows = 1
  private var columns = 1
  private var atEnd = false
  private var current = inputStream.read().asInstanceOf[Char]
  private var next = inputStream.read().asInstanceOf[Char]
  private var readNext = true
  private var blockCommentCount = 0
  private var lineComment = false

  /**
   * Returns the next token from the inputStream.
   *
   * On match calls the appropriate token-generating function with the lexeme as
   * the only argument.
   */
  private def getNextToken(): Token = {
    // HERE
    // TODO handle comments and whitespace (insert break after newlines and potentially ;)

    var buffer = new StringBuilder("")
    var result: Token = null
    var advancing = true
    var innerRecognizers = collection.mutable.Map(recognizers.toSeq: _*)

    var didNotAcceptLast = true
    var rowsBegin = rows
    var columnsBegin = columns

    while (advancing && !atEnd) {
      if (current == '/' && next == '*') blockCommentCount += 1
      if (current == '/' && next == '/') lineComment = true

      if (blockCommentCount == 0 && lineComment == false) {
        innerRecognizers.keys.foreach(_.advance(current))

        val accepting = innerRecognizers.keys.filter(_.phase == FSAPhase.Accepting)
        val continuing = innerRecognizers.keys.filter(_.phase == FSAPhase.Continuing)

        if (accepting.size == 1 && continuing.size == 0) {
          val accepted = accepting.head
          val lexeme = new Lexeme(buffer.toString, rowsBegin, columnsBegin)
          val construction = innerRecognizers.get(accepted).get
          result = construction(lexeme)
          advancing = false
          readNext = false
        } else {
          val notBroken = innerRecognizers.keys.filter(_.phase == FSAPhase.Continuing).toList
          if (notBroken.size == 0) {
            buffer = new StringBuilder("")
            recognizers.keys.foreach(_.reset())
            innerRecognizers = collection.mutable.Map(recognizers.toSeq: _*)
            didNotAcceptLast = true
          } else {
            innerRecognizers = innerRecognizers.filter(p => notBroken.contains(p._1))
            if (didNotAcceptLast) {
              rowsBegin = rows
              columnsBegin = columns
            }
            buffer.append(current)
            didNotAcceptLast = false
          }
        }
      }

      if (readNext) {
        if (current == '\n') {
          lineComment = false
          rows += 1
          columns = 0
        } else {
          columns += 1
        }

        var skip = false
        if (current == '*' && next == '/' && blockCommentCount > 0) {
          blockCommentCount -= 1
          skip = true
        }

        if (!skip) current = next
        else current = inputStream.read().asInstanceOf[Char]
        next = inputStream.read().asInstanceOf[Char]

        if (current == (-1).asInstanceOf[Char]) {
          advancing = false
          atEnd = true
          result = EndOfSource
        }
      } else readNext = true
    }

    recognizers.keys.foreach(_.reset())
    result
  }

  /**
   * A stream object containing the tokens from the source code.
   */
  val tokenStream = Stream.continually(getNextToken)
}

object Lexer {
  /**
   * A map of finite state automatons constructed from regular expressions that
   * analyze the input and identify a corresponding token construction function.
   *
   * special characters:
   *
   * classes (encompass all characters, no crossover):
   * L - letters
   * D - digits
   * S - ASCII special characters except newline (with period)
   * U - unicode special characters
   *
   * specific characters:
   * P - period
   * E - newline
   * O - open parens
   * C - closing parens
   * V - vertical line
   * K - asterisk
   *
   * A - any character except newline
   *
   * additional operators:
   * [NOT IMPLEMENTED] cN - not character (c), accepts characters from all other classes
   */
  val special = " P,/!#$%^&KOC_+=\\V~`<>?-\"\'"
  def toUnion(chars: String): String = chars.toList.map(_ + "|").foldLeft("")((buff, a) => buff + a).init
  val quoteInner = toUnion(special.filter(_ != '"'))
  val escapeInner = toUnion(special.filter(_ != '\\'))
  val stringInner = "(\\A|L|D|U|(" + quoteInner + "))*"
  val recognizers = Map(
    FSA("L(L|D)*") -> Identifier,
    FSA("SS*") -> Identifier,
    FSA("DD*") -> IntegerNumeral,
    FSA("DD*PDD*") -> FloatNumeral,
    FSA("\"" + stringInner + "\"") -> StringLiteral,
    FSA("\"\"\"" + stringInner + "\"\"\"") -> StringLiteral,
    FSA("'(\\A|(L|D|U|(" + escapeInner + ")))'") -> CharacterLiteral)
}
