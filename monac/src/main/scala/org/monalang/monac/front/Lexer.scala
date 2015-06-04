package org.monalang.monac.front

import java.io.BufferedReader
import org.monalang.monac.common.util.CharUtil
import scala.collection.mutable.Queue
import com.sun.org.apache.xpath.internal.functions.FuncBoolean
import org.monalang.monac.front.FunctionArrow

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
  private val tokenQueue = new Queue[Token]()

  /**
   * Returns the next token from the inputStream.
   *
   * On match calls the appropriate token-generating function with the lexeme as
   * the only argument.
   */
  private def getNextToken(): Token = {
    var buffer = new StringBuilder("")
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
          tokenQueue += construction(lexeme)
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
        var addBreak = false
        if (current == '\n') {
          addBreak = true
          lineComment = false
          rows += 1
          columns = 0
        } else {
          columns += 1
        }

        if (addBreak && (tokenQueue.length > 0 && tokenQueue.last != BreakStatement || tokenQueue.length == 0))
          tokenQueue += InsertedBreakStatement

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
          tokenQueue += EndOfSource
        }
      } else readNext = true
    }

    recognizers.keys.foreach(_.reset())
    tokenQueue.dequeue()
  }

  /**
   * A stream object containing the tokens from the source code.
   */
  val tokenStream = Stream.continually(getNextToken)
}

object Lexer {
  // classes
  val special = "W;:{}[]P,/!#$%^&KOC_+=\\V~`<>?-\"\'"

  // construction functions
  def integerNumeral(lexeme: Lexeme): Token = IntegerNumeral(lexeme)
  def floatNumeral(lexeme: Lexeme): Token = FloatNumeral(lexeme)
  def stringLiteral(lexeme: Lexeme): Token = StringLiteral(lexeme)
  def characterLiteral(lexeme: Lexeme): Token = CharacterLiteral(lexeme)
  def identifier(lexeme: Lexeme): Token = Identifier(lexeme)
  // combinations of special characters are either purely syntactic elements, or identifiers
  def specialConstructor(lexeme: Lexeme): Token = lexeme.data match {
    case "->" => FunctionArrow(lexeme)
    case "{" => OpenBlock(lexeme)
    case "}" => CloseBlock(lexeme)
    case "[" => OpenList(lexeme)
    case "]" => CloseList(lexeme)
    case ":" => StatementType(lexeme)
    case "=" => EqualsSign(lexeme)
    case ";" => BreakStatement(lexeme)
    case _ => Identifier(lexeme)
  }

  // helpers for constructing regexes
  def without(string: String, cs: Char*) = string.filter(c => cs.forall(_ != c))
  def toUnion(chars: String): String = chars.toList.map(_ + "|").foldLeft("")((buff, a) => buff + a).init

  val specialInner = toUnion(without(special, 'W'))
  val characterInner = toUnion(without(special, '\\'))
  val stringInner = "(\\A|L|D|U|(" + toUnion(without(special, '"')) + "))*"

  /**
   * A map of finite state automatons constructed from regular expressions that
   * analyze the input and identify a corresponding token construction function.
   */
  val recognizers = Map(
    FSA("L(L|D)*") -> identifier _,
    FSA("(" + specialInner + ") (" + specialInner + ")*") -> specialConstructor _,
    FSA("DD*") -> integerNumeral _,
    FSA("DD*PDD*") -> floatNumeral _,
    FSA("\"" + stringInner + "\"") -> stringLiteral _,
    FSA("\"\"\"" + stringInner + "\"\"\"") -> stringLiteral _,
    FSA("'(\\A|(L|D|U|(" + characterInner + ")))'") -> characterLiteral _)
}
