package org.monalang.monac.front

import java.io.BufferedReader

import scala.collection.mutable.Queue

class Lexer(inputStream: BufferedReader) {
  import Recognizer._

  private val tokenQueue = new Queue[Token]()
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
    var buffer = new StringBuilder("")
    var advancing = true
    var innerRecognizers = collection.mutable.Map(recognizers.toSeq: _*)

    var didNotAcceptLast = true
    var rowsBegin = rows
    var columnsBegin = columns

    while (advancing && !atEnd) {
      if (current == '/' && next == '*') blockCommentCount += 1
      if (current == '/' && next == '/') lineComment = true

      if (blockCommentCount == 0 && !lineComment) {
        innerRecognizers.keys.foreach(_.advance(current))

        val accepting = innerRecognizers.keys.filter(_.phase == FSAPhase.Accepting)
        val continuing = innerRecognizers.keys.filter(_.phase == FSAPhase.Continuing)

        if (accepting.size == 1 && continuing.isEmpty) {
          // construct
          val accepted = accepting.head
          val lexeme = new ValueLexeme(rowsBegin, columnsBegin, buffer.toString)
          val construction = innerRecognizers.get(accepted).get
          tokenQueue += construction(lexeme)
          advancing = false
          readNext = false
        } else {
          val notBroken = innerRecognizers.keys.filter(_.phase == FSAPhase.Continuing).toList

          if (notBroken.isEmpty) {
            // reset
            buffer = new StringBuilder("")
            recognizers.keys.foreach(_.reset())
            innerRecognizers = collection.mutable.Map(recognizers.toSeq: _*)
            didNotAcceptLast = true
          } else {
            // continue
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

        if (addBreak && (tokenQueue.nonEmpty && tokenQueue.last != InsertedBreakStatement || tokenQueue.isEmpty))
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
  val tokenStream: Stream[Token] = Stream.continually(getNextToken)
}
