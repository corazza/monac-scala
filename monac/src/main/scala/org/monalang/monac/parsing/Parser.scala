package org.monalang.monac.parsing

import org.monalang.monac.lexing._
import org.monalang.monac.symbol.Scope

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Try

case class Parser(lexer: Lexer) {
  val logParsing = false

  val tokens = lexer.tokenStream

  // state which remembers the next token to be read
  var i = 0

  def parse(start: NonTerminal): ASTNode = {
    val nextToken = tokens(i)

    val terminal = if (nextToken != EndOfSource) Terminal(ClassTag(nextToken.getClass)) else End

    if (logParsing) println("parsing: " + start)

    val ((_, production), fragment) = Try(MonaGrammar.rules(MonaGrammar.parseTable(start)(terminal))).toOption match {
      case Some(a) => a
      case None => {
        println(start)
        println(nextToken)
        throw new Exception("Syntax error (production)")
      }
    }

    val elements = new ArrayBuffer[ASTNode]()

    for(e <- production) e match {
      case Eta => EmptyNode()
      case e: NonTerminal => elements += parse(e)
      case Terminal(tag)  => elements += matchToken(tag)
    }

    fragment(Context(elements.toList))
  }

  def matchToken[A <: Token](tokenTag: ClassTag[A]): ASTNode = {
    val read = tokens(i)
    i += 1

    if (logParsing) println("matching: " + read)

    if (tokenTag.runtimeClass == read.getClass) {
      read match {
        case NumLiteral(lexeme) => Num(lexeme)
        case CharLiteral(lexeme) => Char(lexeme)
        case StringLiteral(lexeme) => StringNode(lexeme)
        case LowerIdToken(lexeme) => LowerId(lexeme)
        case UpperIdToken(lexeme) => UpperId(lexeme)
        case OperatorId(lexeme) => Operator(lexeme)
        case _ => EmptyNode()
      }
    } else {
      throw new Exception("Syntax error (match)")
    }
  }

  val parsed = parse(StartNT)
}