package org.monalang.monac.parsing

import org.monalang.monac.lexing._
import org.monalang.monac.symbol.SymbolTable

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Try

// HERE test parsing of simplified grammar

// create a global file AST, fragments just return ASTs
// create basic symbol table system which accounts for modules
// expand grammar

case class Parser(lexer: Lexer) {
  val tokens = lexer.tokenStream

  val sourceScope = new SymbolTable(None)

  // state which remembers the next token to be read
  var i = 0

  def parse(start: NonTerminal, parentScope: SymbolTable): ASTNode = {
    println("parsing " + start)

    val nextToken = tokens(i)

    val terminal = if (nextToken != EndOfSource) Terminal(ClassTag(nextToken.getClass)) else End

    val ((_, production), fragment) = Try(MonaGrammar.rules(MonaGrammar.parseTable(start)(terminal))).toOption match {
      case Some(a) => a
      case None => throw new Exception("Syntax error (production)")
    }

    val scope = new SymbolTable(Some(parentScope))
    val elements = new ArrayBuffer[ASTNode]()

    for(e <- production) e match {
      case Eta => EmptyNode()
      case e: NonTerminal => elements += parse(e, scope)
      case Terminal(tag)  => elements += matchToken(tag)
    }

    fragment(Context(parentScope, elements.toList))
  }

  def matchToken[A <: Token](tokenTag: ClassTag[A]): ASTNode = {
    val read = tokens(i)
    println("matching: " + read + " with " + tokenTag)
    i += 1

    if (tokenTag.runtimeClass == read.getClass) {
      read match {
        case NumLiteral(lexeme) => Num(lexeme)
        case CharLiteral(lexeme) => Char(lexeme)
        case StringLiteral(lexeme) => StringNode(lexeme)
        case LowerId(lexeme) => LowerIdNode(lexeme)
        case UpperId(lexeme) => UpperIdNode(lexeme)
        case OperatorId(lexeme) => OperatorNode(lexeme)
        case _ => EmptyNode()
      }
    } else {
      throw new Exception("Syntax error (match)")
    }
  }

  parse(Start, sourceScope)
}