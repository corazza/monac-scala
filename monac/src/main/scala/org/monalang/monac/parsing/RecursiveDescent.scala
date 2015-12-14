package org.monalang.monac.parsing

import org.monalang.monac.lexing._
import org.monalang.monac.symbol.SymbolTable
import org.monalang.monac.iface._

import scala.collection.mutable.ArrayBuffer
import scala.reflect._

/**
  * Engine for parsing grammars recursively.
  */
class RecursiveDescent(grammar: Grammar, tokens: Stream[Token]) {
  // state which remembers the next token to be read
  var i = 0

  def parse(start: NonTerminal, parentScope: SymbolTable): ASTNode = {
    val (production, fragment) = grammar.ntmp(start).head

    val scope = new SymbolTable(Some(parentScope))
    val elements = new ArrayBuffer[ASTNode]()

    for(e <- production) e match {
      case EtaProduction  => EmptyNode()
      case e: NonTerminal => elements += parse(e, scope)
      case Terminal(tag)  => matchToken(tag)
    }

    fragment(new Context(parentScope, elements.toList))
  }

  def matchToken[A <: Token](tokenTag: ClassTag[A]): ASTNode = {
    val read = tokens(i)
    i += 1

    if (tokenTag.runtimeClass == read.getClass) {
      read match {
        case NumLiteral(lexeme) => NumNode(lexeme)
        case CharLiteral(lexeme) => CharNode(lexeme)
        case StringLiteral(lexeme) => StringNode(lexeme)
        case LowerId(lexeme) => LowerIdNode(lexeme)
        case UpperId(lexeme) => UpperIdNode(lexeme)
        case _: Keyword => EmptyNode()
      }
    } else {
      val line = if (read.isInstanceOf[ValueToken]) read.asInstanceOf[ValueToken].lexeme.row
                 else read.asInstanceOf[SyntacticToken].lexeme.row

      Message.error("Line " + line + ", syntax error")
      throw new Exception()
    }
  }
}