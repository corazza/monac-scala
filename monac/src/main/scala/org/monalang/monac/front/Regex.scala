package org.monalang.monac.front

import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer

abstract class Node()
case object Empty extends Node
case class Kleene(node: Node) extends Node
case class Union(left: Node, right: Node) extends Node
case class Cat(first: Node, second: Node) extends Node
case class Lit(lit: Char) extends Node
// recognizes all characters
case object Character extends Node
// recognizes all digits
case object Digit extends Node
// recognizes special characters
case object Special extends Node

class Regex(val first: Node) {
  override def toString = first toString
}

object Regex {
  def apply(regex: String): Regex = {
    val postfix = toPostfix(withConcat(regex))
    new Regex(makeTree(postfix))
  }

  /**
   * Parses a postfix regular expression and creates a syntax tree representation of it.
   */
  private def makeTree(regex: String): Node = {
    val operands = new Stack[Node]()

    for (i <- 0 to (regex.length() - 1)) {
      regex(i) match {
        case '*' => {
          val node = operands.pop()
          operands.push(Kleene(node))
        }
        case '|' => {
          val noder = operands.pop()
          val nodel = operands.pop()
          operands.push(Union(nodel, noder))
        }
        case '.' => {
          val noder = operands.pop()
          val nodel = operands.pop()
          operands.push(Cat(nodel, noder))
        }
        case 'C' => {
          operands.push(Character)
        }
        case 'D' => {
          operands.push(Digit)
        }
        case 'S' => {
          operands.push(Special)
        }
        case c: Char => {
          operands.push(Lit(c))
        }
      }
    }

    operands.pop()
  }

  private def toPostfix(regex: String): String = {
    val operators = new Stack[Char]()
    val result = new StringBuilder()

    def popOperator() {
      if (operators.size > 0 && operators.top != '(') {
        result += operators.pop()
      }
    }

    for (i <- 0 to (regex.length - 1)) {
      regex(i) match {
        case '*' => {
          result += '*'
        }
        case '.' => {
          popOperator()
          operators.push('.')
        }
        case '|' => {
          popOperator()
          operators.push('|')
        }
        case '(' => {
          operators.push('(')
        }
        case ')' => {
          var c = operators.pop()
          while (c != '(') {
            result += c
            c = operators.pop()
          }
        }
        case c: Char => {
          result += c
        }
      }
    }

    operators foreach { c => result += c }

    result.toString
  }

  private def withConcat(regex: String): String = {
    val result = new StringBuilder
    result += regex(0)

    def closer(c: Char) = !Set('(', '|').contains(c)
    def opener(c: Char) = !Set(')', '|', '*').contains(c)

    for (i <- 1 to (regex.length - 1)) {
      val last = regex(i - 1)
      val current = regex(i)
      if (closer(last) && opener(current)) {
        result += '.'
      }
      result += current
    }

    result.toString
  }
}
