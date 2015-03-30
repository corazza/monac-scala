package org.monalang.monac.front

import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer

abstract class Node
case object Empty extends Node
case class Kleene(node: Node) extends Node
case class Union(left: Node, right: Node) extends Node
case class Cat(first: Node, second: Node) extends Node
case class Lit(lit: Char) extends Node
// recognizes all letters
case object Letter extends Node
// recognizes all digits
case object Digit extends Node
// recognizes special characters
case object Special extends Node
// recognizes any character
case object Whichever
// period
case object Period extends Node

class Regex(val first: Node) {
  override def toString = first toString
}

object Regex {
  def apply(regex: String): Regex = {
    val explicitConcat = withConcat(regex)
    val postfix = toPostfix(explicitConcat)
    new Regex(makeTree(postfix))
  }

  /**
   * Parses a postfix regular expression and creates a syntax tree representation of it.
   *
   * @param regex regular expression string in postfix form with excplicit concat operators
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
        case 'L' => operands.push(Letter)
        case 'D' => operands.push(Digit)
        case 'S' => operands.push(Special)
        case 'A' => operands.push(Whichever)
        case 'P' => operands.push(Period)
        case c: Char => operands.push(Lit(c))
      }
    }

    operands.pop()
  }

  private def toPostfix(regex: String): String = {
    val operators = new Stack[Char]()
    val result = new StringBuilder()

    def newOperator(c: Char) {
      if (operators.size == 0 || operators.top == '(') {
        operators.push(c)
      } else if (c == '.' && operators.top == '|') {
        operators.push(c)
      } else {
        result += operators.pop()
        newOperator(c)
      }
    }

    for (i <- 0 to (regex.length - 1)) {
      regex(i) match {
        case '*' => result += '*'
        case '.' => newOperator('.')
        case '|' => newOperator('|')
        case '(' => operators.push('(')
        case ')' => {
          var c = operators.pop()
          while (c != '(') {
            result += c
            c = operators.pop()
          }
        }
        case c: Char => result += c
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
