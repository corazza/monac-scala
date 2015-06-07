package org.monalang.monac.front

import scala.collection.mutable.Stack

abstract class Node
case class Kleene(node: Node) extends Node
case class Union(left: Node, right: Node) extends Node
case class Cat(first: Node, second: Node) extends Node
case class Lit(lit: Char) extends Node
case class Not(c: Char) extends Node

// classes (nonenumerated)
case object Letter extends Node
case object Digit extends Node

case object Id extends Node

case object NonId extends Node
case object Unicode extends Node

// specific characters
case object Newline extends Node
case object Period extends Node
case object Opening extends Node
case object Closing extends Node
case object Vertical extends Node
case object Star extends Node
case object Space extends Node

case object Tab extends Node

case object Whichever extends Node

class Regex(val first: Node) {
  override def toString = first.toString
}

/**
 * classes (encompass all characters, no crossover):
 * W - whitespace
 * L - letter
 * D - digits
 * I - id (special ASCII, operator)
 * N - nonid
 * U - unicode (rest)
 *
 * specific characters:
 * P - period
 * E - newline
 * O - open parens
 * C - closing parens
 * V - vertical line
 * K - asterisk
 * S - space
 * T - tabulator
 *
 * A - any character except newline or tab or CR
 */
object Regex {
  // can't mix classes with characters that belong to them -- FIX: enumerate and expand classes
  def apply(regex: String): Regex = {
    val explicitConcat = withConcat(regex)
    val postfix = toPostfix(explicitConcat)
    val firstNode = makeTree(postfix)
    new Regex(firstNode)
  }

  /**
   * Parses a postfix regular expression and creates a syntax tree representation of it.
   *
   * @param regex regular expression string in postfix form with excplicit concat operators
   */
  private def makeTree(regex: String): Node = {
    val operands = new Stack[Node]()

    for (i <- 0 until regex.length) {
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
        case 'I' => operands.push(Id)
        case 'N' => operands.push(NonId)
        case 'U' => operands.push(Unicode)
        case 'A' => operands.push(Whichever)
        case 'O' => operands.push(Opening)
        case 'C' => operands.push(Closing)
        case 'V' => operands.push(Vertical)
        case 'K' => operands.push(Star)
        case 'S' => operands.push(Space)
        case 'T' => operands.push(Tab)
        case 'E' => operands.push(Newline)
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
      if (operators.isEmpty || operators.top == '(') {
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
    def opener(c: Char) = !Set(')', '|', '*', 'N').contains(c)

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
