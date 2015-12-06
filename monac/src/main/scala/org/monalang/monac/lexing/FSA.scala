package org.monalang.monac.lexing

import FSAPhase.Accepting
import FSAPhase.Broken
import FSAPhase.Continuing
import org.monalang.monac.common.util.Reader
import scala.util.Try
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set

object FSAPhase extends Enumeration {
  type FSAState = Value
  val Continuing, Accepting, Broken = Value
}
/**
 * A finite state automaton constructed from a transition diagram
 */
class FSA(val transitions: TransitionDiagram, val startingState: Int, val fromExpression: String) {
  import FSAPhase._

  var phase = Continuing
  var currentState: Int = startingState

  def atFinal = transitions.finalStates.contains(currentState)

  def advance(c: Char) = transitions.fromState(currentState, c) match {
    case Some(state) => currentState = state
    case None => if (atFinal) phase = Accepting else phase = Broken
  }

  def reset() = {
    currentState = startingState
    phase = Continuing
  }

  override def toString = {
    val r = new StringBuffer("")

    r.append(transitions.toString)
    r.append(", current state: " + currentState + "\n")

    r.toString

    fromExpression
  }
}

object FSA {
  var expressionMap = Map[String, TransitionDiagram]()

  val inputStream = getClass().getResourceAsStream("/expressions")
  var reading = true

  while (reading) {
    def getDimensions(): Option[Int] =
      Try(Reader.readUntil(inputStream, ' ').toInt).toOption
    def getExpression(): String = Reader.readUntil(inputStream, '\n')

    def getMatrix(dimensions: Int): ArrayBuffer[ArrayBuffer[Char]] = {
      val matrix = new ArrayBuffer[ArrayBuffer[Char]]()
      for (i <- 0 until dimensions) {
        val row = new ArrayBuffer[Char]
        for (j <- 0 until dimensions) {
          row += inputStream.read().asInstanceOf[Char]
        }
        inputStream.read().asInstanceOf[Char]
        matrix += row
      }
      matrix
    }

    def getFinalStates(): Set[Int] = {
      val statesString = Reader.readUntil(inputStream, '\n')
      val statesList = statesString.split(' ').toList.map(_.toInt)
      Set() ++ statesList.toSet // because toSet returns immutable
    }

    val dimensions = getDimensions().getOrElse(-1)

    if (dimensions != -1) {
      val expression = getExpression()
      val matrix = getMatrix(dimensions)
      val finalStates = getFinalStates
      expressionMap += expression -> TransitionDiagram(matrix, finalStates)
    } else {
      reading = false
    }
  }

  def apply(expressiona: String): FSA = {
    val expression = expressiona.filter(_ != ' ')
    expressionMap.get(expression) match {
      case Some(td) => new FSA(td, 0, expression)
      case None => new FSA(TransitionDiagramEditor.fromRegex(expression), 0, expression)
    }
  }
}
