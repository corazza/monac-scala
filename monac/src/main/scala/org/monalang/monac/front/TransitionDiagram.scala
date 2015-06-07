package org.monalang.monac.front

import org.monalang.monac.common.util.CharUtil

import scala.collection.mutable.{ArrayBuffer, Set}

/**
 * Common diagram functionality
 */
class TransitionDiagram(val matrix: ArrayBuffer[ArrayBuffer[Char]], val finalStates: Set[Int]) {
  def beginState = 0

  def endState: Int = {
    def isEndState(state: Int): Boolean = {
      val eq = matrix(state).head
      for (i <- 0 to currentStates - 1) if (matrix(state)(i) != eq) return false
      return true
    }
    for (i <- 0 until currentStates) if (isEndState(i)) return i
    currentStates - 1
  }

  def currentStates = matrix.length

  /**
   * Not final, just states which have no leads.
   */
  def endStates: List[Int] = {
    states filter { state =>
      matrix(state).foldLeft(true) { (acc, transition) =>
        acc && transition == matrix(state).head && transition == TransitionDiagram.NoTransition
      }
    }
  }

  def states = Range(0, currentStates).toList

  /**
   * The number of nodes `state` leads to.
   *
   * @return 0 if `state` is accepting
   */
  def outNum(state: Int) = {
    matrix(state).sum
  }

  /**
   * Returns the result of a transition from `state` over `transition`.
   *
   * @return Some(state : Int) if the transition exists
   * @return None if the transition doesn't exist.
   */
  def fromState(state: Int, transition: Char): Option[Int] = {
    // TODO test support for other languages and unicode symbols

    def matchesTransition(c: Char): Boolean =
      c == TransitionDiagram.AnyTransition && !Recognizer.without(Recognizer.whitespace, '\u0020').contains(transition) ||
        c == transition ||
        c == TransitionDiagram.LetterTransition && Recognizer.letter.contains(transition) ||
        c == TransitionDiagram.DigitTransition && Recognizer.digit.contains(transition) ||
        c == TransitionDiagram.IdTransition && Recognizer.id.contains(transition) ||
        c == TransitionDiagram.NonIdTransition && Recognizer.nonid.contains(transition) ||
        c == TransitionDiagram.UnicodeTransition && CharUtil.isUnicode(transition)

    val result = (matrix(state) zipWithIndex) filter (_ match {
      case (c, i) => matchesTransition(c)
    })

    if (result.isEmpty) None else Some(result.head._2)
  }

  /**
   * Creates a new state not connected to any other.
   *
   * @return index of state
   */
  def addState(): Int = {
    matrix.foreach(_.append(TransitionDiagram.NoTransition))
    val newrow = ArrayBuffer[Char]()
    for (i <- 0 to currentStates)
      newrow += TransitionDiagram.NoTransition
    matrix += newrow
    currentStates - 1
  }

  /**
   * Create a transition rule `from --(over)--> to`
   */
  def addTransition(from: Int, to: Int, over: Char) {
    matrix(from)(to) = over
  }

  def removeTransition(from: Int, to: Int) {
    matrix(from)(to) = TransitionDiagram.NoTransition
  }

  override def toString = {
    val r = new StringBuffer("")

    matrix.toList.foreach { row: ArrayBuffer[Char] =>
      row.toList.foreach { c: Char =>
        r.append(c match {
          case TransitionDiagram.NoTransition => "N"
          case TransitionDiagram.EtaTransition => "E"
          case TransitionDiagram.AnyTransition => "A"
          case TransitionDiagram.LetterTransition => "L"
          case TransitionDiagram.DigitTransition => "D"
          case TransitionDiagram.IdTransition => "I"
          case TransitionDiagram.NonIdTransition => "N"
          case TransitionDiagram.UnicodeTransition => "U"
          case c: Char => c
        })
        r.append(' ')
      }
      r.append('\n')
    }

    r.append("final states: " + finalStates.toList)

    r.toString
  }

  def toSaveString = {
    val r = new StringBuffer("")

    matrix.toList.foreach { row: ArrayBuffer[Char] =>
      row.toList.foreach { c: Char =>
        r.append(c)
      }
      r.append('\n')
    }

    finalStates.toList.foreach { state => r.append(state.toString + ' ') }

    r.toString
  }
}

object TransitionDiagram {
  // control
  val NoTransition = 0: Char
  val EtaTransition = 1: Char

  // classes
  val LetterTransition = 2: Char
  val DigitTransition = 3: Char
  val IdTransition = 4: Char
  val NonIdTransition = 5: Char
  val UnicodeTransition = 6: Char

  val AnyTransition = 7: Char // except newline

  /**
   * Create empty transition diagram with initialStates states
   */
  def apply(initialStates: Int) = {
    val matrix: ArrayBuffer[ArrayBuffer[Char]] = new ArrayBuffer[ArrayBuffer[Char]]()
    // initialize the transition diagram (empty at the beginning)
    for (i <- 0 until initialStates) {
      val row: ArrayBuffer[Char] = ArrayBuffer()
      for (j <- 0 until initialStates)
        row += TransitionDiagram.NoTransition
      matrix += row
    }

    // set later
    val finalStates = Set[Int]()

    new TransitionDiagram(matrix, finalStates)
  }

  /**
   * Create transition diagram from existing matrix and final states set
   */
  def apply(matrix: ArrayBuffer[ArrayBuffer[Char]], finalStates: Set[Int]) =
    new TransitionDiagram(matrix, finalStates)
}

























