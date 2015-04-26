package org.monalang.monac.front

import scala.Range
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set
import org.monalang.monac.common.util.CharUtil

/**
 * Common diagram functionality
 */
class TransitionDiagram(val matrix: ArrayBuffer[ArrayBuffer[Char]], val finalStates: Set[Int]) {
  def currentStates = matrix.length
  def beginState = 0
  def endState: Int = {
    def isEndState(state: Int): Boolean = {
      val eq = matrix(state)(0)
      for (i <- 0 to currentStates - 1) if (matrix(state)(i) != eq) (return false)
      return true
    }
    for (i <- 0 until currentStates) if (isEndState(i)) (return i)
    return currentStates - 1
  }

  def states = Range(0, currentStates) toList

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

    //    println("state: " + state + ", over: " + transition)
    def matchesTransition(c: Char): Boolean =
      c == TransitionDiagram.AnyTransition && transition != '\n' ||
        c == transition ||
        c == TransitionDiagram.LetterTransition && transition.isLetter ||
        c == TransitionDiagram.DigitTransition && transition.isDigit ||
        c == TransitionDiagram.SpecialTransition && CharUtil.isSpecial(transition)

    val result = (matrix(state) zipWithIndex) filter (_ match {
      case (c, i) => matchesTransition(c)
    })

    if (result.length == 0) None else Some(result.head._2)
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

    (matrix toList) foreach { row: ArrayBuffer[Char] =>
      (row toList) foreach { c: Char =>
        r.append(c)
        r.append(' ')
      }
      r.append('\n')
    }

    r.append("final states: " + finalStates.toList)

    r.toString
  }

  def toSaveString = {
    val r = new StringBuffer("")

    (matrix toList) foreach { row: ArrayBuffer[Char] =>
      (row toList) foreach { c: Char =>
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
  val SpecialTransition = 4: Char

  val AnyTransition = 5: Char // except newline

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

























