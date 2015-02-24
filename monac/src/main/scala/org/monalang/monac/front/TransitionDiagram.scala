package org.monalang.monac.front

import scala.collection.mutable.ArrayBuffer;

/**
 * Information about a finite state automaton transition functions.
 *
 * 0 represents no transition, 1 eta-transition
 */
class TransitionDiagram(var nstates: Int) {
  /**
   * Transition graph representing the state machine.
   */
  val matrix: ArrayBuffer[ArrayBuffer[Char]] = ArrayBuffer()

  // initialize the transition diagram (empty at the beginning
  for (i <- 0 to (nstates - 1)) {
    val row: ArrayBuffer[Char] = ArrayBuffer()
    for (j <- 0 to (nstates - 1))
      row += 0
    matrix += row
  }

  /**
   * Creates a new state not connected to any other.
   *
   * @return state
   */
  def addState(): Int = {
    nstates += 1
    val newrow = ArrayBuffer[Char]()
    for (i <- 0 to (nstates - 1)) {
      newrow += 0
    }
    matrix.foreach(_.append(0))
    nstates - 1
  }

  /**
   * Create a transition rule `from --(over)--> to`
   */
  def addTransition(from: Int, to: Int, over: Char) {
    matrix(from)(to) = over
  }

  def removeTransition(from: Int, to: Int) {
    matrix(from)(to) = 0
  }

  /**
   * Returns the result of a transition from `state` over `transition`.
   *
   * @return Some(state : Int) if the transition exists
   * @return None if the transition doesn't exist.
   */
  def fromState(state: Int, transition: Char): Option[Int] = {
    for (i <- 0 to (nstates - 1)) {
      if (matrix(state)(i) == transition) Some(i)
    }

    None
  }

  /**
   * The number of nodes `state` leads to.
   *
   * @return 0 if `state` is accepting
   */
  def outNum(state: Int) = {
    matrix(state).toArray.sum
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

    r.toString
  }
}

object TransitionDiagram {
  def nfa(regex: Regex): TransitionDiagram = {
    ???
    // HERE
  }

  def nfaToDfa(nfa: TransitionDiagram): TransitionDiagram = {
    ???
  }
}