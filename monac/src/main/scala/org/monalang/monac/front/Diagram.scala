package org.monalang.monac.front

import scala.collection.mutable.ArrayBuffer

/**
 * Common diagram functionality
 */
// TODO (later) rename to TD, remove hierachy
abstract class Diagram {
  protected val matrix: ArrayBuffer[ArrayBuffer[Char]]

  def currentStates = matrix.length
  def beginState = 0
  def endState: Int = {
    def isEndState(state: Int): Boolean = {
      val eq = matrix(state)(0)
      for (i <- 0 to currentStates - 1) if (matrix(state)(i) != eq) (return false)
      return true
    }
    for (i <- 0 to currentStates - 1) if (isEndState(i)) (return i)
    return currentStates - 1
  }
  def states = Range(0, currentStates) toArray

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
    val result = (matrix(state) zipWithIndex) filter (_ match {
      case (c, i) => c == transition
    })
    if (result.length == 0) None else Some(result.head._2)
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