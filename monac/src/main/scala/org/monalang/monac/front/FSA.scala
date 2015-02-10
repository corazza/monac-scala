package org.monalang.monac.front

/**
 * A finite state automaton constructed from a transition diagram
 */
class FSA (transitions : TransitionDiagram, startingState : Int /*accepting state info (set later?)*/) {
  var currentState : Int = startingState
  
  def advance(c : Char) {
    // TODO implement transition
    currentState = 0
  }
  
  def accepting() = false
}

object FSA {
  def apply(regex : String) = fromExpression(regex)
  
  def fromExpression(regex : String) : FSA = {
    // TODO implement accepting states after invalid characters
    new FSA(new TransitionDiagram(), 0)
  }
}