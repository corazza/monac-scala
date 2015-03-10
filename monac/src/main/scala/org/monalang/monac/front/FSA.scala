package org.monalang.monac.front

/**
 * A finite state automaton constructed from a transition diagram
 */
class FSA(transitions: TransitionDiagram, startingState: Int) {
  var currentState: Int = startingState

  /**
   * Advances the automaton by one character.
   *
   * In case no given transition exists for current state and character combination,
   * the state is left unmodified (no error is thrown).
   */
  def advance(c: Char) {
    transitions.fromState(currentState, c) match {
      case Some(state) => currentState = state
      case None => Unit
    }
  }

  /**
   * Returns true if the current state is accepting.
   *
   * An accepting state is the one without any outward transitions.
   */
  def accepting() = transitions.outNum(currentState) == 0
  
  override def toString = {
    val r = new StringBuffer("")
    
    r.append(transitions.toString)
    r.append("Current state: " + currentState.toChar + "\n")
    
    r.toString
  }
}

object FSA {
  def apply(dfa: TransitionDiagram) = new FSA(dfa, 0)
    
  def apply(expression: String): FSA = {
    val regex = Regex(expression)
    val nfa = TransitionDiagram.nfa(regex)
    val dfa = TransitionDiagram.nfaToDfa(nfa)
    new FSA(dfa, 0)
  }
}