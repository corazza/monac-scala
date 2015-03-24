package org.monalang.monac.front

/**
 * A finite state automaton constructed from a transition diagram
 */
class FSA(val transitions: TransitionDiagram, val startingState: Int) {
  var currentState: Int = startingState
  var accepting = false

  def atFinal = transitions.finalStates.contains(currentState)

  /**
   * Advances the automaton by one character.
   *
   * In case no given transition exists for current state and character combination,
   * the state is left unmodified (no error is thrown).
   */
  def advance(c: Char) = transitions.fromState(currentState, c) match {
    case Some(state) => currentState = state
    case None => {
      accepting = atFinal
    }
  }

  def reset() = {
    currentState = startingState
    accepting = false
  }

  override def toString = {
    val r = new StringBuffer("")

    r.append(transitions.toString)
    r.append("current state: " + currentState.toChar + "\n")

    r.toString
  }
}

object FSA {
  def apply(dfa: TransitionDiagram) = new FSA(dfa, 0)

  // TODO (later) read from file
  def apply(expression: String): FSA = {
    val regex = Regex(expression)
    val nfa = TransitionDiagramEditor.nfa(regex)
    val dfa = TransitionDiagramEditor.nfaToDfa(nfa)
    new FSA(dfa, 0)
  }
}
