package org.monalang.monac.front

import scala.collection.mutable.ArrayBuffer;

/**
 * Information about a finite state automaton transition functions
 */
class TransitionDiagram {
  /**
   * Transition graph representing the state machine.
   */
  val transitions : ArrayBuffer[ArrayBuffer[Char]] = new ArrayBuffer()
}
