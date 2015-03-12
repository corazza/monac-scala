package org.monalang.monac.front

import scala.collection.mutable.ArrayBuffer
import java.io.File

/**
 * Information about a finite state automaton transition functions.
 *
 * state 0 is the initial state
 */
class TransitionDiagram(protected val matrix: ArrayBuffer[ArrayBuffer[Char]]) extends Diagram
