package org.monalang.monac.front

import scala.collection.mutable.ArrayBuffer;

/**
 * Information about a finite state automaton transition functions.
 *
 * character 0 represents no transition, 1 eta-transition
 * 
 * state 0 is the initial state, state n is the final state
 */
class TransitionDiagram(var nstates: Int) {
  val NoTransition = 0
  val EtaTransition = 1
  val CharacterTransition = 2
  val DigitTransition = 3
  val SpecialTransition = 4
    
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
  
  def beginState = 0
  def endState = nstates-1
  
  /**
   * Expands a state of the diagram with another diagram.
   * 
   * A - state being expanded
   * B - this graph
   * C - graph being added
   */
   // TODO test this
  def expandState(state: Int, td: TransitionDiagram) {
    val mystates = nstates
    for (i <- 0 to (td.nstates-2)) addState
      
    // save what A previously lead to
    val previousLeads = matrix(state).copy    
    
    // change A to be the beginning of C
    for (i <- 0 to (td.nstates-1)) {
      matrix(state)(i) = td.matrix(0)(i)+mystates
    }
    
    // connect remaining states in B according to C
    for (i <- 1 to (td.nstates-2)) {
      for (j <- 1 to (td.nstates-2)) {
        addTransition(i+mystates, j+mystates, td.matrix()())
      }
    }
    
    // set C's final state's leads to what A previously lead to
    for (i <- 0 to mystates-1) {
      matrix(td.endState+mystates)(i) = previousLeads(i)
    }
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
  private def eval(node: Node): TransitionDiagram = {
    node match {
      case Empty => {
          val result = new TransitionDiagram(2)
          result.addTransition(0, 1, EtaTransition)
          result
      }
        
      case Lit(lit) => {
          val result = new TransitionDiagram(2)
          result.addTransition(0, 1, lit)
          result
      }
      
      case Character => {
          val result = new TransitionDiagram(2)
          result.addTransition(0, 1, CharacterTransition)
          result
      }

      case Digit => {
          val result = new TransitionDiagram(2)
          result.addTransition(0, 1, DigitTransition)
          result
      }
      
      case Special => {
          val result = new TransitionDiagram(2)
          result.addTransition(0, 1, SpecialTransition)
          result
      }

      case Union(left, right) => {
        val leftDiag = eval(left)
        val rightDiag = eval(right)
        // 0 is begin, 1 and 2 are left and right, 3 is end
        val result = new TransitionDiagram(4)
        
        result.addTransition(0, 1, EtaTransition)
        result.addTransition(0, 2, EtaTransition)
        result.addTransition(1, 3, EtaTransition)
        result.addTransition(2, 3, EtaTransition)

        result.expandState(1, leftDiag)
        result.expandState(2, rightDiag)        
        
        result
      }
      
      case Cat(first, second) => {
        val firstDiag = eval(first)
        val secondDiag = eval(second)
        // 0 is begin, 1 and 2 are left and right, 3 is end
        val result = new TransitionDiagram(4)

        result.addTransition(0, 1, EtaTransition)
        result.addTransition(1, 2, EtaTransition)
        result.addTransition(2, 3, EtaTransition)

        result.expandState(1, firstDiag)
        result.expandState(2, secondDiag)        

        result
      }
      
      case Kleene(node) => {
        val diag = eval(node)
        // 0 is begin, 1 is node beginning, 2 is node end, 3 is graph end
        val result = new TransitionDiagram(4)
        
        result.addTransition(0, 1, EtaTransition)
        result.addTransition(1, 2, EtaTransition)
        result.addTransition(2, 3, EtaTransition)
        result.addTransition(2, 1, EtaTransition)
        result.addTransition(0, 3, EtaTransition)

        result.expandState(1, diag)
      }
    }
  }
  
  def nfa(regex: Regex): TransitionDiagram = eval(regex.first)
  
  def nfaToDfa(nfa: TransitionDiagram): TransitionDiagram = {
    ???
  }
}
