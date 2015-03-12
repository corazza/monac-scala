package org.monalang.monac.front

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Queue
// TODO convert to TransitionDiagramEditor (-> TransitionDiagramEditor)

/**
 * Algorithms for converting regular expressions to transition diagrams.
 */
class TransitionDiagramEditor(initialStates: Int) extends Diagram {
  val matrix = ArrayBuffer[ArrayBuffer[Char]]()

  // initialize the transition diagram (empty at the beginning)
  for (i <- 0 to (initialStates - 1)) {
    val row: ArrayBuffer[Char] = ArrayBuffer()
    for (j <- 0 to (initialStates - 1))
      row += TransitionDiagramEditor.NoTransition
    matrix += row
  }

  def getTransitionDiagram: TransitionDiagram = new TransitionDiagram(matrix)

  /**
   * Creates a new state not connected to any other.
   *
   * @return index of state
   */
  private def addState(): Int = {
    matrix.foreach(_.append(TransitionDiagramEditor.NoTransition))
    val newrow = ArrayBuffer[Char]()
    for (i <- 0 to currentStates)
      newrow += TransitionDiagramEditor.NoTransition
    matrix += newrow
    currentStates - 1
  }

  /**
   * Create a transition rule `from --(over)--> to`
   */
  private def addTransition(from: Int, to: Int, over: Char) {
    matrix(from)(to) = over
  }

  private def removeTransition(from: Int, to: Int) {
    matrix(from)(to) = TransitionDiagramEditor.NoTransition
  }

  /**
   * Expands a state of the diagram with another diagram.
   *
   * A - state being expanded
   * B - this graph
   * C - graph being added
   */
  private def expandState(state: Int, td: TransitionDiagramEditor) {
    val myStates = currentStates
    for (i <- 0 to td.currentStates - 2) addState

    def myState(n: Int) = n + myStates - 1 // first is not added

    // save what A previously lead to
    val previousLeads = matrix(state) toArray

    // remove previous transitions from A
    matrix(state) = new ArrayBuffer[Char]()
    for (i <- 0 to currentStates - 1) {
      matrix(state) += TransitionDiagramEditor.NoTransition
    }

    // change A to be the beginning of C
    for (i <- 0 to td.currentStates - 1) {
      addTransition(state, myState(i), td.matrix(0)(i))
    }

    // connect remaining states in B according to C
    for (i <- 1 to (td.currentStates - 1)) {
      for (j <- 1 to (td.currentStates - 1)) {
        addTransition(myState(i), myState(j), td.matrix(i)(j))
      }
    }

    // set C's final state's leads to what A previously lead to
    for (i <- 0 to previousLeads.length - 1) {
      matrix(myState(td.endState))(i) = previousLeads(i)
    }
  }
}

object TransitionDiagramEditor {
  val NoTransition = 'n': Char
  val EtaTransition = 'e': Char
  val CharacterTransition = 'c': Char
  val DigitTransition = 'd': Char
  val SpecialTransition = 's': Char

  private def eval(node: Node): TransitionDiagramEditor = {
    node match {
      case Empty => {
        val result = new TransitionDiagramEditor(2)
        result.addTransition(0, 1, EtaTransition)
        result
      }

      case Lit(lit) => {
        val result = new TransitionDiagramEditor(2)
        result.addTransition(0, 1, lit)
        result
      }

      case Character => {
        val result = new TransitionDiagramEditor(2)
        result.addTransition(0, 1, CharacterTransition)
        result
      }

      case Digit => {
        val result = new TransitionDiagramEditor(2)
        result.addTransition(0, 1, DigitTransition)
        result
      }

      case Special => {
        val result = new TransitionDiagramEditor(2)
        result.addTransition(0, 1, SpecialTransition)
        result
      }

      case Union(left, right) => {
        val leftDiag = eval(left)
        val rightDiag = eval(right)
        // 0 is begin, 1 and 2 are left and right, 3 is end
        val result = new TransitionDiagramEditor(4)

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
        val result = new TransitionDiagramEditor(4)

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
        val result = new TransitionDiagramEditor(4)

        result.addTransition(0, 1, EtaTransition)
        result.addTransition(1, 2, EtaTransition)
        result.addTransition(2, 3, EtaTransition)
        result.addTransition(2, 1, EtaTransition)
        result.addTransition(0, 3, EtaTransition)

        println("kleene before expansion:")
        println(result)
        println("expanding 1 with:")
        println(diag)
        result.expandState(1, diag)

        result
      }
    }
  }

  def nfa(regex: Regex): TransitionDiagramEditor = eval(regex.first)

  // TODO implement
  def minimize(dfa: TransitionDiagramEditor) = dfa

  def nfaToDfa(nfa: TransitionDiagramEditor): TransitionDiagramEditor = {
    def getClosure(nfaState: Int, over: Char): Array[Int] = ???
    def getEtaClosure(nfaState: Int) = getClosure(nfaState, EtaTransition)
    def getTransitions(nfaStates: Array[Int]): Array[Char] = ???

    val result = new TransitionDiagramEditor(0)
    // dfa states to be processed
    val stateQueue = new Queue[Int]
    // nfa state -> {dfa states}
    val stateMap = Map[Int, Array[Int]]()

    // first state of dfa
    stateQueue.enqueue(0)
    // needs to be kickstarted with the eta-transition - it is all these states
    stateMap += 0 -> getEtaClosure(0)

    while (!stateQueue.isEmpty) {
      // dfa label
      val state = stateQueue.dequeue()
      // states in nfa for the current dfa label
      val etaClosure = stateMap(state)
      val transitions = getTransitions(etaClosure)
      // for each transition, get closure
      val closures = transitions map { c: Char =>
        // for each in etaClosure, check if has transition over c, if so, add
        nfa.states filter { state =>
          def checkTransition(i: Int, to: Int): Boolean = {
            if (nfa.fromState(etaClosure(i), c).get == to) true
            else if (i + 1 == etaClosure.size) false
            else checkTransition(i + 1, state)
          }
          checkTransition(0, state)
        }
      }

      // (dfa states) -> nfa state
      var closureMap: Map[Array[Int], Int] = null

      def computeClosureMap() {
        closureMap = stateMap map (_.swap)
      }

      computeClosureMap()

      // for each closure, create transition in result, for new closures,
      // create a new state in result
      for ((closure, i) <- (closures zipWithIndex)) {
        if (!closureMap.contains(closure)) {
          val newState = result.addState()
          stateMap += newState -> closure
          computeClosureMap()
        }
        result.addTransition(state, closureMap(closure), transitions(i))
      }
    }

    minimize(result)
  }
}
