package org.monalang.monac.front

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Queue

/**
 * Algorithms for converting regular expressions to transition diagrams.
 */
object TransitionDiagramEditor {
  /**
   * Expands a state of the diagram with another diagram.
   *
   * A - state being expanded
   * B - this graph
   * C - graph being added
   */
  private def expandState(on: TransitionDiagram, state: Int, out: TransitionDiagram) {
    val myStates = on.currentStates
    for (i <- 0 to out.currentStates - 2) on.addState()

    def myState(n: Int) = n + myStates - 1 // first is not added

    // save what A previously lead to
    val previousLeads = on.matrix(state) toArray

    // remove previous transitions from A
    on.matrix(state) = new ArrayBuffer[Char]()
    for (i <- 0 until on.currentStates) {
      on.matrix(state) += TransitionDiagram.NoTransition
    }

    // change A to be the beginning of C
    for (i <- 0 until out.currentStates) {
      on.addTransition(state, myState(i), out.matrix(0)(i))
    }

    // connect remaining states in B according to C
    for (i <- 1 until out.currentStates) {
      for (j <- 1 until out.currentStates) {
        on.addTransition(myState(i), myState(j), out.matrix(i)(j))
      }
    }

    // set C's final state's leads to what A previously lead to
    for (i <- 0 until previousLeads.length) {
      on.matrix(myState(out.endState))(i) = previousLeads(i)
    }
  }

  private def eval(node: Node): TransitionDiagram = {
    node match {
      case Lit(lit) => {
        val result = TransitionDiagram(2)
        result.addTransition(0, 1, lit)
        result
      }

      case Letter => {
        val result = TransitionDiagram(2)
        result.addTransition(0, 1, TransitionDiagram.LetterTransition)
        result
      }

      case Digit => {
        val result = TransitionDiagram(2)
        result.addTransition(0, 1, TransitionDiagram.DigitTransition)
        result
      }

      case Special => {
        val result = TransitionDiagram(2)
        result.addTransition(0, 1, TransitionDiagram.SpecialTransition)
        result
      }

      case Whichever => {
        val result = TransitionDiagram(2)
        result.addTransition(0, 1, TransitionDiagram.AnyTransition)
        result
      }

      case Newline => {
        val result = TransitionDiagram(2)
        result.addTransition(0, 1, '\n')
        result
      }

      case Period => {
        val result = TransitionDiagram(2)
        result.addTransition(0, 1, '.')
        result
      }

      case Opening => {
        val result = TransitionDiagram(2)
        result.addTransition(0, 1, '(')
        result
      }

      case Closing => {
        val result = TransitionDiagram(2)
        result.addTransition(0, 1, ')')
        result
      }

      case Vertical => {
        val result = TransitionDiagram(2)
        result.addTransition(0, 1, '|')
        result
      }

      case Star => {
        val result = TransitionDiagram(2)
        result.addTransition(0, 1, '*')
        result
      }

      case Union(left, right) => {
        val leftDiag = eval(left)
        val rightDiag = eval(right)
        // 0 is begin, 1 and 2 are left and right, 3 is end
        val result = TransitionDiagram(4)

        result.addTransition(0, 1, TransitionDiagram.EtaTransition)
        result.addTransition(0, 2, TransitionDiagram.EtaTransition)
        result.addTransition(1, 3, TransitionDiagram.EtaTransition)
        result.addTransition(2, 3, TransitionDiagram.EtaTransition)

        expandState(result, 1, leftDiag)
        expandState(result, 2, rightDiag)

        result
      }

      case Cat(first, second) => {
        val firstDiag = eval(first)
        val secondDiag = eval(second)
        // 0 is begin, 1 and 2 are left and right, 3 is end
        val result = TransitionDiagram(4)

        result.addTransition(0, 1, TransitionDiagram.EtaTransition)
        result.addTransition(1, 2, TransitionDiagram.EtaTransition)
        result.addTransition(2, 3, TransitionDiagram.EtaTransition)

        expandState(result, 1, firstDiag)
        expandState(result, 2, secondDiag)

        result
      }

      case Kleene(node) => {
        val diag = eval(node)
        // 0 is begin, 1 is node beginning, 2 is node end, 3 is graph end
        val result = TransitionDiagram(4)

        result.addTransition(0, 1, TransitionDiagram.EtaTransition)
        result.addTransition(1, 2, TransitionDiagram.EtaTransition)
        result.addTransition(2, 3, TransitionDiagram.EtaTransition)
        result.addTransition(2, 1, TransitionDiagram.EtaTransition)
        result.addTransition(0, 3, TransitionDiagram.EtaTransition)

        expandState(result, 1, diag)

        result
      }
    }
  }

  def nfa(regex: Regex): TransitionDiagram = eval(regex.first)

  // TODO implement
  def minimize(dfa: TransitionDiagram) = dfa

  def nfaToDfa(nfa: TransitionDiagram): TransitionDiagram = {
    def getClosure(nfaState: Int, over: Char): List[Int] = nfa.states filter { state =>
      if (nfa.matrix(nfaState)(state) == over) true
      else false
    }

    def getEtaClosureInner(acc: List[Int], nfaState: Int): List[Int] = {
      val first = getClosure(nfaState, TransitionDiagram.EtaTransition)
      val stateIncluded = first :+ nfaState
      if (first.length != 0) {
        val others = first map { state =>
          if (acc.exists(_ == state)) List[Int]()
          else getEtaClosureInner(acc ++ stateIncluded, state)
        }

        (stateIncluded ++ (others flatten)) distinct
      } else stateIncluded
    }

    def getEtaClosure(nfaState: Int): List[Int] = getEtaClosureInner(List(), nfaState)
    def getTransitions(nfaStates: List[Int]): List[Char] =
      (nfaStates map (state => nfa.matrix(state) filter { t =>
        (t != TransitionDiagram.NoTransition) && (t != TransitionDiagram.EtaTransition)
      }) flatten) distinct

    val result = TransitionDiagram(1)

    // dfa states to be processed
    val stateQueue = new Queue[Int]

    // dfa state -> {nfa states}
    val stateMap = Map[Int, List[Int]]()

    // first state of dfa
    stateQueue.enqueue(0)
    // needs to be kickstarted with the eta-transition - it is all these states
    stateMap += 0 -> getEtaClosure(0)

    // process DFA states
    while (!stateQueue.isEmpty) {

      // dfa label
      val state = stateQueue.dequeue()

      // states in nfa for the current dfa label
      val etaClosure = stateMap(state)

      // all transitions that lead from states in etaClosure
      val transitions = getTransitions(etaClosure)

      // for each transition, get closure
      val closures = transitions map { transition: Char =>
        ((for (inEta <- etaClosure; state <- nfa.states) yield {
          if (nfa.matrix(inEta)(state) == transition) Some(getEtaClosure(state))
          else None
        }) flatten) flatten
      }

      // (dfa states) -> nfa state
      var closureMap: Map[List[Int], Int] = null

      def computeClosureMap() {
        closureMap = stateMap map (_.swap)
      }
      computeClosureMap()

      // for each closure, create transition in result, for new closures,
      // create a new state in result
      for ((closure, transition) <- (closures zip transitions)) {
        if (!closureMap.contains(closure)) {
          val newState = result.addState()
          stateMap += newState -> closure
          computeClosureMap()
          stateQueue += newState
        }
        result.addTransition(state, closureMap(closure), transition)
      }
    }

    // mark final states in the DFA
    for (dfaState <- stateMap.keys; nfaEndState <- nfa.endStates)
      if (stateMap(dfaState).contains(nfaEndState))
        result.finalStates += dfaState

    minimize(result)
  }

  def fromRegex(expression: String) = {
    val regex = Regex(expression)
    val nfa = TransitionDiagramEditor.nfa(regex)
    TransitionDiagramEditor.nfaToDfa(nfa)
  }
}
