package org.monalang.monac.tests

import org.scalatest.FlatSpec
import org.monalang.monac.front.TransitionDiagramEditor
import org.monalang.monac.front.Regex

// TODO useless test, remove
class TransitionDiagramSpec extends FlatSpec {
  "TransitionDiagram" should "create a correct diagram" in {
    val regex = Regex("(C|S)(C|S|D)*")
    val regex2 = Regex("(a|b)*")
    val nfa = TransitionDiagramEditor.nfa(regex2)
    //    val dfa = TransitionDiagram.nfaToDfa(nfa)
    println(nfa)
    //    println(dfa)
    assert(true)
  }
}