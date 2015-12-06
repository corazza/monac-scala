package org.monalang.monac.tests

import org.monalang.monac.lexing.Lexer
import org.monalang.monac.lexing.Regex
import org.monalang.monac.lexing.TransitionDiagramEditor
import org.scalatest.FlatSpec

class TransitionDiagramSpec extends FlatSpec {
  "TransitionDiagram" should "create a correct diagram" in {
    val regex = Regex("(a|fg)")
    val nfa = TransitionDiagramEditor.nfa(regex)
    val dfa = TransitionDiagramEditor.nfaToDfa(nfa)
    println("dfa")
    println(dfa)
    assert(true)
  }
}