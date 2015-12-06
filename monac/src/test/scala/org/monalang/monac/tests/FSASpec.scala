package org.monalang.monac.tests

import org.monalang.monac.lexing.FSA
import org.scalatest.FlatSpec

class FSASpec extends FlatSpec {
  "FSA" should "accept correct strings for a given expression" in {
    println("start")
    val fsa = FSA("(SSS*)|(P|,|/|!|#|$|%|^|&|K|O|C|_|+|=|\\|V|~|`|<|>|?|-|\"|')")
    println("finish")
    fsa.reset()
    println(fsa.phase + " " + fsa.currentState)
    fsa.advance('-')
    println(fsa.phase + " " + fsa.currentState)
    fsa.advance('-')
    println(fsa.phase + " " + fsa.currentState)
  }

  it should "reject wrong strings for a given expression" in {

  }
}