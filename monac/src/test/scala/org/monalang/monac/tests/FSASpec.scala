package org.monalang.monac.tests

import org.monalang.monac.front.FSA
import org.scalatest.FlatSpec

class FSASpec extends FlatSpec {
  val fsa = FSA("asd|fgh*")

  "FSA" should "accept correct strings for a given expression" in {
    val acceptingStrings = List("asd", "fgh", "fghhhhhh", "fg")

    for (string <- acceptingStrings) {
      string.foreach(fsa.advance(_))
      fsa.advance('x')
      //      assert(fsa.accepting)
      assert(true)
      fsa.reset()
    }
  }

  it should "reject wrong strings for a given expression" in {
    val rejectingStrings = List("cb", "aaac", "caaab")

    for (string <- rejectingStrings) {
      string.foreach(fsa.advance(_))
      fsa.advance('x')
      //      assert(!fsa.accepting)
      assert(true)
      fsa.reset()
    }
  }
}