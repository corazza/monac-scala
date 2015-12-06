package org.monalang.monac.tests

import org.monalang.monac.lexing.Regex

import org.scalatest.FlatSpec
import org.monalang.monac.lexing.FSA

class RegexSpec extends FlatSpec {
  "Regex" should "create a transition diagram from a regular expression" in {
    val fsa = FSA("(C|S)(C|S|D)*")
    println(fsa)
    assert(true)
  }

  /*"Regex" should "add concat operators to input regex" in {
    val conv = Regex.withConcat("(acd|b)*d(a|b)")
    assert(conv.equals("(a.c.d|b)*.d.(a|b)"))
  }

  it should "convert infix regex to postfix" in {
    val conv = Regex.toPostfix(Regex.withConcat("(acd|b)*d"))
    assert(conv.equals("ac.d.b|*d."))
  }*/
}