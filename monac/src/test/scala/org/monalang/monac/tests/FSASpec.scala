package org.monalang.monac.tests

import org.scalatest.FlatSpec
import org.monalang.monac.front.FSA

class FSASpec extends FlatSpec {
  "FSA" should "accept correct strings for a given expression" in {
    val fsa = FSA("a*|(a|b)")
    val testStrings = List("", "aaa")
  }
}