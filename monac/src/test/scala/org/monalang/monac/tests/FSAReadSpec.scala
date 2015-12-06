package org.monalang.monac.tests

import org.monalang.monac.lexing._
import org.scalatest.FlatSpec

class FSAReadSpec extends FlatSpec {
  "FSA" should "read the expressions file correctly" in {
    println(Recognizer.recognizers.keys.head.fromExpression)
    println(Recognizer.recognizers.keys.last.fromExpression)
  }
}