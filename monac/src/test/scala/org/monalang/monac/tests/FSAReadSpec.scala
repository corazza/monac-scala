package org.monalang.monac.tests

import org.monalang.monac.front.Lexer
import org.monalang.monac.front.Regex
import org.monalang.monac.front.TransitionDiagramEditor
import org.scalatest.FlatSpec
import org.monalang.monac.front.FSA

class FSAReadSpec extends FlatSpec {
  "FSA" should "read the expressions file correctly" in {
    println(Lexer.recognizers.keys.head.fromExpression)
    println(Lexer.recognizers.keys.last.fromExpression)
  }
}