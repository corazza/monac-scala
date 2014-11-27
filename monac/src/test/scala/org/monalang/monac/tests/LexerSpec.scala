package org.monalang.monac.tests

import org.scalatest.FlatSpec
import org.monalang.monac.front.Lexer

class LexerSpec extends FlatSpec {
}

class LexerStaticSpec extends FlatSpec {
  "Lexer" should "strip single quotation marks from a string literal" in {
    assert(Lexer.lexemeToLiteral("\"test\"") == "test")
  }

  it should "strip triple quotation marks from a string literal" in {
    assert(Lexer.lexemeToLiteral("\"\"\"test\"\"\"") == "test")
  }
  
  it should "convert a numerical literal into an internal representation" in {
    assert(Lexer.lexemeToNumerical("9999999999999999999") == BigInt("9999999999999999999"))
  }
}