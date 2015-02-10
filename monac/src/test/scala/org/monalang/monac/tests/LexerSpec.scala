package org.monalang.monac.tests

import org.scalatest.FlatSpec
import org.monalang.monac.front.LexerConversions

class LexerSpec extends FlatSpec {
}

class LexerStaticSpec extends FlatSpec {
  "Lexer" should "strip single quotation marks from a string literal" in {
    assert(LexerConversions.lexemeToLiteral("\"test\"") == "test")
  }

  it should "strip triple quotation marks from a string literal" in {
    assert(LexerConversions.lexemeToLiteral("\"\"\"test\"\"\"") == "test")
  }
  
  it should "convert a numeral literal into an internal representation" in {
    assert(LexerConversions.lexemeToNumeral("9999999999999999999") == BigInt("9999999999999999999"))
  }
}