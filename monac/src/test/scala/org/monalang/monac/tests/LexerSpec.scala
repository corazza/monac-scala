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
  
  it should "correctly tokenize an input string" in {
      val inputString = """
      identifier
      /=
      this is a test 9.99 000 100
      {}{
      { } {
      { another, test }
      """
      
      val lexer = new Lexer(inputString)
      
      val correctTokens = List()
      
      (correctTokens zip lexer.tokenStream) foreach { (token, correct) => 
          // check position and content of lexemes
          assert(token == correct)
      }
  }
}
