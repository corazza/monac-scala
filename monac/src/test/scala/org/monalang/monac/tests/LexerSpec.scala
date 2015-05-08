package org.monalang.monac.tests

import java.io.BufferedReader
import java.io.ByteArrayInputStream
import java.io.InputStreamReader
import scala.BigInt
import org.monalang.monac.front.Identifier
import org.monalang.monac.front.Lexeme
import org.monalang.monac.front.Lexer
import org.monalang.monac.front.LexerConversions
import org.monalang.monac.front.IntegerNumeral
import org.monalang.monac.front.FloatNumeral
import org.monalang.monac.front.Token
import org.scalatest.FlatSpec
import org.monalang.monac.front.StringLiteral
import org.monalang.monac.front.CharacterLiteral

class LexerSpec extends FlatSpec {
  "Lexer" should "correctly tokenize an input string" in {
    val inputString = """
      identifier
      /=
      this is a test 9.99 000 100
      {}{
      { } {
      { "another, test" } 'a' '\n' '\A'
      """

    val lexer = new Lexer(new BufferedReader(new InputStreamReader(new ByteArrayInputStream(inputString.getBytes()), "ISO-8859-1")))

    val correctTokens = List[Token](
      Identifier(Lexeme("identifier", 2, 6)),
      Identifier(Lexeme("/=", 3, 6)),
      Identifier(Lexeme("this", 4, 6)), Identifier(Lexeme("is", 4, 11)), Identifier(Lexeme("a", 4, 14)), Identifier(Lexeme("test", 4, 16)),
      FloatNumeral(Lexeme("9.99", 4, 21)), IntegerNumeral(Lexeme("000", 4, 26)), IntegerNumeral(Lexeme("100", 4, 30)),
      Identifier(Lexeme("{}{", 5, 6)),
      Identifier(Lexeme("{", 6, 6)), Identifier(Lexeme("}", 6, 8)), Identifier(Lexeme("{", 6, 10)),
      Identifier(Lexeme("{", 7, 6)), StringLiteral(Lexeme("\"another, test\"", 7, 8)), Identifier(Lexeme("}", 7, 24)),
      CharacterLiteral(Lexeme("'a'", 7, 26)), CharacterLiteral(Lexeme("""'\n'""", 7, 30)), CharacterLiteral(Lexeme("""'\A'""", 7, 35)))

    val pairs = correctTokens zip lexer.tokenStream
    println(pairs)

    pairs foreach { pair =>
      // check position and content of lexemes
      val correct = pair._1
      val token = pair._2
      assert(token == correct)
    }
  }

  // TODO test unicode
}

class LexerStaticSpec extends FlatSpec {
  "Lexer" should "strip single quotation marks from a string literal" in {
    assert(LexerConversions.lexemeToStringLiteral("\"test\"") == "test")
  }

  it should "strip triple quotation marks from a string literal" in {
    assert(LexerConversions.lexemeToStringLiteral("\"\"\"test\"\"\"") == "test")
  }

  it should "convert a numeral literal into an internal representation" in {
    assert(LexerConversions.lexemeToIntegerNumeral("9999999999999999999") == BigInt("9999999999999999999"))
  }
}
