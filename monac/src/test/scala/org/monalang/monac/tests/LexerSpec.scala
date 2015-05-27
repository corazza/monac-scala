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
import org.monalang.monac.front.StatementType
import org.monalang.monac.front.BreakStatement
import org.monalang.monac.front.EndOfSource
import org.monalang.monac.front.OpenBlock
import org.monalang.monac.front.CloseBlock

class LexerSpec extends FlatSpec {
  "Lexer" should "correctly tokenize an input string" in {
    val inputString = """
      ::;
      /=
      this is :: test 9.99 000 100
      {}{
      { } {
      { "another, test" } 'a' '\n' '\A'
      """

    val lexer = new Lexer(new BufferedReader(new InputStreamReader(new ByteArrayInputStream(inputString.getBytes()), "ISO-8859-1")))

    val correctTokens = List[Token](
      StatementType(Lexeme("::", 2, 6)),
      Identifier(Lexeme("/=", 3, 6)),
      Identifier(Lexeme("this", 4, 6)), Identifier(Lexeme("is", 4, 11)), StatementType(Lexeme("::", 4, 14)), Identifier(Lexeme("test", 4, 17)),
      FloatNumeral(Lexeme("9.99", 4, 22)), IntegerNumeral(Lexeme("000", 4, 27)), IntegerNumeral(Lexeme("100", 4, 31)),
      OpenBlock(Lexeme("{", 5, 6)), CloseBlock(Lexeme("}", 5, 7)), OpenBlock(Lexeme("{", 5, 8)),
      OpenBlock(Lexeme("{", 6, 6)), CloseBlock(Lexeme("}", 6, 8)), OpenBlock(Lexeme("{", 6, 10)),
      OpenBlock(Lexeme("{", 7, 6)), StringLiteral(Lexeme("\"another, test\"", 7, 8)), CloseBlock(Lexeme("}", 7, 24)),
      CharacterLiteral(Lexeme("'a'", 7, 26)), CharacterLiteral(Lexeme("""'\n'""", 7, 30)), CharacterLiteral(Lexeme("""'\A'""", 7, 35)))

    val output = lexer.tokenStream.takeWhile(_ != EndOfSource).toList.filter(_ != BreakStatement)
    val pairs = correctTokens zip output
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
