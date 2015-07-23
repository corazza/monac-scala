package org.monalang.monac.tests

import java.io.{BufferedReader, ByteArrayInputStream, InputStreamReader}

import org.monalang.monac.front._
import org.scalatest.FlatSpec

class LexerSpec extends FlatSpec {
  "Lexer" should "correctly tokenize an input string" in {
    val inputString = """
      : :;
      /=
      this is :: Test -    9.99 000 -100 9
      {}{
      { } {
      { "another, test" } 'a' '\n' '\A'
      """

    val lexer = new Lexer(new BufferedReader(new InputStreamReader(new ByteArrayInputStream(inputString.getBytes()), "ISO-8859-1")))

    val correctTokens = List[Token](
      DoubleColon(SyntacticLexeme(2, 6)), DoubleColon(SyntacticLexeme(2, 8)), SemiColon(SyntacticLexeme(2, 9)),
      LowerId(ValueLexeme(3, 6, "/=")),
      LowerId(ValueLexeme(4, 6, "this")), LowerId(ValueLexeme(4, 11, "is")), LowerId(ValueLexeme(4, 14, "::")), UpperId(ValueLexeme(4, 17, "Test")),
      FloatNumeral(ValueLexeme(4, 22, "-    9.99")), IntegerNumeral(ValueLexeme(4, 32, "000")), IntegerNumeral(ValueLexeme(4, 36, "-100")), IntegerNumeral(ValueLexeme(4, 41, "9")),
      OpenBlock(SyntacticLexeme(5, 6)), CloseBlock(SyntacticLexeme(5, 7)), OpenBlock(SyntacticLexeme(5, 8)),
      OpenBlock(SyntacticLexeme(6, 6)), CloseBlock(SyntacticLexeme(6, 8)), OpenBlock(SyntacticLexeme(6, 10)),
      OpenBlock(SyntacticLexeme(7, 6)), StringLiteral(ValueLexeme(7, 8, "\"another, test\"")), CloseBlock(SyntacticLexeme(7, 24)),
      CharacterLiteral(ValueLexeme(7, 26, "'a'")), CharacterLiteral(ValueLexeme(7, 30, """'\n'""")), CharacterLiteral(ValueLexeme(7, 35, """'\A'""")))

    val output = lexer.tokenStream.takeWhile(_ != EndOfSource).toList.filter(_ match {
      case t: Newlines => false
      case _ => true
    })
    val pairs = output zip correctTokens

    pairs foreach { pair =>
      // check position and content of lexemes
      val token = pair._1
      val correct = pair._2
      assert(token == correct)
    }
  }
}

class LexerStaticSpec extends FlatSpec {
  "Lexer" should "strip single quotation marks from a string literal" in {
    assert(LexerConversions.lexemeToStringLiteral("\"test\"") == "standard-library/test")
  }

  it should "strip triple quotation marks from a string literal" in {
    assert(LexerConversions.lexemeToStringLiteral("\"\"\"test\"\"\"") == "standard-library/test")
  }

  it should "convert a numeral literal into an internal representation" in {
    assert(LexerConversions.lexemeToIntegerNumeral("9999999999999999999") == BigInt("9999999999999999999"))
  }
}
