package org.monalang.monac.front

object Recognizer {
  // classes
  val whitespace = "\u0020\u0009\u000D\u000A"
  val letter = makeRange('a', 'z') + makeRange('A', 'Z') + "_"
  val digit = "0123456789"
  val id = "+=-/\\<>?V!#%^&~K$"
  val nonid = "\"\'OC[]{}P,;"
  // ' is special as it can occur only at the end
  val characterInner = "\\A|\\uAAAA|L|D|U|N|S|(" + toUnion(without(id, '\\')) + ")"
  // unicode characters which can be in identifiers (I)
  val stringInner = "(\\A|\\uAAAA|L|D|U|I|S|(" + toUnion(without(nonid, '"')) + "))*"
  /**
   * A map of finite state automatons constructed from regular expressions that
   * analyze the input and identify a corresponding token construction function.
   */
  val recognizers = Map(
    // constants
    FSA("DD*") -> integerNumeral _,
    FSA("DD*PDD*") -> floatNumeral _,
    FSA("-(S|T)*DD*") -> integerNumeral _,
    FSA("-(S|T)*DD*PDD*") -> floatNumeral _,
    FSA("\"" + stringInner + "\"") -> stringLiteral _,
    FSA("\"\"\"" + stringInner + "\"\"\"") -> stringLiteral _,
    FSA("'(" + characterInner + ")'") -> characterLiteral _,

    // standalone purely syntactic elements
    FSA("O") -> openParens _,
    FSA("C") -> closeParens _,
    FSA("{") -> openBlock _,
    FSA("}") -> closeBlock _,
    FSA("[") -> openList _,
    FSA("]") -> closeList _,
    FSA("P") -> period _,
    FSA(",") -> comma _,
    FSA(";") -> semicolon _,

    // identifiers and lookalike purely syntactic elements
    FSA("L(L|D)*'*") -> regularIdentifier _,
    FSA("(I|U)(I|U)*") -> special _)

  def makeRange(from: Int, to: Int) = Range(from, to).map(_.toChar).foldLeft("")((acc, n) => acc + n)

  // construction functions
  def integerNumeral(lexeme: ValueLexeme): Token = IntegerNumeral(lexeme)

  def floatNumeral(lexeme: ValueLexeme): Token = FloatNumeral(lexeme)

  def stringLiteral(lexeme: ValueLexeme): Token = StringLiteral(lexeme)

  def characterLiteral(lexeme: ValueLexeme): Token = CharacterLiteral(lexeme)

  def openParens(lexeme: ValueLexeme) = OpenParens(lexeme.toSyntactic)

  def closeParens(lexeme: ValueLexeme) = CloseParens(lexeme.toSyntactic)

  def openBlock(lexeme: ValueLexeme) = OpenBlock(lexeme.toSyntactic)

  def closeBlock(lexeme: ValueLexeme) = CloseBlock(lexeme.toSyntactic)

  def openList(lexeme: ValueLexeme) = OpenList(lexeme.toSyntactic)

  def closeList(lexeme: ValueLexeme) = CloseList(lexeme.toSyntactic)

  def period(lexeme: ValueLexeme) = PeriodToken(lexeme.toSyntactic)

  def comma(lexeme: ValueLexeme) = Comma(lexeme.toSyntactic)

  def semicolon(lexeme: ValueLexeme) = SemiColon(lexeme.toSyntactic)

  def regularIdentifier(lexeme: ValueLexeme): Token = {
    if (lexeme.data(0).isLower) LowerId(lexeme)
    else UpperId(lexeme)
  }

  /**
   * Operator identifiers and purely syntactic elements with the same pattern.
   */
  def special(lexeme: ValueLexeme): Token = lexeme.data match {
    case "->" => FunctionArrow(lexeme.toSyntactic)
    case ":" => StatementType(lexeme.toSyntactic)
    case "=" => EqualsSign(lexeme.toSyntactic)
    case "_" => Underscore(lexeme.toSyntactic)
    case _ => specialIdentifier(lexeme)
  }

  def specialIdentifier(lexeme: ValueLexeme): Token = LowerId(lexeme)

  // helpers for constructing regexes
  def without(string: String, cs: Char*) = string.filter(c => cs.forall(_ != c))

  def toUnion(chars: String): String = chars.toList.map(_ + "|").foldLeft("")((buff, a) => buff + a).init
}
