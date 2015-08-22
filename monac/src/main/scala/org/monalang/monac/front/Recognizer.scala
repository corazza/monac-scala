package org.monalang.monac.front

/**
 * FSA recognizers used by the lexer, character classes enumeration.
 */
object Recognizer {
  val specialCharacters: Map[Char, Char] = Map(
    'P' -> '.',
    'E' -> '\n',
    'O' -> '(',
    'C' -> ')',
    'V' -> '|',
    '*' -> '*',
    'S' -> ' ',
    'T' -> '\t'
  )

  // TODO complete character set (additional ASCII chars and unicode symbols
  object SpecialCharacters {
    val whitespace = "\u0020\u0009\u000D\u000A"
    val letter = makeRange('a', 'z') + makeRange('A', 'Z') + "_"
    val digit = "0123456789"
    val id = "+=-/\\<>?V!#%^&~K$" // $ is an operator
    val nonid = "\"\'OC[]{}P,;`"
    // ' (apostrophe) is special as it can occur only at the end of identifiers
    // unicode characters which can be in identifiers (U) - defined by exclusion
  }

  object LiteralCharacters {
    val whitespace = SpecialCharacters.whitespace
    val letter = SpecialCharacters.letter
    val digit = SpecialCharacters.digit
    val id = SpecialCharacters.id.map((c: Char) => specialCharacters.getOrElse(c, c))
    val nonid = SpecialCharacters.nonid.map((c: Char) => specialCharacters.getOrElse(c, c))
  }

  val characterInner = "\\A|\\uAAAA|L|D|U|N|S|(" + toUnion(without(SpecialCharacters.id, '\\')) + ")"
  val stringInner = "(\\A|\\uAAAA|L|D|U|I|S|(" + toUnion(without(SpecialCharacters.nonid, '"')) + "))*"

  /* A map of finite state automatons constructed from regular expressions that
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
    FSA("PP") -> doublePeriod _,
    FSA(",") -> comma _,
    FSA(";") -> semicolon _,
    FSA("EE*") -> newlines _,

    // identifiers and lookalike purely syntactic elements
    FSA("L(L|D)*'*") -> regularIdentifier _,
    FSA("(I|U)(I|U)*") -> special _)

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
  def period(lexeme: ValueLexeme) = Period(lexeme.toSyntactic)
  def doublePeriod(lexeme: ValueLexeme) = DoublePeriod(lexeme.toSyntactic)
  def comma(lexeme: ValueLexeme) = Comma(lexeme.toSyntactic)
  def semicolon(lexeme: ValueLexeme) = SemiColon(lexeme.toSyntactic)
  def newlines(lexeme: ValueLexeme) = Newlines(lexeme.toSyntactic)

  def regularIdentifier(lexeme: ValueLexeme): Token = {
    if (lexeme.data(0).isLower) LowerId(lexeme)
    else UpperId(lexeme)
  }

  /**
   * Operator identifiers and purely syntactic elements with the same pattern.
   */
  def special(lexeme: ValueLexeme): Token = lexeme.data match {
    case "->" => FunctionArrow(lexeme.toSyntactic)
    case ":" => DoubleColon(lexeme.toSyntactic)
    case "=" => EqualsSign(lexeme.toSyntactic)
    case "_" => Underscore(lexeme.toSyntactic)
    case _ => specialIdentifier(lexeme)
  }

  def specialIdentifier(lexeme: ValueLexeme): Token = LowerId(lexeme)

  // helpers for constructing regexes
  def without(string: String, cs: Char*) = string.filter(c => cs.forall(_ != c))
  def makeRange(from: Int, to: Int) = Range(from, to).map(_.toChar).foldLeft("")((acc, n) => acc + n)
  def toUnion(chars: String): String = chars.toList.map(_ + "|").foldLeft("")((buff, a) => buff + a).init
}
