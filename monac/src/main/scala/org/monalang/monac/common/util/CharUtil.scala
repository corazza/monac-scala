package org.monalang.monac.common.util

import org.monalang.monac.front.Recognizer

object CharUtil {
  def isUnicode(c: Char) = !Recognizer.LiteralCharacters.whitespace.contains(c) && !Recognizer.LiteralCharacters.letter.contains(c) && !Recognizer.LiteralCharacters.digit.contains(c) && !Recognizer.LiteralCharacters.id.contains(c) && !Recognizer.LiteralCharacters.nonid.contains(c)
}