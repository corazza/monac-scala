package org.monalang.monac.common.util

import org.monalang.monac.front.Recognizer

object CharUtil {
  def isUnicode(c: Char) = !Recognizer.whitespace.contains(c) && !Recognizer.letter.contains(c) && !Recognizer.digit.contains(c) && !Recognizer.id.contains(c) && !Recognizer.nonid.contains(c)
}