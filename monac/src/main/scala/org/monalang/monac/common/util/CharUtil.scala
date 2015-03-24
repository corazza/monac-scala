package org.monalang.monac.common.util

object CharUtil {
  def isSpecial(c: Char) = !c.isLetterOrDigit && !c.isWhitespace
}