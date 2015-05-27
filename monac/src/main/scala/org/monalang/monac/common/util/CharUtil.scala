package org.monalang.monac.common.util

import org.monalang.monac.front.Lexer

object CharUtil {
  def isSpecial(c: Char) = Lexer.special.contains(c)
}