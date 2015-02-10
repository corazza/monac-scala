package org.monalang.monac.common.util

object CharUtil {
  def isWhitespace(a: Char) = a == ' ' || a == '\n' || a == '\t'
}