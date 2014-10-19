package org.monalang.monac.common.util

object OptionUtils {
  /**
   * Perform an action that may throw an exception and wrap the result in an Option
   * 
   * @param action An action that returns a result
   * @return None if the action threw an exception, Some(resultOfAction) otherwise
   */
  def fromAction[A](action: => A): Option[A] = {
    try {
      Some[A](action)
    } catch {
      case e: Exception => None
    }
  }
}