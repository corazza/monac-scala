package org.monalang.monac.common.util

import scala.reflect.ClassTag
import scala.reflect.classTag

object ListUtil {
  /**
   * Used for extracting a specific subtype out of a list of supertypes.
   *
   * @return all elements of type A (as elements of type A) in a list of Bs where A is a subtype of B.
   */
  def allOf[A: ClassTag, B >: A](bs: List[B]): List[A] =
    (bs filter { b =>
      b match {
        case a if classTag[A].runtimeClass.isInstance(a) => true
        case _ => false
      }
    }).asInstanceOf[List[A]]

  def getDuplicates[A](in: List[A]): Set[A] = (in diff in.distinct).distinct.toSet
}