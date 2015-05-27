package org.monalang.monac.types

// predefined types (Int, Char, ...)
/**
 * @param size size of type in bytes
 */
abstract class AbstractType(size: Int) extends ConcreteType
object IntATunsigned32 extends AbstractType(4)
object CharAT extends AbstractType(1)
