package org.monalang.monac.front

import scala.collection.mutable.ArrayBuffer
import org.monalang.monac.types.TypeConstructor
import org.monalang.monac.types.ConcreteType

// entities that can be referred to through source code
abstract class Symbol

case class DataVariable(concreteType: ConcreteType, constant: Boolean) extends Symbol
class TypeVariable extends Symbol
case class ConcreteTypeName(concreteType: ConcreteType) extends Symbol
case class TypeConstructorName(typeConstructor: TypeConstructor) extends Symbol

// modules etc