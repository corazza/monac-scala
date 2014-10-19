package org.monalang.monac.iface

case class OptionDefinition[A](
  name: A,
  forms: List[String],
  numberOfParameters: Int = 1,
  separator: Char = ' ')