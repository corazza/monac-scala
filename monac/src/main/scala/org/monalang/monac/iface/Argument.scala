package org.monalang.monac.iface

class Argument
case class InputArgument(value: String) extends Argument
case class OptionArgument[A](optionName: A, parameters: List[String]) extends Argument {
  def withParameter(parameter: String) = OptionArgument[A](optionName, parameters :+ parameter)
}

object OptionArgument {
  /**
   * Creates an OptionArgument with no parameters
   *
   * @definition An OptionDefinition from which to construct the OptionArgument
   */
  def apply[A](definition: OptionDefinition[A]) = new OptionArgument(definition.name, List())
}
