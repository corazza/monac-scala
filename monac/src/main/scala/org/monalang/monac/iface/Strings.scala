package org.monalang.monac.iface

/**
 * Interface to externalized strings
 *
 * TODO actually externalize strings
 */
object Strings {
  def inputDuplicates(names: Iterable[String]): String =
    "duplicate inputs: " + names.mkString(" ")

  def optionDuplicate(name: String): String =
    "duplicate option \"" + name + "\""

  def optionParameterLacking(name: String, takes: Int, provided: Int) =
    "Option \"" + name + "\" takes " + takes + " parameters, " + provided + " provided."
}