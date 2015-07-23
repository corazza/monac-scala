package org.monalang.monac.iface
import org.monalang.monac.common.util.ListUtil

object OptionName extends Enumeration {
  type OptionName = Value

  val OUTPUT = Value("output")
  val ARCHITECTURE = Value("sets the target architecture")
}
import OptionName._

/* 
 * NOTE currently there is a 1-1 correspondence between command-line arguments
 * and compile options, Should parse parameterMap -> optionMap, and return from
 * optionMap
 */
case class CompileOptions(
  sources: Set[String],
  objects: Set[String],
  private val parameterMap: Map[OptionName, List[String]],
  private val duplicateOptions: Set[OptionName]) {

  /* 
   * TODO parse, use configuration objects for options
   * like architecture
   */
  val optionMap = parameterMap

  /**
   * Get option parameters
   * 
   * @return scala.Option containing a list of parameters for the specified OptionName
   */
  def get(name: OptionName): Option[List[String]] = {
    if (duplicateOptions contains name) throw
      new IllegalStateException("Duplicate option \"" + name + "\"")

    optionMap.get(name)
  }
}

object CompileOptions {
  private val argumentDefinitions = List(
    OptionDefinition(OUTPUT, List("-o", "--output")),
    OptionDefinition(ARCHITECTURE, List("-a", "--architecture")))

  private def isSource(argument: String) = argument.endsWith(".mona")

  def apply(argumentStrings: List[String]) = {
    val parsedArguments = ParsedConsoleInput(argumentDefinitions, argumentStrings)

    val optionParameterMap = parsedArguments.optionArguments map (argument =>
      argument.optionName -> argument.parameters) toMap

    val optionNames = parsedArguments.optionArguments map (_.optionName)

    // Determined now so it can be passed on to CompileOptions which reports on access
    val duplicateOptionNames = ListUtil.getDuplicates(optionNames)

    val sources = parsedArguments.inputs filter isSource
    val objects = parsedArguments.inputs filter (!isSource(_))

    new CompileOptions(sources.toSet, objects.toSet, optionParameterMap, duplicateOptionNames)
  }
}