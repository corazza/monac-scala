package org.monalang.monac.iface

import scala.collection.mutable.ArrayBuffer

import org.monalang.monac.common.util.ListUtil

/**
 * Encapsulates information about a list of command-line argument strings.
 */
case class ParsedConsoleInput[A](
  private val mInputs: List[String],
  optionArguments: List[OptionArgument[A]]) {

  /**
   * Input files specified on the command line
   */
  def inputs: List[String] = {
    if (mInputs.distinct.length != mInputs.length) throw
      new IllegalStateException(Strings.inputDuplicates(ListUtil.getDuplicates(mInputs)))
    mInputs
  }
}

object ParsedConsoleInput {
  /**
   * Handles processing single raw argument strings from the command line.
   */
  def processArgument[A](rootIndexedDefinitions: Map[String, OptionDefinition[A]])(argumentString: String) = {
    val equalsSignIndex = argumentString.indexOf('=')
    val root = if (equalsSignIndex == -1) argumentString
    else argumentString take equalsSignIndex
    rootIndexedDefinitions.get(root) match {
      case Some(definition) => {
        val providedSeparator = if (equalsSignIndex == -1) ' ' else '='
        if (providedSeparator != definition.separator)
          Message.error("Wrong separator for option \"" + argumentString + "\" and its parameters.")
        OptionArgument(definition)
      } case None => InputArgument(argumentString)
    }
  }

  // `=> List` is used to escape duplicate definition due to type erasure
  def apply[A](definitions: => List[OptionDefinition[A]], argumentStrings: List[String]) = {
    val rootIndexedDefinitions = (definitions map { definition =>
      for (form <- definition.forms) yield form -> definition
    } flatten).toMap
    val nameIndexedDefinitions =
      definitions map (definition => definition.name -> definition) toMap

    val argumentProcessor = processArgument(rootIndexedDefinitions) _

    val processedArguments = (argumentStrings foldLeft List[Argument]()) { (cumulativeArguments, currentArgument) =>
      if (cumulativeArguments isEmpty) List(argumentProcessor(currentArgument))
      else cumulativeArguments last match {
        case argument: InputArgument => cumulativeArguments :+ argumentProcessor(currentArgument)
        case argument: OptionArgument[A] => {
          if (argument.parameters.length == nameIndexedDefinitions(argument.optionName).numberOfParameters)
            cumulativeArguments :+ argumentProcessor(currentArgument)
          else cumulativeArguments.init :+ cumulativeArguments.last.asInstanceOf[OptionArgument[A]].withParameter(currentArgument)
        }
      }
    }

    val inputs = ListUtil.allOf[InputArgument, Argument](processedArguments) map (argument => argument.value)
    val duplicateInputs = ListUtil.getDuplicates(inputs)
    if (duplicateInputs.size > 0)
      Message.error(Strings.inputDuplicates(duplicateInputs))
    val options = ListUtil.allOf[OptionArgument[A], Argument](processedArguments)
    optionsMalformationMessages(nameIndexedDefinitions, options) foreach Message.error _
    new ParsedConsoleInput(inputs, options)
  }

  /**
   * Performs checks on a list of option arguments.
   *
   * Tests duplicity of options and numbers of parameters.
   */
  def optionsMalformationMessages[A](
    nameIndexedDefinitions: Map[A, OptionDefinition[A]],
    optionArguments: List[OptionArgument[A]]): List[String] = {
    val optionNames = optionArguments map (_.optionName.toString)
    val duplicateOptionNames = ListUtil.getDuplicates(optionNames).toList
    val duplicationMessages = duplicateOptionNames map (name => Strings.optionDuplicate(name))

    val lackingParameters = optionArguments filter { argument =>
      val definition = nameIndexedDefinitions(argument.optionName)
      argument.parameters.length != definition.numberOfParameters
    }

    val parameterMessages = lackingParameters map { argument =>
      val definition = nameIndexedDefinitions(argument.optionName)
      Strings.optionParameterLacking(
        definition.name.toString,
        definition.numberOfParameters,
        argument.parameters.length)
    }

    duplicationMessages ::: parameterMessages
  }
}
