package org.monalang.monac.parsing

import java.io.{File, PrintWriter}

import org.monalang.monac.common.util.SmartReader

import scala.collection.mutable.HashMap
import scala.reflect.ClassTag
import scala.util.Try

object GrammarPrecompute extends App {
  def saveParseTable(name: String, parseTable: Map[NonTerminal, Map[LogicalTerminal, Int]]): Unit = {
    val parseTableSave = new StringBuilder("")

    parseTable foreach { case (nt, tr) => {
      val simpleName = nt.getClass.getSimpleName
      parseTableSave ++= (if (simpleName.endsWith("$")) simpleName.take(simpleName.length - 1)
      else simpleName) + '\n'

      tr foreach ((tr) => {
        tr match {
          case (End, r) => parseTableSave ++= "End " + r
          case (Eta, r) => parseTableSave ++= "Eta " + r
          case (Terminal(c), r) => parseTableSave ++= c.runtimeClass.getSimpleName + " " + r
        }

        parseTableSave ++= "\n"
      })

      parseTableSave ++= "\n"
    }}

    val writer = new PrintWriter(new File(this.getClass().getResource("/parsing-tables/mona").getPath()))
    writer.write(parseTableSave.toString)
    writer.flush()
    writer.close()
  }

  def toImmutable(mut: HashMap[NonTerminal, HashMap[LogicalTerminal, Int]]) = {
    mut.toMap map { case (k, v) => (k, v.toMap) }
  }

  def loadParseTable(name: String) = {
    // temporary, should use reflection, not clear how objects can be obtained from their names
    def getNonTerminal(name: String): NonTerminal = name match {
      case "StartNT" => StartNT
      case "StartPrimeNT" => StartPrimeNT
      case "OptionalNewlinesNT" => OptionalNewlinesNT
      case "DefinitionSeparatorNT" => DefinitionSeparatorNT
      case "DefinitionNT" => DefinitionNT
      case "FLHSNT" => FLHSNT
      case "ArgumentListHeadNT" => ArgumentListHeadNT
      case "ArgumentListNT" => ArgumentListNT
      case "RHSNT" => RHSNT
      case "ExpressionNT" => ExpressionNT
      case "ExpressionPrimeNT" => ExpressionPrimeNT
      case "SimpleExpressionNT" => SimpleExpressionNT
      case "OperatorNT" => OperatorNT
      case "StatementNT" => StatementNT
      case "StatementSequenceNT" => StatementSequenceNT
      case "StatementSequencePrimeNT" => StatementSequencePrimeNT
      case "StatementSeparatorNT" => StatementSeparatorNT
      case "DefinitionOrExpressionNT" => DefinitionOrExpressionNT
      case "DefinitionOrExpressionRepeatIdNT" => DefinitionOrExpressionRepeatIdNT
      case "DefinitionOrExpressionPrimeNT" => DefinitionOrExpressionPrimeNT
      case "DefinitionOrExpressionPrimePrimeNT" => DefinitionOrExpressionPrimePrimeNT
      case "BlockNT" => BlockNT
      case "FunctionExpressionNT" => FunctionExpressionNT
      case "FunctionExpressionPrimeNT" => FunctionExpressionPrimeNT
      case "ArgumentNT" => ArgumentNT
      case "SimpleArgumentNT" => SimpleArgumentNT
      case "SimpleArgumentPrimeNT" => SimpleArgumentPrimeNT
      case "LiteralNT" => LiteralNT
      case _ => StartNT // error
    }

    val parseTable = HashMap[NonTerminal, HashMap[LogicalTerminal, Int]]()
    val inputStream = getClass().getResourceAsStream("/parsing-tables/" + name)
    var reading = true

    def addBlock(name: String) {
      var reading = true

      while (reading) {
        val line = SmartReader.readUntil(inputStream, '\n')

        if (line != "") {
          val split = line.split(" ")
          val terminal = split(0)
          val rule = split(1).toInt
          if (terminal != "End") Grammar.addEntry(parseTable, getNonTerminal(name), Terminal(ClassTag(Class.forName("org.monalang.monac.lexing." + terminal))), rule)
          else Grammar.addEntry(parseTable, getNonTerminal(name), End, rule)
        }
        else reading = false
      }
    }

    while (reading) Try(SmartReader.readUntil(inputStream, '\n')).toOption match {
        case Some(name) => if (name == "") reading = false
                           else addBlock(name)
        case None => reading = false
    }

    toImmutable(parseTable)
  }

  saveParseTable(MonaGrammar.name, toImmutable(MonaGrammar.constructParseTable))
}