package org.monalang.monac

import org.monalang.monac.front.TransitionDiagram
import org.monalang.monac.common.util.FileUtil
import org.monalang.monac.front.TransitionDiagram
import org.monalang.monac.front.Regex

object LexerPreprocessing extends App {
  val identifier = "identifier"

  def fromFile(path: String): TransitionDiagram = {
    val source = scala.io.Source.fromFile(path)
    val s = source mkString "\n"

    val rows = s split '\n'
    val nstates = rows length

    val td = new TransitionDiagram(nstates)

    for (i <- 0 to nstates - 1) {
      for (j <- 0 to nstates - 1) {
        td.addTransition(i, j, rows(i)(j))
      }
    }

    td
  }

  def toFile(td: TransitionDiagram, path: String) {
    val s = td toString
    val toWrite = s filter (_ != ' ')
    FileUtil.writeToFile(path, toWrite)
  }

  def save(name: String, expression: String) {
    val regex = Regex(expression)
    val nfa = TransitionDiagram.nfa(regex)
    val dfa = TransitionDiagram.nfaToDfa(nfa)
    toFile(dfa, name)
  }

  // create name-diagram pairs
  save(identifier, "regex")

  //HERE
}
