package org.monalang.monac

import org.monalang.monac.front.TransitionDiagramEditor
import org.monalang.monac.common.util.FileUtil
import org.monalang.monac.front.TransitionDiagramEditor
import org.monalang.monac.front.Regex
import org.monalang.monac.front.TransitionDiagramEditor

object LexerPreprocessing extends App {
  val identifier = "identifier"

  def fromFile(path: String /*, regex (all in one file)*/ ): TransitionDiagramEditor = ???

  def toFile(td: TransitionDiagramEditor, path: String) {
    val s = td toString
    val toWrite = s filter (_ != ' ')
    FileUtil.writeToFile(path, toWrite)
  }

  def save(name: String, expression: String) {
    val regex = Regex(expression)
    val nfa = TransitionDiagramEditor.nfa(regex)
    val dfa = TransitionDiagramEditor.nfaToDfa(nfa)
    toFile(dfa, name)
  }

  // create name-diagram pairs
  save(identifier, "regex")

  //HERE
}
