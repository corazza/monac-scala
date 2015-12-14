package org.monalang.monac

import java.io.{File, PrintWriter}

import org.monalang.monac.lexing._

object LexerPrecompute extends App {
  // create expressions from FSAs
  val expressions = new StringBuilder("")

  Recognizer.recognizers.keys.foreach({ fsa =>
    val expression = fsa.fromExpression
    println(expression)
    val td = TransitionDiagramEditor.fromRegex(expression)
    val matrix = td.toSaveString
    println(td)
    println()
    val dimensions = td.matrix.length.toString
    expressions ++= dimensions + ' ' + expression + '\n' + matrix + '\n'
  })

  // write to file
  val writer = new PrintWriter(new File(this.getClass().getResource("/expressions").getPath()))
  writer.write(expressions.toString)
  writer.flush()
  writer.close()
  assert(true)
}
