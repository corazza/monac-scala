package org.monalang.monac.tests

import org.monalang.monac.front.Lexer
import org.scalatest.FlatSpec
import org.monalang.monac.front.TransitionDiagramEditor
import java.io.PrintWriter
import java.io.File

class OneCreateRegexFileSpec extends FlatSpec {
  "OneCreateRegexFile" should "create a regex file" in {
    // create expressions from FSAs    
    val expressions = new StringBuilder("")

    Lexer.recognizers.keys.foreach({ fsa =>
      val expression = fsa.fromExpression
      val td = TransitionDiagramEditor.fromRegex(expression)
      val matrix = td.toSaveString
      val dimensions = td.matrix.length.toString
      expressions ++= dimensions + ' ' + expression + '\n' + matrix + '\n'
    })

    // write to file
    val writer = new PrintWriter(new File(this.getClass().getResource("/expressions").getPath()))
    println(this.getClass().getResource("/expressions").getPath())
    writer.write(expressions.toString)
    writer.flush()
    writer.close()
    assert(true)
  }
}
