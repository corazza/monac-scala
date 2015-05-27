package org.monalang.monac

import org.monalang.monac.iface.CompileOptions
import org.monalang.monac.iface.OptionName
import org.monalang.monac.front.Lexer
import java.io.FileReader
import java.io.BufferedReader
import org.monalang.monac.front.Parser
import org.monalang.monac.front.EndOfSource
import org.monalang.monac.front.BreakStatement

object MonacMain extends App {
  val configuration = CompileOptions(args.toList)
  val lexer = new Lexer(new BufferedReader(
    new FileReader("/home/jan/Projects/Mona/monac-run/hello.mona")))
  println(lexer.tokenStream.takeWhile(_ != EndOfSource).toList)
  val parser = new Parser()
}
