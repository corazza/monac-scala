package org.monalang.monac

import java.io.{BufferedReader, FileReader}

import org.monalang.monac.front._
import org.monalang.monac.iface.CompileOptions

object MonacMain extends App {
  val configuration = CompileOptions(args.toList)
  val lexer = new Lexer(new BufferedReader(
    new FileReader("/home/jan/Projects/Mona/monac-run/hello.mona")))

  println(lexer.tokenStream.takeWhile(_ != EndOfSource).toList)

  val parser = new Parser()
}
