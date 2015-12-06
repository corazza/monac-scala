package org.monalang.monac

import java.io.{File, InputStreamReader, BufferedReader, FileReader}

import org.monalang.monac.common.util.Reader
import org.monalang.monac.lexing._
import org.monalang.monac.iface.CompileOptions
import org.monalang.monac.parsing.Parser

object MonacMain extends App {
  val configuration = CompileOptions(args.toList)
  val lexer = new Lexer(new BufferedReader(new FileReader("/home/jan/Projects/monac-run/test.mona")))
  val parser = new Parser().parse(lexer.tokenStream)
}
