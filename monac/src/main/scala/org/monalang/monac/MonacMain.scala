package org.monalang.monac

import java.io.{File, InputStreamReader, BufferedReader, FileReader}

import org.monalang.monac.common.util.SmartReader
import org.monalang.monac.lexing._
import org.monalang.monac.iface.CompileOptions
import org.monalang.monac.parsing.Parser

object MonacMain extends App {
  val configuration = CompileOptions(args.toList)
  val sources = configuration.sources.toList
  val readers = sources map SourceReader
  val lexers = readers map Lexer
  val parsers = lexers map Parser

  println(parsers.head.parsed)
}