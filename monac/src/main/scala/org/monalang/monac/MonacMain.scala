package org.monalang.monac

import java.io.{InputStreamReader, BufferedReader, FileReader}

import org.monalang.monac.common.util.Reader
import org.monalang.monac.front._
import org.monalang.monac.iface.CompileOptions

object MonacMain extends App {
  val configuration = CompileOptions(args.toList)
  val lexer = new Lexer(new BufferedReader(new InputStreamReader(getClass().getResourceAsStream("/standard-library/Data/collection.mona"))))

  println(lexer.tokenStream.takeWhile(_ != EndOfSource).toList)

  val parser = new Parser()

  //println(Reader.readWhole(getClass().getResourceAsStream("/standard-library/Data/collection.mona")))
}
