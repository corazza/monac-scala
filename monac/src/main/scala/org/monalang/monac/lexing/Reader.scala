package org.monalang.monac.lexing

import java.io.{ByteArrayInputStream, InputStreamReader, FileReader, BufferedReader}

abstract class Reader {
  val inputStream: BufferedReader
}

case class SourceReader(path: String) extends Reader {
  val inputStream = new BufferedReader(new FileReader(path))
}

case class StringReader(inputString: String) extends Reader {
  val inputStream = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(inputString.getBytes()), "ISO-8859-1"))
}
