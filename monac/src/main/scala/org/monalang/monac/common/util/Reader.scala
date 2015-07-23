package org.monalang.monac.common.util

import java.io.InputStream

// TODO docs

object Reader {
  def readWhole(inputStream: InputStream) = readUntil(inputStream, (-1).asInstanceOf[Char])

  def readUntil(inputStream: InputStream, stop: Char) = {
    val result = new StringBuilder("")
    Reader.read(inputStream, { c =>
      if (c == stop) {
        false
      } else {
        result += c
        true
      }
    })
    result.toString
  }

  def read(inputStream: InputStream, readCommand: Char => Boolean) {
    var reading = true
    var c = inputStream.read().asInstanceOf[Char]
    while (notEnd(c) && reading) {
      reading = readCommand(c)
      if (reading) c = inputStream.read().asInstanceOf[Char]
    }
  }

  def notEnd(c: Char) = c != (-1).asInstanceOf[Char]
}