package org.monalang.monac.common.util

import java.io.FileWriter

object FileUtil {
  def writeToFile(path: String, data: String) {
    val w = new FileWriter(path)
    try {
      w.write(data)
    } finally {
      w.close()
    }
  }
}