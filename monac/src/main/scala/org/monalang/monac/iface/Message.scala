package org.monalang.monac.iface

/**
 * Interface to the textual output of the compiler.
 */
object Message {
  /**
   * Error that allows for the compilation to continue until
   * illegal data is accessed.
   *
   * Exceptions handled by the accessor of data.
   */
  def error(message: String) {
    println(message)
  }

  def assert(condition: Boolean, message: String) {
    if (!condition) error(message)
  }
}