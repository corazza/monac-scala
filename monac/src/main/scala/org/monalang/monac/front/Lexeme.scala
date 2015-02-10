package org.monalang.monac.front

/**
 * Stores lexical data about a token.
 *
 * @param data The textual content of the lexeme, e.g. the name of the variable or a numerical constant.
 * @param position (row, column) position at which this lexeme was discovered in the source file
 */
case class Lexeme(data: String, rows: Int, columns: Int)
