package org.monalang.monac.symbol

import org.monalang.monac.parsing.ASTNode

// entities that can be referred to through source code
abstract class Symbol

case class ASTSymbol(node: ASTNode) extends Symbol

case class ArgumentMarker() extends Symbol

// modules etc