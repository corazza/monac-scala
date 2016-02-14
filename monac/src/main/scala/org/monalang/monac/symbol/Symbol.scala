package org.monalang.monac.symbol

import org.monalang.monac.parsing.{Expression, ASTNode}
import org.monalang.monac.types.{ConcreteType, TypeConstructor}

// entities that can be referred to through source code
abstract class Symbol

case class SymbolToAST(node: ASTNode) extends Symbol

case class ArgumentMarker() extends Symbol

case class FunctionSymbol(expression: Expression) extends Symbol

// modules etc