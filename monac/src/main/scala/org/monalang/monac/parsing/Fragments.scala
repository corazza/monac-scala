package org.monalang.monac.parsing

import org.monalang.monac.parsing
import org.monalang.monac.symbol._

import scala.util.Try

object ExtraFragments {
  def functionDefinition(flhs: FLHS, expression: Expression) = {
    val symbol = ASTSymbol(expression)
    val definitionScope = new SymbolTable()

    for (id <- flhs.arguments.arguments)
      definitionScope.addSymbol(id.lexeme.data, ArgumentMarker())

    definitionScope.addSymbol(flhs.identifier.lexeme.data, symbol)

    expression match {
      case ScopedExpression(scope, _) => scope.parent = Some(definitionScope)
      case _ => Unit
    }

    Definition(definitionScope, flhs.identifier.lexeme.data, symbol)
  }

  def callFromIds(ids: List[Identifier], expression: Expression) =
    ids.foldRight[Expression](expression)((id, call) =>
      FunctionApplication(BindingExpression(id), call))
}

object Fragments {
  private def co[A](i: Int)(implicit c: Context): A = c.elements(i-1).asInstanceOf[A]
  private def ge(i: Int)(implicit c: Context): ASTNode = c.elements(i-1)

  def emptyNode(c: Context) = EmptyNode()
  def extract(n: Int)(c: Context) = c.elements(n-1)
  val matched = extract(1) _

  def start(c: Context) = {
    val definition = c.elements(0).asInstanceOf[Definition]

    val (definitionSequence, fileScope) = if (c.elements(1).isInstanceOf[EmptyNode]) {
      val scope = new SymbolTable()
      scope.addSymbol(definition.name, definition.symbol)
      (DefinitionSequence(scope, List(definition)), scope)
    }
    else {
      val sequence = c.elements(1).asInstanceOf[DefinitionSequence]
      val scope = sequence.scope
      scope.addSymbol(definition.name, definition.symbol)
      (DefinitionSequence(scope, definition +: sequence.definitions), scope)
    }

    definition.scope.parent = Some(fileScope)
    definitionSequence
  }

  def functionDefinition(c: Context) = {
    implicit val c2 = c
    val flhs = co[FLHS](1)
    val expression = co[Expression](2)
    ExtraFragments.functionDefinition(flhs, expression)
  }

  def FLHSNT(c: Context) = {
    implicit val c2 = c
    FLHS(co[LowerId](1), co[ArgumentList](2))
  }

  def argumentListHead(c: Context) = {
    implicit val c2 = c
    if (ge(2).isInstanceOf[EmptyNode]) ArgumentList(List(co[LowerId](1)))
    else ArgumentList(co[LowerId](1) +: co[ArgumentList](2).arguments)
  }

  def expression(c: Context) = {
    implicit val c2 = c
    if (c.elements.length == 1 || ge(2).isInstanceOf[EmptyNode]) ge(1)
    else {
      val expression1 = co[Expression](1)
      val InfixRight(operator, expression2) = co[InfixRight](2)
      val firstCall = FunctionApplication(BindingExpression(operator), expression1)
      FunctionApplication(firstCall, expression2)
    }
  }

  def infix(c: Context) = {
    implicit val c2 = c
    InfixRight(co[Operator](1), co[Expression](2))
  }

  def functionExpression(c: Context): ASTNode = {
    implicit val c2 = c
    val first = ge(1) match {
      case literal: LiteralNode => LiteralExpression(literal)
      case identifier: Identifier => BindingExpression(identifier)
      case expression: Expression => expression
      case ExpressionStatement(expression) => expression
      case n: EmptyNode => return n
    }

    val second = Try(ge(2)).toOption
    if (second.isDefined && second.get.isInstanceOf[Expression]) FunctionApplication(first, second.get.asInstanceOf[Expression])
    else first
  }

  def ifExpression(c: Context) = {
    implicit val c2 = c
    val conditional = co[Expression](2)
    val branch1 = co[Expression](4)
    val branch2 = co[Expression](6)
    IfExpression(conditional, branch1, branch2)
  }

  def statementSequence(c: Context): ASTNode = {
    implicit val c2 = c

    val first = ge(1) match {
      case e: Expression => ExpressionStatement(co[Expression](1))
      case s: Statement => s
      case SimpleContinuation(simpleArgument, continuation) => ExpressionStatement(continuation match {
        case InfixRight(operator, expression) => {
          val firstOpCall = FunctionApplication(BindingExpression(operator), expression)
          FunctionApplication(firstOpCall, simpleArgument)
        }
        case fa @ FunctionApplication(function, argument) => FunctionApplication(simpleArgument, fa)
        case EmptyNode() => simpleArgument
      })
      case EmptyNode() => return EmptyNode()
    }

    val second = co[ASTNode](2)

    val block = if (second.isInstanceOf[EmptyNode]) {
      val blockScope = new SymbolTable()
      ScopedExpression(blockScope, List(first))
    } else second match {
        case ScopedExpression(scope, statements) => ScopedExpression(scope, first +: statements)
    }

    first match {
      case Definition(scope, name, _) => scope.parent = Some(scope)
      case ExpressionStatement(ScopedExpression(innerScope, _)) => innerScope.parent = Some(block.scope)
      case _ => Unit
    }

    block
  }

  def definitionOrFE(c: Context) = {
    implicit val c2 = c

    val ids = co[Identifier](1) +: co[IdList](2).ids

    ge(3) match {
      case expression: Expression => {
        val flhs = FLHS(co[LowerId](1), ArgumentList(co[IdList](2).ids.asInstanceOf[List[LowerId]]))
        ExtraFragments.functionDefinition(flhs, expression)
      }
      case _: EmptyNode => ExtraFragments.callFromIds(ids.dropRight(1), BindingExpression(ids.last))
      case InfixRight(operator, expression) => {
        val idCall = ExtraFragments.callFromIds(ids.dropRight(1), BindingExpression(ids.last))
        val firstOpCall = FunctionApplication(BindingExpression(operator), expression)
        FunctionApplication(firstOpCall, idCall)
      }
      case SimpleContinuation(simpleArgument, continuation) => continuation match {
        case InfixRight(operator, expression) => {
          val call = ExtraFragments.callFromIds(ids, simpleArgument)
          val firstOpCall = FunctionApplication(BindingExpression(operator), call)
          FunctionApplication(firstOpCall, expression)
        }
        case call1: FunctionApplication => {
          val call2 = ExtraFragments.callFromIds(ids, simpleArgument)
          FunctionApplication(call2, call1)
        }
        case n: EmptyNode => ExtraFragments.callFromIds(ids, simpleArgument)
      }
    }
  }

  def simpleContinuation(c: Context) = {
    implicit val c2 = c
    if (ge(1).isInstanceOf[EmptyNode]) parsing.EmptyNode()
    else SimpleContinuation(ge(1) match {
      case e: Expression => e
      case ExpressionStatement(expression) => expression // (expression already has proper scope)
    }, ge(2))
  }

  def repeatId(c: Context) = {
    implicit val c2 = c
    if (c.elements.length > 1) IdList(co[LowerId](1) +: co[IdList](2).ids)
    else IdList(List())
  }

  def literalExpression(c: Context) = {
    implicit val c2 = c
    LiteralExpression(co[LiteralNode](1))
  }
}