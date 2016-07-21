package org.monalang.monac.parsing

import org.monalang.monac.parsing
import org.monalang.monac.symbol._

import scala.util.Try

object ExtraFragments {
  def functionDefinition(flhs: FLHS, expression: Expression) = {
    val symbol = ASTSymbol(expression)
    for (id <- flhs.arguments.arguments)
      expression.parentScope.addSymbol(id.lexeme.data, ArgumentMarker())
    expression.parentScope.addSymbol(flhs.identifier.lexeme.data, symbol)
    Definition(expression.parentScope, flhs.identifier.lexeme.data, symbol)
  }

  def callFromIds(scope: SymbolTable, ids: List[Identifier], expression: Expression) =
    ids.foldRight[Expression](expression)((id, call) =>
      FunctionApplication(scope, BindingExpression(scope, id), call))
}

// TODO: get symbol tables to build correctly, then code emission

object Fragments {
  private def co[A](i: Int)(implicit c: Context): A = c.elements(i-1).asInstanceOf[A]
  private def ge(i: Int)(implicit c: Context): ASTNode = c.elements(i-1)

  def emptyNode(c: Context) = EmptyNode()
  def extract(n: Int)(c: Context) = c.elements(n-1)
  val matched = extract(1) _

  def start(c: Context) = {
    val definition = c.elements(0).asInstanceOf[Definition]

    if (c.elements(1).isInstanceOf[EmptyNode]) {
      val scope = new SymbolTable()
      scope.addSymbol(definition.name, definition.symbol)
      DefinitionSequence(scope, List(definition))
    }
    else {
      val sequence = c.elements(1).asInstanceOf[DefinitionSequence]
      val scope = sequence.scope
      scope.addSymbol(definition.name, definition.symbol)
      DefinitionSequence(scope, definition +: sequence.definitions)
    }
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
      val callScope = new SymbolTable()
      val expression1 = co[Expression](1)
      val InfixRight(operator, expression2) = co[InfixRight](2)
      val firstCall = FunctionApplication(callScope, BindingExpression(callScope, operator), expression1)
      FunctionApplication(callScope, firstCall, expression2)
    }
  }

  def infix(c: Context) = {
    implicit val c2 = c
    InfixRight(co[Operator](1), co[Expression](2))
  }

  def functionExpression(c: Context): ASTNode = {
    implicit val c2 = c

    val first = ge(1) match {
      case literal: LiteralNode => LiteralExpression(new SymbolTable(), literal)
      case binding: Identifier => BindingExpression(new SymbolTable(), binding)
      case expression: Expression => expression
      case ExpressionStatement(parentScope, expression) => expression
      case n: EmptyNode => return n
    }

    val second = Try(ge(2)).toOption
    if (second.isDefined && second.get.isInstanceOf[Expression]) FunctionApplication(new SymbolTable(), first, second.get.asInstanceOf[Expression])
    else first
  }

  def ifExpression(c: Context) = {
    implicit val c2 = c
    val conditional = co[Expression](2)
    val branch1 = co[Expression](4)
    val branch2 = co[Expression](6)
    IfExpression(new SymbolTable(), conditional, branch1, branch2)
  }

  def statementSequence(c: Context): ASTNode = {
    implicit val c2 = c

    val first = ge(1) match {
      case e: Expression => ExpressionStatement(e.parentScope, co[Expression](1))
      case s: Statement => s
      case SimpleContinuation(simpleArgument, continuation) => ExpressionStatement(simpleArgument.parentScope, continuation match {
        case InfixRight(operator, expression) => {
          val firstOpCall = FunctionApplication(new SymbolTable(), BindingExpression(new SymbolTable(), operator), expression)
          FunctionApplication(new SymbolTable(), firstOpCall, simpleArgument)
        }
        case fa @ FunctionApplication(parentScope, function, argument) => FunctionApplication(parentScope, simpleArgument, fa)
        case EmptyNode() => simpleArgument
      })
      case EmptyNode() => return EmptyNode()
    }

    val second = co[ASTNode](2)

    if (second.isInstanceOf[EmptyNode]) first
    else second match {
      case Block(scope, parentScope, statements) => Block(parentScope, scope, first +: statements)
      case s: Statement => Block(s.parentScopeCarrier, new SymbolTable(), List(first, s))
    }
  }

  def definitionOrFE(c: Context) = {
    implicit val c2 = c

    val ids = co[Identifier](1) +: co[IdList](2).ids

    ge(3) match {
      case expression: Expression => {
        val flhs = FLHS(co[LowerId](1), ArgumentList(co[IdList](2).ids.asInstanceOf[List[LowerId]]))
        ExtraFragments.functionDefinition(flhs, expression)
      }
      case _: EmptyNode => {
        val callScope = new SymbolTable()
        ExtraFragments.callFromIds(callScope, ids.dropRight(1), BindingExpression(callScope, ids.last))
      }
      case InfixRight(operator, expression) => {
        val callScope = new SymbolTable()
        val idCall = ExtraFragments.callFromIds(callScope, ids.dropRight(1), BindingExpression(callScope, ids.last))
        val firstOpCall = FunctionApplication(callScope, BindingExpression(new SymbolTable(), operator), expression)
        FunctionApplication(callScope, firstOpCall, idCall)
      }
      case SimpleContinuation(simpleArgument, continuation) => continuation match {
        case InfixRight(operator, expression) => {
          val callScope = new SymbolTable()
          val call = ExtraFragments.callFromIds(callScope, ids, simpleArgument)
          val firstOpCall = FunctionApplication(new SymbolTable(), BindingExpression(new SymbolTable(), operator), call)
          FunctionApplication(new SymbolTable(), firstOpCall, expression)
        }
        case call1: FunctionApplication => {
          val callScope = new SymbolTable()
          val call2 = ExtraFragments.callFromIds(callScope, ids, simpleArgument)
          FunctionApplication(callScope, call2, call1)
        }
        case n: EmptyNode =>ExtraFragments.callFromIds(new SymbolTable(), ids, simpleArgument)
      }
    }
  }

  def simpleContinuation(c: Context) = {
    implicit val c2 = c
    if (ge(1).isInstanceOf[EmptyNode]) parsing.EmptyNode()
    else SimpleContinuation(ge(1) match {
      case e: Expression => e
      case ExpressionStatement(parentScopeCarrier, expression) => expression // (expression already has proper scope)
    }, ge(2))
  }

  def repeatId(c: Context) = {
    implicit val c2 = c
    if (c.elements.length > 1) IdList(co[LowerId](1) +: co[IdList](2).ids)
    else IdList(List())
  }

  def literalExpression(c: Context) = {
    implicit val c2 = c
    LiteralExpression(new SymbolTable(), co[LiteralNode](1))
  }
}