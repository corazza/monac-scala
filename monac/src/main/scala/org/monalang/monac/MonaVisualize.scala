package org.monalang.monac

import org.graphstream.graph.Node
import org.monalang.monac.iface.CompileOptions
import org.monalang.monac.lexing.{Lexer, SourceReader}
import org.monalang.monac.parsing._
import org.monalang.monac.symbol.ASTSymbol
import org.graphstream.graph.implementations.{AbstractEdge, SingleGraph}
import org.graphstream.ui.layout.HierarchicalLayout

object MonaVisualize extends App {
  val configuration = CompileOptions(args.toList)
  val sources = configuration.sources.toList
  val readers = sources map SourceReader
  val lexers = readers map Lexer
  val parsers = lexers map Parser

  def visualizeAST(node: ASTNode) {
    val graph = new SingleGraph("Mona AST")
    val viewer = graph.display()
    val hl = new HierarchicalLayout()
    viewer.enableAutoLayout(hl)
    var first = true

    object Color {
      val main = "fill-color: rgb(0, 255, 100);"
      val exp = "fill-color: rgb(0, 0, 0);"
      val call = "fill-color: rgb(255, 0, 255);"
      val cnst = "fill-color: rgb(255, 69, 0);"
      val ident = "fill-color: rgb(0, 128, 255);"
    }

    def displayNode(node: ASTNode, color: String, order: Int): String = {
      val name = node.hashCode.toString
      val vnode = graph.addNode(name).asInstanceOf[Node]

      vnode.addAttribute("ui.label", node.getClass.getSimpleName + (if (order != 0) " " + order.toString else ""))
      vnode.addAttribute("ui.style", color)

      if (first) {
        first = false
      }

      node match {
        case Block(parentScope, scope, statements) => for (i <- 0 to statements.length - 1) {
          val child = displayNode(statements(i), Color.exp, i+1)
          graph.addEdge(name + child, name, child).asInstanceOf[AbstractEdge] // scala/java/graphstream bug
        }

        case FunctionApplication(parentScope, expression, argument) => {
          val childExpression = displayNode(expression, Color.call, 0)
          val childArgument = displayNode(argument, Color.exp, 0)
          graph.addEdge(name + childExpression, name, childExpression).asInstanceOf[AbstractEdge]
          graph.addEdge(name + childArgument, name, childArgument).asInstanceOf[AbstractEdge]
        }

        case BindingExpression(parentScope, identifier) => {
          vnode.changeAttribute("ui.label", identifier.lexeme.data.toString)
//          vnode.addAttribute("ui.style", Color.ident)
        }
        case LiteralExpression(parentScope, literalNode) => {
          vnode.changeAttribute("ui.label", literalNode.lexeme.data.toString)
          vnode.addAttribute("ui.style", Color.cnst)
        }

        case ExpressionStatement(parentScopeCarrier, expression) => {
          val childExpression = displayNode(expression, Color.exp, 0)
          graph.addEdge(name + childExpression, name, childExpression).asInstanceOf[AbstractEdge]
        }
      }

      name
    }

    displayNode(node, Color.main, 0)
  }

  val definitions = parsers.head.parsed.asInstanceOf[DefinitionSequence]
  visualizeAST(definitions.scope.lookup("anotherfunction").get.asInstanceOf[ASTSymbol].node)
}