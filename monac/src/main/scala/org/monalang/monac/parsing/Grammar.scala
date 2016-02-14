package org.monalang.monac.parsing

import org.monalang.monac.lexing.{OperatorId, Token}
import org.monalang.monac.symbol.SymbolTable

import scala.reflect._
import scala.collection.mutable.{ArrayBuffer, HashMap, Set}

/**
  * Contains information about the context of a production.
  *
  *  - elements of production (for creating the AST / further processing)
  */
case class Context(elements: List[ASTNode])

class Grammar(val name: String, val rules: List[((NonTerminal, List[Symbol]), Context=>ASTNode)]) {
  import Grammar._

  def constructParseTable() = {
    val parseTable = HashMap[NonTerminal, HashMap[LogicalTerminal, Int]]()
    val first = HashMap[Symbol, Set[LogicalTerminal]]()

    def getFirst(s: Symbol) = {
      if (first.contains(s)) first(s)
      else {
        first(s) = Set()
        first(s)
      }
    }

    def firstMult(in: List[Symbol]) = {
      val result = Set[LogicalTerminal]()
      var pass = true
      var i = 0

      while (pass && i < in.length) {
        val elfirst = getFirst(in(i))
        result ++= elfirst filter (_ != Eta)
        if (!elfirst.contains(Eta)) pass = false
        else i += 1
      }

      if (pass && i == in.length && in.length != 0) result += Eta

      result
    }

    for (rule <- rules) {
      val nonterminal = rule._1._1
      val elements = rule._1._2

      for (symbol <- elements) symbol match {
        case a: LogicalTerminal => first(a) = Set(a)
        case _ => Unit
      }

      if (elements == List(Eta)) {
        getFirst(nonterminal) += Eta
      }
    }

    var added = true
    while (added) {
      added = false

      for (rule <- rules) {
        val nonterminal = rule._1._1
        val elements = rule._1._2
        val ntfirst = getFirst(nonterminal)

        var pass = true
        var i = 0
        while (pass && i < elements.length) {
          val elfirst = getFirst(elements(i)) filter (_ != Eta)

          if (!elfirst.subsetOf(ntfirst)) {
            ntfirst ++= elfirst
            added = true
          }

          if (!elfirst.contains(Eta)) pass = false
          else i += 1
        }

        if (pass && i == elements.length && !ntfirst.contains(Eta)) {
          ntfirst += Eta
          added = true
        }
      }
    }

    val follow = HashMap[Symbol, Set[LogicalTerminal]]()

    def getFollow(s: Symbol) = {
      if (follow.contains(s)) follow(s)
      else {
        follow(s) = Set()
        follow(s)
      }
    }

    getFollow(StartNT) += End

    added = true
    while (added) {
      added = false

      for (rule <- rules) {
        val nonterminal = rule._1._1
        val elements = rule._1._2
        val followsnt = getFollow(nonterminal)

        var i = 0
        while (i < elements.length) {
          val firstAfteri = firstMult(elements.slice(i+1, elements.length+1))

          elements(i) match {
            case e: NonTerminal => {
              val followse = getFollow(e)
              val firstAdd = firstAfteri filter (_ != Eta)

              if (!firstAdd.subsetOf(followse)) {
                followse ++= firstAdd
                added = true
              }

              if (!followsnt.subsetOf(followse) && (firstAfteri.contains(Eta) || followse.isEmpty)) {
                followse ++= followsnt
                added = true
              }
            }

            case _ => Unit
          }

          i += 1
        }
      }
    }

    for (rule <- 0 to rules.length - 1) {
      val nonterminal = rules(rule)._1._1
      val elements = rules(rule)._1._2
      val elsfirst = firstMult(elements)

      for (terminal <- elsfirst filter (_ != Eta)) addEntry(parseTable, nonterminal, terminal, rule)

      if (elsfirst.contains(Eta)) {
        val followsnt = getFollow(nonterminal)
        for (terminal <- followsnt) addEntry(parseTable, nonterminal, terminal, rule)
        if (followsnt.contains(End)) addEntry(parseTable, nonterminal, End, rule)
      }
    }

    parseTable
  }

//  val parseTable = constructParseTable
  val parseTable = GrammarPrecompute.loadParseTable(name)
}

object Grammar {
  def addEntry(parseTable: HashMap[NonTerminal, HashMap[LogicalTerminal, Int]], nonterminal: NonTerminal, terminal: LogicalTerminal, rule: Int) = {
    if (!parseTable.contains(nonterminal)) {
      parseTable(nonterminal) = HashMap()
    }

    if (parseTable(nonterminal).contains(terminal) && parseTable(nonterminal)(terminal) != rule) {
      println(nonterminal)
      println(terminal)
      println(parseTable(nonterminal)(terminal))
      println(rule)
      throw new Exception("Grammar needs to be left-factored")
    }

    parseTable(nonterminal)(terminal) = rule
  }
}

abstract class Symbol

abstract class LogicalTerminal extends Symbol
object Eta extends LogicalTerminal
object End extends LogicalTerminal
case class Terminal[A <: Token](token: ClassTag[A]) extends LogicalTerminal

abstract class NonTerminal extends Symbol

// operations

case class RepeatP(s: Symbol) extends Symbol