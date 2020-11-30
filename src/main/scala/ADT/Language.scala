package Prolog.ADT

sealed trait Term
case class Atom[T](a: T) extends Term
case class Variable(name: String, version: Int) extends Term
case class Predicate(name: String, list: List[Term]) extends Term {

  def key: String = name + list.size.toString

  def contains(variable: Variable): Boolean = list.find(f => 
    f match
      case term: Variable => term.name == variable.name
      case term: Predicate => term.contains(variable)
      case _ => false
    ).isDefined

  def &&(right: Predicate) = ClauseBody(List(this,right))

  def :=(body: Predicate) = Clause(this, body :: Nil)

  def :=(list: ClauseBody) = Clause(this, list.body)
}

type Goal = Predicate
case class Query(goals: List[Goal])
case class Clause(head: Goal, body: List[Goal] = Nil)
case class ClauseBody(body: List[Goal])
case class Binding(term: Term, variable: Variable)
case class Program(program: Map[String, List[Clause]]) {
  def get(goal: Goal): List[Clause] = program.getOrElse(goal.name + goal.list.size.toString, Nil)

  def append[T](facts: List[T])(using BuildPredicate[T]): Program = {
    Program(
      this.program ++
      facts
        .map(summon[BuildPredicate[T]].build)
        .groupBy(k => k.name + k.list.size.toString)
        .map(g => g._1 -> (g._2.map(x => Clause(x, Nil)))).toMap)
  }

  def append(clause: Clause): Program = Program(this.program + 
    (clause.head.key -> (this.program.get(clause.head.key).getOrElse(List()) ++ List(clause))))
}

object Program {
  def build: Program = Program(Map())
}

trait BuildPredicate[T] {
  def build(t: T): Predicate
}

def atom[T]: T => Term = t => Atom(t)
def variable(name: String): Term = Variable(name, 0)
def predicate(name: String, terms: Term*): Predicate = Predicate(name, terms.toList)
def query(goals: Goal*): Query = Query(goals.toList)

// Convenience functions

def A = variable("A")
def B = variable("B")
def C = variable("C")
def X = variable("X")
def Y = variable("Y")
def Z = variable("Z")

import scala.language.implicitConversions
implicit def fromInt(a: Int): Term = atom(a)