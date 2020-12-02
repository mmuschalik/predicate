package Prolog.ADT

sealed trait Term {
  def /(variable: Variable): Binding = Binding(this, variable)
  def show: String
  def substitute(binding: Binding): Term
  def contains(variable: Variable): Boolean
}

case class Atom[T](a: T) extends Term {
  def show: String = a.toString
  def substitute(binding: Binding): Term = this
  def contains(variable: Variable): Boolean = false
}

case class Variable(name: String, version: Int) extends Term {
  def show: String = if version == 0 then name else name + version.toString
  def substitute(binding: Binding): Term = if binding.variable == this then binding.term else this
  def contains(variable: Variable): Boolean = this == variable
}

case class Predicate(name: String, list: List[Term]) extends Term {

  def key: String = name + list.size.toString

  def show: String = name + "(" + list.map(_.show).mkString(", ") + ")"

  def contains(variable: Variable): Boolean = list.find(f => 
    f match
      case term: Variable => term.name == variable.name
      case term: Predicate => term.contains(variable)
      case _ => false
    ).isDefined

  def substitute(binding: Binding): Term = Predicate(name, list.map(m => m.substitute(binding)))

  def &&(right: Predicate) = ClauseBody(List(this,right))

  def :=(body: Predicate) = Clause(this, body :: Nil)

  def :=(list: ClauseBody) = Clause(this, list.body)
}

type Goal = Predicate
case class Query(goals: List[Goal]) {
  def show: String = goals.map(_.show).mkString(", ")
}
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
def variable(name: String): Variable = Variable(name, 0)
def predicate(name: String, terms: Term*): Predicate = Predicate(name, terms.toList)
def query(goals: Goal*): Query = Query(goals.toList)

// Convenience functions

val A = variable("A")
val B = variable("B")
val C = variable("C")
val X = variable("X")
val Y = variable("Y")
val Z = variable("Z")

import scala.language.implicitConversions
implicit def fromInt(a: Int): Term = atom(a)
implicit def fromString(a: String): Term = atom(a)