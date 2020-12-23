package mmuschalik.predicate

import mmuschalik.predicate.engine.solve

sealed trait Term:
  type This >: this.type <: Term
  type Substitution >: this.type <: Term

  def /(variable: Variable): Binding = Binding(this, variable)
  def show: String
  def substitute(binding: Binding): Substitution
  def contains(variable: Variable): Boolean
  def rename(newVersion: Int): This

case class Atom[T](a: T) extends Term:
  type This = Atom[T]
  type Substitution = This

  def show: String = a.toString
  def substitute(binding: Binding): Substitution = this
  def contains(variable: Variable): Boolean = false
  def rename(newVersion: Int): This = this

case class Variable(name: String, version: Int) extends Term:
  type This = Variable
  type Substitution = Term

  def show: String = if version == 0 then name else name + version.toString
  def substitute(binding: Binding): Substitution = if binding.variable == this then binding.term else this
  def contains(variable: Variable): Boolean = this == variable
  def rename(newVersion: Int): This = if version == 0 then Variable("_" + name, newVersion) else this

case class Predicate(name: String, list: List[Term] = Nil) extends Term:
  type This = Predicate
  type Substitution = Predicate

  def key: String = name + list.size.toString

  def show: String = name + "(" + list.map(_.show).mkString(", ") + ")"

  def contains(variable: Variable): Boolean = list.find(f => 
    f match
      case term: Variable => term.name == variable.name
      case term: Predicate => term.contains(variable)
      case _ => false
    ).isDefined

  def substitute(binding: Binding): Substitution = Predicate(name, list.map(m => m.substitute(binding)))

  def substitute(binding: Set[Binding]): Predicate = binding.foldLeft(this)((a, b) => a.substitute(b))

  def rename(newVersion: Int): This = Predicate(name, list.map(m => m.rename(newVersion)))

  def &&(right: Predicate) = Query(List(this,right))

  def :=(body: Predicate) = Clause(this, body :: Nil)

  def :=(query: Query) = Clause(this, query.goals)

end Predicate

type Goal = Predicate
case class Query(goals: List[Goal]):
  def show: String = goals.map(_.show).mkString(", ")

  def &&(right: Goal) = Query(goals ++ List(right))

case class Clause(head: Goal, body: List[Goal] = Nil):
  def rename(newVersion: Int): Clause = Clause(head.rename(newVersion), body.map(g => g.rename(newVersion)))

case class Binding(term: Term, variable: Variable):
  def show: String = term.show + " /" + variable.show

case class Program(program: Map[String, List[Clause]]):
  def get(goal: Goal): List[Clause] = program.getOrElse(goal.name + goal.list.size.toString, Nil)

  def append[T](facts: List[T])(using BuildPredicate[T]): Program = 
    Program(
      this.program ++
      facts
        .map(summon[BuildPredicate[T]].build)
        .groupBy(k => k.key)
        .map(g => g._1 -> (g._2.map(x => Clause(x)))).toMap)

  def append(clauses: Clause*): Program = clauses.foldLeft(this)((p, clause) => Program(p.program + 
    (clause.head.key -> (p.get(clause.head) ++ List(clause)))))

  def appendFacts(facts: Predicate*): Program = append(facts.map(m => Clause(m)) :_*)

  def solve(query: Query) = engine.solve(query)(using this)
  def solve(goals: Goal*) = engine.solve(Query(goals.toList))(using this)


val A = variable("A")
val B = variable("B")
val C = variable("C")
val X = variable("X")
val Y = variable("Y")
val Z = variable("Z")

val cut = Predicate("cut")
def not(t: Term) = Predicate("not", t :: Nil)
def call(t: Term) = Predicate("call", t :: Nil)

object Program:
  def build: Program = 
    Program(Map())
      .append(
        not(A) := A && cut && false,
        not(A)
      )

trait BuildPredicate[T]:
  def build(t: T): Predicate

def atom[T]: T => Term = t => Atom(t)
def variable(name: String): Variable = Variable(name, 0)
def predicate(name: String, terms: Term*): Predicate = Predicate(name, terms.toList)
def query(goals: Goal*): Query = Query(goals.toList)

// Convenience functions

import scala.language.implicitConversions
implicit def fromInt(a: Int): Term = atom(a)
implicit def fromString(a: String): Term = atom(a)
implicit def fromBool(a: Boolean): Predicate = if a then Predicate("true") else Predicate("False")
implicit def fromPredicate(p: Predicate): Clause = Clause(p)
implicit def fromVariable(v: Variable): Predicate = call(v)