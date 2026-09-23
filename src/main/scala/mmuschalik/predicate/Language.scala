package mmuschalik.predicate

import mmuschalik.predicate.engine.solve

case class Query(goals: List[Goal]):

  def show: String = 
    goals
      .map(_.show)
      .mkString(", ")

  def &&(right: Goal) = 
    Query(goals ++ List(right))

case class Clause(head: Goal, body: List[Goal] = Nil):

  def rename(newVersion: Int): Clause = 
    Clause(head.rename(newVersion), body.map(g => g.rename(newVersion)))

// one solution: the values of the query's variables that were bound
final class Answer(val bindings: Map[Variable, Term]):

  def apply(variable: Variable): Term = 
    bindings(variable)

  def get(variable: Variable): Option[Term] = 
    bindings.get(variable)

  def as[T](variable: Variable)(using decoder: Decoder[T]): Either[DecodeError, T] = 
    get(variable)
      .toRight(DecodeError(variable.show + " is unbound"))
      .flatMap(decoder.decode)

  def show: String = 
    bindings
      .toList
      .sortBy(_._1.name)
      .map((v, t) => v.show + " = " + t.show)
      .mkString(", ")

  override def equals(other: Any): Boolean = 
    other match
      case a: Answer => bindings == a.bindings
      case _ => false

  override def hashCode: Int = 
    bindings.hashCode

  override def toString: String = 
    "Answer(" + show + ")"

object Answer:

  // a single non-overloaded apply, so A -> 1 converts the value to a Term
  def apply(bindings: (Variable, Term)*): Answer = 
    new Answer(bindings.toMap)

// declares a predicate by name: val woman = Functor("woman"); woman(jean)
case class Functor(name: String):

  def apply(args: Term*): Predicate = 
    Predicate(name, args.toList)

case class Program(program: Map[(String, Int), List[Clause]]):

  def get(goal: Goal): List[Clause] = 
    program
      .getOrElse(goal.key, Nil)

  def append[T](facts: List[T])(using BuildPredicate[T]): Program = 
    appendFacts(facts.map(summon[BuildPredicate[T]].build)*)

  def append(clauses: Clause*): Program = 
    clauses.foldLeft(this)((p, clause) => 
      Program(p.program + 
        (clause.head.key -> (p.get(clause.head) ++ List(clause)))))

  def appendFacts(facts: Predicate*): Program = 
    append(facts.map(m => Clause(m))*)

  def solve(query: Query) = 
    engine.solve(query)(using this)

  def solve(goals: Goal*) = 
    engine.solve(Query(goals.toList))(using this)


object Program:

  def build: Program = 
    Program(Map())
      .append(Library.lists*)

trait BuildPredicate[T]:

  def build(t: T): Predicate

