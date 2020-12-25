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

case class Binding(term: Term, variable: Variable):

  def show: String = 
    term.show + " /" + variable.show

case class Program(program: Map[String, List[Clause]]):

  def get(goal: Goal): List[Clause] = 
    program
      .getOrElse(goal.name + goal.list.size.toString, Nil)

  def append[T](facts: List[T])(using BuildPredicate[T]): Program = 
    Program(
      this.program ++
      facts
        .map(summon[BuildPredicate[T]].build)
        .groupBy(k => k.key)
        .map(g => g._1 -> (g._2.map(x => Clause(x)))).toMap)

  def append(clauses: Clause*): Program = 
    clauses.foldLeft(this)((p, clause) => 
      Program(p.program + 
        (clause.head.key -> (p.get(clause.head) ++ List(clause)))))

  def appendFacts(facts: Predicate*): Program = 
    append(facts.map(m => Clause(m)) :_*)

  def solve(query: Query) = 
    engine.solve(query)(using this)

  def solve(goals: Goal*) = 
    engine.solve(Query(goals.toList))(using this)


object Program:

  def build: Program = 
    Program(Map())
      .append(
        eql(A, A), 
        
        not(A) := A && cut && false,
        not(A)
      )

trait BuildPredicate[T]:

  def build(t: T): Predicate

