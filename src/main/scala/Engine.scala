package Prolog

import Prolog.ADT._
import zio.stream.Stream

class ResultIterator(program: Program, query: Query) extends collection.Iterator[Set[Binding]] {
  var result: Option[Result] = None

  def hasNext: Boolean = 
    result = result.fold(Prolog.next(query)(using program))(r => Prolog.next(r.stack)(using program))
    result.isDefined

  def next(): Set[Binding] = 
    result
      .flatMap(f => f.solution)
      .getOrElse(Set())
}

def solve(query: Query)(using Program): Stream[Nothing, Set[Binding]] =
  Stream.fromIterable(new Iterable[Set[Binding]] { def iterator = ResultIterator(summon[Program], query) })

def next(query: Query)(using p: Program): Option[Result] = 
  next(Stack(State(query, 0, Set(), 1) :: Nil))

def next(stack: Stack[State])(using Program): Option[Result] = 
  stack.peek.foreach(f => println(f.query.show))
  for {
    state     <- stack.peek
    goal      <- state.query.goals.headOption
    result    <- findUnifiedClause(state, goal, getResult(stack, state))
                   .orElse(next(stack.pop))
  } yield result

def getResult(stack: Stack[State], state: State)(using Program): (Clause, Int, Set[Binding]) => Option[Result] = (clause, foundIndex, bindings) =>
  val nextPosition = stack.pop.push(state.copy(index = foundIndex + 1))
  val goalRemainder = state.query.goals.tail
  val newBindings = merge(state.solution, bindings)

  if goalRemainder.isEmpty && clause.body.isEmpty then
    Some(Result(nextPosition, Some(newBindings)))
  else
    next(nextPosition.push(State(Query(substitutePredicate(clause.body ::: goalRemainder, bindings)), 0, newBindings, state.depth + 1)))

    
def findUnifiedClause(state: State, goal: Goal, fetch: (Clause, Int, Set[Binding]) => Option[Result])(using Program): Option[Result] =
  LazyList(summon[Program].get(goal) :_*)
    .zipWithIndex
    .drop(state.index)
    .map { (clause, index) => 
      val substitutedClause = renameVariables(clause, state.depth)

      unify(goal, substitutedClause.head)
        .flatMap(f => fetch(substitutedClause, index, f))
    }
    .find(_.isDefined)
    .flatten

def renameVariables(predicate: Predicate, version: Int): Predicate = 
  new Predicate(predicate.name, predicate.list.map(a => a match {
    case v: Variable if v.version == 0 => Variable("_" + v.name, version)
    case p: Predicate => renameVariables(p, version)
    case _ => a
  }))

def renameVariables(clause: Clause, version: Int): Clause = 
  Clause(renameVariables(clause.head, version), clause.body.map(g => renameVariables(g, version)))

def buildPredicates[T](facts: List[T])(using BuildPredicate[T]): List[Predicate] = 
  facts.map(summon[BuildPredicate[T]].build)

def unify(t1: Term, t2: Term): Option[Set[Binding]] = 
  (t1, t2) match
    case (_: Variable, _) | (_: Atom[_], _) | (_, _: Variable) | (_, _: Atom[_]) if t1 == t2 => Some(Set())
    case (v: Variable, p: Predicate) if p.contains(v) => None 
    case (v: Variable, _) => Some(Set(Binding(t2, v)))
    case (p: Predicate, v: Variable) if p.contains(v) => None 
    case (_, v: Variable) => Some(Set(Binding(t1, v)))
    case (_: Variable, _) | (_: Atom[_], _) | (_, _: Variable) | (_, _: Atom[_]) => None
    case (p1: Predicate, p2: Predicate) 
      if p1.name == p2.name && p1.list.size == p2.list.size => 
        unify(p1.list zip p2.list, Set())
    case _ => None

def unify(list: List[(Term, Term)], result: Set[Binding]): Option[Set[Binding]] =
  list match
    case Nil => Some(result)
    case (a::as) => 
      unify(a._1, a._2)
        .flatMap(r => unify(substitute(as, r), result ++ r))

def substitute(list: List[(Term, Term)], sub: Set[Binding]): List[(Term, Term)] =
  substituteTerm(list.map(_._1), sub) zip substituteTerm(list.map(_._2), sub)

def substituteTerm(list: List[Term], sub: Set[Binding]): List[Term] =
  list.map(m => (sub.foldLeft(m)((a, b) => substitute(a, b))))

def substitutePredicate(list: List[Predicate], sub: Set[Binding]): List[Predicate] =
  list.map(m => (sub.foldLeft(m)((a, b) => substitutePredicate(a, b))))

def substitutePredicate(p: Predicate, s: Binding): Predicate =
  p.copy(list = p.list.map(m => substitute(m, s)))

def substitute(t: Term, s: Binding): Term =
  t match
    case v: Variable if v == s.variable => s.term
    case p: Predicate => p.copy(list = p.list.map(m => substitute(m, s)))
    case _ => t // no substitution required

def merge(left: Set[Binding], right: Set[Binding]): Set[Binding] =
  merge(left.map(m => m.variable -> m.term).toMap, right.map(m => m.variable -> m.term).toMap)
    .map(m => Binding(m._2,m._1))
    .toSet

def merge(left: Map[Variable, Term], right: Map[Variable, Term]): Map[Variable, Term] =
  left.map(l => 
      l._2 match
        case v: Variable => l._1 -> right.get(v).getOrElse(v)
        case _ => l
    ).toMap ++ right