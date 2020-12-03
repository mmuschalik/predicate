package Prolog

import Prolog.ADT._
import Prolog.Operation._
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
  //stack.peek.foreach(f => println(f.query.show))
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
      val substitutedClause = clause.rename(state.depth)

      unify(goal, substitutedClause.head)
        .flatMap(f => fetch(substitutedClause, index, f))
    }
    .find(_.isDefined)
    .flatten

def buildPredicates[T](facts: List[T])(using BuildPredicate[T]): List[Predicate] = 
  facts.map(summon[BuildPredicate[T]].build)

def substitute(list: List[(Term, Term)], sub: Set[Binding]): List[(Term, Term)] =
  substituteTerm(list.map(_._1), sub) zip substituteTerm(list.map(_._2), sub)

def substituteTerm(list: List[Term], sub: Set[Binding]): List[Term] =
  list.map(m => (sub.foldLeft(m)((a, b) => a.substitute(b))))

def substitutePredicate(list: List[Predicate], sub: Set[Binding]): List[Predicate] =
  list.map(m => (sub.foldLeft(m)((a, b) => a.substitute(b))))
