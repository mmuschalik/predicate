package Prolog

import Prolog.ADT._
import Prolog.Operation._
import zio.stream.Stream

def solve(query: Query)(using Program): Stream[Nothing, Set[Binding]] =
  Stream.fromIterable(new Iterable[Set[Binding]] { def iterator = ResultIterator(query) })
    .map(_.filter(_._2.version == 0))

def next(query: Query)(using p: Program): Option[Result] = 
  next(Stack(State(query, 0, Set(), 1) :: Nil))

def getGoal(g: Goal): Goal =
  g match
    case Predicate("call", (p: Predicate) :: Nil) => getGoal(p)
    case _ => g

def next(stack: Stack[State])(using Program): Option[Result] = 
  //stack.peek.foreach(f => println(f.query.show))
  for {
    state     <- stack.peek
    goal      <- state.query.goals.headOption.map(getGoal)
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


class ResultIterator(query: Query)(using program: Program) extends collection.Iterator[Set[Binding]] {
  var result: Option[Result] = None

  def hasNext: Boolean = 
    result = result.fold(Prolog.next(query))(r => Prolog.next(r.stack))
    result.isDefined

  def next(): Set[Binding] = 
    result
      .flatMap(f => f.solution)
      .getOrElse(Set())
}