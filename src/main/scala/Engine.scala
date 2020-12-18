package Prolog

import Prolog.ADT._
import Prolog.Operation._
import zio.stream.Stream

def solve(query: Query)(using Program): Stream[Nothing, Set[Binding]] =
  val i = ResultIterator(query).to(Iterable)
  Stream.fromIterable(i)
    .map(_.filter(_._2.version == 0))

def next(query: Query)(using p: Program): Option[Result] = 
  next(Stack(State(query, 0, Set(), 1) :: Nil))

def getGoal(g: Goal): Goal =
  g match
    case Predicate("call", (p: Predicate) :: Nil) => getGoal(p)
    case _ => g

    /*
def next(subQuery: Query, stack: Stack[State])(using Program): Option[Result] =
  stack.peek.foreach(f => println("?: " + f.query.show))
  if subQuery.goals.contains(cut) then
    next(stack)
      .flatMap(r => {
        println("cut returned")
        val n = r.stack.peek.fold(Stack.empty)(s => stack.pop.pop.push(s))
        // check could have no more queries, in that case, just return the result, dont do next
        next(n)
        }) // returned from cut, continue on with the stack from the result    [maybe think of looking for additional !]
  else
    next(stack) */

def next(stack: Stack[State])(using Program): Option[Result] = 
  stack.peek.foreach(f => println("?: " + f.query.show + " " + f.index.toString))
  for {
    state     <- stack.peek
    goal      <- state.query.goals.headOption.map(getGoal)
    if goal != pFalse
    result    <- 
      if(goal == cut) then 
        Some(Result(Stack.empty.push(state.copy(query = Query(state.query.goals.tail))), Some(state.solution), true))
      else
        findUnifiedClause(stack, state, goal, getResult(stack, state))
          .orElse(next(stack.pop))
  } yield result

def getResult(stack: Stack[State], state: State)(using Program): (Clause, Int, Set[Binding]) => Option[Result] = (clause, foundIndex, bindings) =>
  val nextPosition = stack.pop.push(state.copy(index = foundIndex + 1))
  val goalRemainder = state.query.goals.tail
  val newBindings = merge(state.solution, bindings)

  println(bindings.map(_.show).mkString(", "))

  if goalRemainder.isEmpty && clause.body.isEmpty then
    println("solved. ")
    Some(Result(nextPosition, Some(newBindings)))
  else
    next(nextPosition.push(State(Query(substitutePredicate(clause.body ::: goalRemainder, bindings)), 0, newBindings, state.depth + 1)))

    
def findUnifiedClause(stack: Stack[State], state: State, goal: Goal, fetch: (Clause, Int, Set[Binding]) => Option[Result])(using Program): Option[Result] =
  LazyList(summon[Program].get(goal) :_*)
    .zipWithIndex
    .drop(state.index)
    .map { (clause, index) => 
      val substitutedClause = clause.rename(state.depth)

      println(goal.show + " = " + substitutedClause.head.show)

      for {
        unified <- unify(goal, substitutedClause.head)
        result <- fetch(substitutedClause, index, unified)
      } yield (clause, result)
    }
    .find(f => f.isDefined)
    .flatMap(f => f.flatMap { m => 
      println("yes, got a result.")
      println(m._1)
      // passed the cut on this clause, try solving the rest
      if(m._1.body.contains(cut) && m._2.isCut) then
        println("cut found here, continue on...")
        next(m._2.stack.peek.fold(Stack.empty)(s => stack.pop.push(s)))
      else // either cutting back further or a normal valid result, we can return
        println("yeah done.")
        Some(m._2)
      })


class ResultIterator(query: Query)(using program: Program) extends collection.Iterator[Set[Binding]] {
  var result: Option[Result] = None
  var start = true

  def hasNext: Boolean = 
      result = if start then Prolog.next(query) else result.flatMap(r => Prolog.next(r.stack))
      start = false
      result.isDefined

  def next(): Set[Binding] = 
    result
      .flatMap(f => f.solution)
      .getOrElse(Set())
}