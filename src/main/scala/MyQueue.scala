package MyQueueP

import zio._
import Prolog.ADT._
import Prolog.Operation._

def test() = {
  for {
    queue <- Queue.bounded[Set[Binding]](1)
    _     <- queue.offer(Set())
    b     <- queue.take
  } yield b
}

trait Result
case object Done extends Result
case class Cut(query: Query, bindings: Set[Binding], depth: Int) extends Result

def solve(query: Query, bindings: Set[Binding], depth: Int)(using Program): Result = {
  query.goals
    .headOption
    .fold {
      // solution found
      println(bindings.map(b => b.show).mkString(", "))
      Done
    }(goal =>
      if goal == cut then
        Cut(Query(query.goals.tail), bindings, depth)
      else 
        LazyList(summon[Program].get(goal) :_*)
          .map { clause => 

            val substitutedClause = clause.rename(depth)

            unify(goal, substitutedClause.head)
              .fold((Done, clause)) { unify => 
                val res = solve(
                            Query(substitutePredicate(substitutedClause.body ::: query.goals.tail, unify)), 
                            merge(bindings, unify), 
                            depth + 1)
                (res, clause)
              }
          }
          .find((res, clause) => 
            res match
              case c: Cut => true
              case _ => false)
          .fold(Done)((res, clause) => // this has to be a cut, 
            res match
              case c: Cut if clause.body.contains(cut) =>
                solve(c.query, c.bindings, c.depth) // this could still return a cut (if more exist)
              case _ => Done) 
    )
}