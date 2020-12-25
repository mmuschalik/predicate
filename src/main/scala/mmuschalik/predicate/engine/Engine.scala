package mmuschalik.predicate.engine

import mmuschalik.predicate._
import zio._
import zio.duration._
import zio.stream._


trait Result
case object Done extends Result
case class Cut(query: Query, bindings: Set[Binding], depth: Int) extends Result

def solve(query: Query)(using Program): ZIO[Any, Nothing, ZStream[Any, Nothing, Set[Binding]]] =
  for
    queue      <- Queue.bounded[Set[Binding]](1)
    stream      = Stream.fromQueue(queue)
    remainder  <- {
                    for 
                      _   <- solve(query, Set(), 1, queue)
                                .onError(_ => queue.shutdown)
                      rem <- queue.takeAll
                      _   <- queue.shutdown
                    yield rem
                  }.fork
  yield stream ++ 
    Stream.fromEffect(remainder.join)
      .flatMap(f => Stream(f :_ *))

def evalWithBreak[A](list: LazyList[ZIO[Any, Nothing, A]], condition: A => Boolean): ZIO[Any, Nothing, Option[A]] =
  list
    .headOption
    .fold(ZIO.succeed(None))
    (head =>
      for
        a <- head
        r <- if condition(a) then ZIO.succeed(Some(a)) else evalWithBreak(list.tail, condition)
      yield r
    )

def isCut(res: Result): Boolean =
  res match
    case c: Cut => true
    case _ => false

def solve(query: Query, bindings: Set[Binding], depth: Int, queue: Queue[Set[Binding]])(using Program): ZIO[Any, Nothing, Result] =
  query.goals
    .headOption
    .fold(
      // solution found
      //println(bindings.map(b => b.show).mkString(", "))
      queue
        .offer(bindings.filter(f => f._2.version == 0))
        .map(_ => Done)
    )
    (goal =>
      goal match
        case Predicate("cut", Nil) => ZIO.succeed(Cut(Query(query.goals.tail), bindings, depth))
        case Predicate("call", c :: Nil) => 
          c match
            case p: Predicate => solve(Query(p :: query.goals.tail), bindings, depth, queue)
            case _ => ZIO.succeed(Done)
        case Predicate("is", (v: Variable) :: (t: Term) :: Nil) =>
          evalNumeric(t)
            .map(m => solve(Query(substitutePredicate(query.goals.tail, Set(Binding(atom(m), v)))), bindings + Binding(atom(m), v), depth + 1, queue))
            .getOrElse(ZIO.die(Exception("Evaluation didn't return a number")))
        case _ => 
          evalWithBreak(
            LazyList(summon[Program].get(goal) :_*)
              .map { clause => 

                val substitutedClause = clause.rename(depth)

                unify(goal, substitutedClause.head)
                  .fold(ZIO.succeed((Done, clause))) { unify => 
                    solve(
                          Query(substitutePredicate(substitutedClause.body ::: query.goals.tail, unify)), 
                          merge(bindings, unify), 
                          depth + 1,
                          queue)
                      .map((_, clause))
                  }
              }, 
            (res: Result, clause: Clause) => isCut(res))
          .flatMap(
            _.fold(ZIO.succeed(Done))
              ((res, clause) => // this has to be a cut, 
                res match
                  case c: Cut if clause.body.contains(cut) =>
                    solve(c.query, c.bindings, c.depth, queue) // this could still return a cut (if more exist)
                  case r => ZIO.succeed(r)))
    )
