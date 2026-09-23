package mmuschalik.predicate.engine

import mmuschalik.predicate.*
import zio.*
import zio.stream.*

sealed trait SolveError
case class ExpectingNumber(t: Term) extends SolveError

// a cut is tagged with the level (barrier) whose alternatives it prunes
private case class Barrier(id: Int)

private sealed trait Step
private case class Solution(s: Subst) extends Step
private case class CutTo(barrier: Int) extends Step

private type Steps = ZStream[Any, SolveError, Step]

def solve(query: Query)(using Program): ZStream[Any, SolveError, Set[Binding]] =
  val queryVariables = query.goals.flatMap(variables).distinct
  solve(bindCuts(query.goals, 0), Map(), 1)
    .collect { case Solution(s) =>
      queryVariables
        .map(v => Binding(resolve(v, s), v))
        .filter(b => b.term != b.variable)
        .toSet
    }

private def bindCuts(goals: List[Goal], barrier: Int): List[Goal] =
  goals.map {
    case Predicate("cut", Nil) => Predicate("cut", List(Atom(Barrier(barrier))))
    case goal => goal
  }

private def solve(goals: List[Goal], s: Subst, depth: Int)(using program: Program): Steps =
  goals match
    case Nil => ZStream.succeed(Solution(s))
    case goal :: rest =>
      goal match
        case Predicate("cut", Atom(Barrier(barrier)) :: Nil) =>
          ZStream.succeed(CutTo(barrier)) ++ solve(rest, s, depth)
        case Predicate("call", c :: Nil) =>
          // call is opaque to cut: a cut inside the called goal only prunes the call itself
          walk(c, s) match
            case p: Predicate => alternatives(depth, List(() => solve(bindCuts(List(p), depth) ++ rest, s, depth + 1)))
            case _ => ZStream.empty
        case Predicate("is", l :: r :: Nil) =>
          ZStream.fromZIO(ZIO.fromEither(evalNumeric(resolve(r, s))))
            .flatMap(n => unify(l, atom(n), s).fold(ZStream.empty)(solve(rest, _, depth)))
        case _ =>
          alternatives(depth, program.get(goal).map { clause => () =>
            val renamed = clause.rename(depth)
            unify(goal, renamed.head, s)
              .fold(ZStream.empty)(solve(bindCuts(renamed.body, depth) ++ rest, _, depth + 1))
          })

// try each branch in order; a cut passing through prunes the remaining branches,
// and is swallowed once it reaches the level it belongs to
private def alternatives(barrier: Int, branches: List[() => Steps]): Steps =
  branches match
    case Nil => ZStream.empty
    case branch :: others =>
      ZStream.unwrap(Ref.make(false).map { cut =>
        branch()
          .tap {
            case CutTo(_) => cut.set(true)
            case _ => ZIO.unit
          }
          .filter {
            case CutTo(b) => b != barrier
            case _ => true
          } ++
        ZStream.unwrap(cut.get.map(if _ then ZStream.empty else alternatives(barrier, others)))
      })
