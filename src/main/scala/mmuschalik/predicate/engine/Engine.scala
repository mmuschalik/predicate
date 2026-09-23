package mmuschalik.predicate.engine

import mmuschalik.predicate.*
import zio.*
import zio.stream.*

// a cut is tagged with the level (barrier) whose alternatives it prunes
private val taggedCut = "$cut"

private sealed trait Step
// depth is the next free variable version, so a continuation never reuses one
private case class Solution(s: Subst, depth: Int) extends Step
private case class CutTo(barrier: Int) extends Step

private type Steps = ZStream[Any, Raised, Step]

def solve(query: Query)(using Program): ZStream[Any, SolveError, Answer] =
  val queryVariables = query.goals.flatMap(variables).distinct
  solve(bindCuts(query.goals, 0), Map(), 1)
    .mapError(raised => Errors.toSolveError(raised.ball))
    .collect { case Solution(s, _) =>
      new Answer(
        queryVariables
          .map(v => v -> resolve(v, s))
          .filter((v, t) => t != v)
          .toMap)
    }

private def bindCuts(goals: List[Goal], barrier: Int): List[Goal] =
  goals.map(bindCut(_, barrier))

// tags cuts that belong to the enclosing clause; conjunction, disjunction and the branches of
// if-then-else are transparent to cut, everything else (call, not, catch, a condition) is opaque
private def bindCut(goal: Goal, barrier: Int): Goal =
  goal match
    case Predicate("cut", Nil) => Predicate(taggedCut, List(num(barrier)))
    case Predicate(op @ ("," | ";"), l :: r :: Nil) =>
      Predicate(op, List(bindCut(asGoal(l), barrier), bindCut(asGoal(r), barrier)))
    case Predicate("->", c :: t :: Nil) =>
      Predicate("->", List(c, bindCut(asGoal(t), barrier)))
    case other => other

private def asGoal(t: Term): Goal =
  t match
    case p: Predicate => p
    case other => call(other)

// solves a goal on its own, without the continuation; cuts inside it stay local
private def isolated(goal: Term, s: Subst, depth: Int)(using Program): ZStream[Any, Raised, Solution] =
  alternatives(depth, List(() => solve(List(bindCut(asGoal(goal), depth)), s, depth + 1)))
    .collect { case solution: Solution => solution }

private def solve(goals: List[Goal], s: Subst, depth: Int)(using program: Program): Steps =
  goals match
    case Nil => ZStream.succeed(Solution(s, depth))
    case goal :: rest =>
      goal match
        case Predicate(`taggedCut`, Num(barrier) :: Nil) =>
          ZStream.succeed(CutTo(barrier.toInt)) ++ solve(rest, s, depth)
        case Predicate(",", l :: r :: Nil) =>
          solve(asGoal(l) :: asGoal(r) :: rest, s, depth)
        case Predicate(";", Predicate("->", c :: t :: Nil) :: e :: Nil) =>
          ifThenElse(c, asGoal(t), Some(asGoal(e)), rest, s, depth)
        case Predicate("->", c :: t :: Nil) =>
          ifThenElse(c, asGoal(t), None, rest, s, depth)
        case Predicate(";", l :: r :: Nil) =>
          // a cut in either branch prunes the other branch too, and belongs to an enclosing level
          alternatives(-1, List(
            () => solve(asGoal(l) :: rest, s, depth),
            () => solve(asGoal(r) :: rest, s, depth)))
        case Predicate("call", c :: Nil) =>
          walk(c, s) match
            case p: Predicate => alternatives(depth, List(() => solve(bindCut(p, depth) :: rest, s, depth + 1)))
            case Atom(name) => solve(Predicate("call", List(Predicate(name, Nil))) :: rest, s, depth)
            case _: Variable => ZStream.fail(Raised(Errors.instantiation, depth))
            case other => ZStream.fail(Raised(Errors.typeError("callable", other), depth))
        case Predicate("not", g :: Nil) =>
          ZStream.unwrap(isolated(g, s, depth).runHead.map {
            case Some(_) => ZStream.empty
            case None => solve(rest, s, depth + 1)
          })
        case Predicate("throw", ball :: Nil) =>
          resolve(ball, s) match
            case _: Variable => ZStream.fail(Raised(Errors.instantiation, depth))
            case b => ZStream.fail(Raised(b, depth))
        case Predicate("catch", g :: catcher :: recovery :: Nil) =>
          // only errors raised by the goal itself are caught, not those of the continuation;
          // bindings made by the goal are undone before unifying the catcher
          isolated(g, s, depth)
            .catchAll(raised =>
              unify(catcher, raised.ball, s).fold(ZStream.fail(raised))(caught =>
                isolated(recovery, caught, raised.depth)))
            .flatMap(solution => solve(rest, solution.s, solution.depth))
        case Predicate(name, args) if Builtins.all.contains((name, args.size)) =>
          Builtins.all((name, args.size))(args, s, depth)
            .flatMap(solve(rest, _, depth + 1))
        case _ =>
          alternatives(depth, program.get(goal).map { clause => () =>
            val renamed = clause.rename(depth)
            unify(goal, renamed.head, s)
              .fold(ZStream.empty)(solve(bindCuts(renamed.body, depth) ++ rest, _, depth + 1))
          })

// the condition is solved once, on its own; the chosen branch continues with the rest
private def ifThenElse(condition: Term, onTrue: Goal, onFalse: Option[Goal], rest: List[Goal], s: Subst, depth: Int)(using Program): Steps =
  ZStream.unwrap(isolated(condition, s, depth).runHead.map {
    case Some(solution) => solve(onTrue :: rest, solution.s, solution.depth)
    case None => onFalse.fold(ZStream.empty)(e => solve(e :: rest, s, depth + 1))
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
