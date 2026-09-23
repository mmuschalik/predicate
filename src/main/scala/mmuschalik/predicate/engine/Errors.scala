package mmuschalik.predicate.engine

import mmuschalik.predicate.*

sealed trait SolveError
case object InstantiationError extends SolveError
case class TypeError(expected: String, culprit: Term) extends SolveError
case class EvaluationError(reason: String) extends SolveError
case class UncaughtThrow(ball: Term) extends SolveError

// errors travel through the solver as Prolog terms (balls) so catch/3 can unify with them;
// depth is the next free variable version at the point the error was raised
private[engine] case class Raised(ball: Term, depth: Int)

private[engine] object Errors:

  def instantiation: Term =
    Predicate("error", List(Atom("instantiation_error")))

  def typeError(expected: String, culprit: Term): Term =
    Predicate("error", List(Predicate("type_error", List(Atom(expected), culprit))))

  def evaluation(reason: String): Term =
    Predicate("error", List(Predicate("evaluation_error", List(Atom(reason)))))

  def toSolveError(ball: Term): SolveError =
    ball match
      case Predicate("error", Atom("instantiation_error") :: Nil) => InstantiationError
      case Predicate("error", Predicate("type_error", Atom(expected) :: culprit :: Nil) :: Nil) => TypeError(expected, culprit)
      case Predicate("error", Predicate("evaluation_error", Atom(reason) :: Nil) :: Nil) => EvaluationError(reason)
      case other => UncaughtThrow(other)
