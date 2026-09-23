package mmuschalik.predicate.engine

import mmuschalik.predicate.*
import zio.stream.*

// a built-in maps its arguments and the current substitution to zero or more extended substitutions;
// depth is the next free variable version, for built-ins that need fresh variables
private[engine] type Builtin = (List[Term], Subst, Int) => ZStream[Any, Raised, Subst]

private[engine] object Builtins:

  // succeeds at most once; Left is an error ball
  private def det(f: (List[Term], Subst) => Either[Term, Option[Subst]]): Builtin =
    (args, s, depth) =>
      f(args, s).fold(
        ball => ZStream.fail(Raised(ball, depth)),
        _.fold(ZStream.empty)(ZStream.succeed(_)))

  private def test(f: (List[Term], Subst) => Boolean): Builtin =
    det((args, s) => Right(Option.when(f(args, s))(s)))

  private def typeCheck(f: Term => Boolean): Builtin =
    test((args, s) => f(walk(args.head, s)))

  private def compare(f: (BigDecimal, BigDecimal) => Boolean): Builtin =
    det((args, s) =>
      for
        l <- evalNumeric(resolve(args(0), s))
        r <- evalNumeric(resolve(args(1), s))
      yield Option.when(f(l, r))(s))

  private def integer(term: Term, s: Subst): Either[Term, BigDecimal] =
    walk(term, s) match
      case _: Variable => Left(Errors.instantiation)
      case Num(n) if n.isWhole => Right(n)
      case other => Left(Errors.typeError("integer", other))

  private val between: Builtin =
    (args, s, depth) =>
      val bounds =
        for
          low <- integer(args(0), s)
          high <- integer(args(1), s)
        yield (low, high)
      bounds.fold(
        ball => ZStream.fail(Raised(ball, depth)),
        (low, high) =>
          walk(args(2), s) match
            case v: Variable =>
              ZStream.iterate(low)(_ + 1).takeWhile(_ <= high).map(n => s + (v -> Num(n)))
            case Num(n) if n.isWhole =>
              if low <= n && n <= high then ZStream.succeed(s) else ZStream.empty
            case other =>
              ZStream.fail(Raised(Errors.typeError("integer", other), depth)))

  val all: Map[(String, Int), Builtin] = Map(
    ("true", 0) -> test((_, _) => true),
    ("false", 0) -> test((_, _) => false),
    ("fail", 0) -> test((_, _) => false),

    ("=", 2) -> det((args, s) => Right(unify(args(0), args(1), s))),
    ("\\=", 2) -> test((args, s) => unify(args(0), args(1), s).isEmpty),
    ("==", 2) -> test((args, s) => resolve(args(0), s) == resolve(args(1), s)),
    ("\\==", 2) -> test((args, s) => resolve(args(0), s) != resolve(args(1), s)),

    ("is", 2) -> det((args, s) => evalNumeric(resolve(args(1), s)).map(n => unify(args(0), Num(n), s))),
    ("<", 2) -> compare(_ < _),
    (">", 2) -> compare(_ > _),
    ("=<", 2) -> compare(_ <= _),
    (">=", 2) -> compare(_ >= _),
    ("=:=", 2) -> compare(_ == _),
    ("=\\=", 2) -> compare(_ != _),
    ("between", 3) -> between,

    ("var", 1) -> typeCheck(_.isInstanceOf[Variable]),
    ("nonvar", 1) -> typeCheck(!_.isInstanceOf[Variable]),
    ("atom", 1) -> typeCheck { case Atom(_) | Predicate(_, Nil) => true; case _ => false },
    ("number", 1) -> typeCheck(_.isInstanceOf[Num]),
    ("compound", 1) -> typeCheck { case Predicate(_, _ :: _) => true; case _ => false }
  )
