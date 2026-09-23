package mmuschalik.predicate.engine

import mmuschalik.predicate.*

// evaluates a fully resolved arithmetic term; Left holds an error ball
def evalNumeric(term: Term): Either[Term, BigDecimal] =
  term match
    case Num(value) => Right(value)
    case _: Variable => Left(Errors.instantiation)
    case Predicate("-", x :: Nil) => evalNumeric(x).map(-_)
    case Predicate("abs", x :: Nil) => evalNumeric(x).map(_.abs)
    case Predicate("+", l :: r :: Nil) => op(l, r)((a, b) => Right(a + b))
    case Predicate("-", l :: r :: Nil) => op(l, r)((a, b) => Right(a - b))
    case Predicate("*", l :: r :: Nil) => op(l, r)((a, b) => Right(a * b))
    case Predicate("/", l :: r :: Nil) => op(l, r)((a, b) => nonZero(b).map(a / _))
    case Predicate("min", l :: r :: Nil) => op(l, r)((a, b) => Right(a.min(b)))
    case Predicate("max", l :: r :: Nil) => op(l, r)((a, b) => Right(a.max(b)))
    case Predicate("//", l :: r :: Nil) =>
      op(l, r)((a, b) => integers(a, b).flatMap(_ => nonZero(b)).map(a.quot))
    case Predicate("mod", l :: r :: Nil) =>
      // the result takes the sign of the divisor, as in Prolog
      op(l, r)((a, b) => integers(a, b).flatMap(_ => nonZero(b)).map { b =>
        val m = a % b
        if m != 0 && m.signum != b.signum then m + b else m
      })
    case other => Left(Errors.typeError("evaluable", other))

private def op(l: Term, r: Term)(f: (BigDecimal, BigDecimal) => Either[Term, BigDecimal]): Either[Term, BigDecimal] =
  for
    a <- evalNumeric(l)
    b <- evalNumeric(r)
    result <- f(a, b)
  yield result

private def nonZero(b: BigDecimal): Either[Term, BigDecimal] =
  if b == 0 then Left(Errors.evaluation("zero_divisor")) else Right(b)

private def integers(values: BigDecimal*): Either[Term, Unit] =
  values
    .find(!_.isWhole)
    .fold(Right(()))(v => Left(Errors.typeError("integer", Num(v))))
