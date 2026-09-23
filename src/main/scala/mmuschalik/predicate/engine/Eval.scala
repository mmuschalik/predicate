package mmuschalik.predicate.engine

import mmuschalik.predicate.*
import zio.*

def evalNumeric(term: Term): Either[SolveError, BigDecimal] = 
  term match
    case Num(value) => Right(value)
    case Predicate("+", l :: r :: Nil) => 
      op(l, r, _ + _)
    case Predicate("-", l :: r :: Nil) => 
      op(l, r, _ - _)
    case Predicate("*", l :: r :: Nil) => 
      op(l, r, _ * _)
    case Predicate("/", l :: r :: Nil) => 
      op(l, r, _ / _)
    case x => Left(ExpectingNumber(x))

def op(l: Term, r: Term, f: (a: BigDecimal, b: BigDecimal) => BigDecimal) = 
  evalNumeric(l)
    .flatMap(a => evalNumeric(r).map(b => f(a, b)))