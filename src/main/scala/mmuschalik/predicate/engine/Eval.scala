package mmuschalik.predicate.engine

import mmuschalik.predicate._
import zio._

trait NumericError
case class NotRecognised(t: Term) extends NumericError

def evalNumeric(term: Term): Either[NumericError, BigDecimal] = 
  term match
    case Atom(a: Int) => Right(BigDecimal(a))
    case Atom(a: Long) => Right(BigDecimal(a))
    case Atom(a: Float) => Right(BigDecimal(a))
    case Atom(a: Double) => Right(BigDecimal(a))
    case Atom(a: BigDecimal) => Right(a)
    case Predicate("+", l :: r :: Nil) => 
      op(l, r, _ + _)
    case Predicate("-", l :: r :: Nil) => 
      op(l, r, _ - _)
    case Predicate("*", l :: r :: Nil) => 
      op(l, r, _ * _)
    case Predicate("/", l :: r :: Nil) => 
      op(l, r, _ / _)
    case x => Left(NotRecognised(x))

def op(l: Term, r: Term, f: (a: BigDecimal, b: BigDecimal) => BigDecimal) = 
  evalNumeric(l)
    .flatMap(a => evalNumeric(r).map(b => f(a, b)))