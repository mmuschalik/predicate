package mmuschalik.predicate

import scala.annotation.tailrec

case class DecodeError(message: String)

// converts a term in an answer into a Scala value
trait Decoder[T]:

  def decode(term: Term): Either[DecodeError, T]

object Decoder:

  private def fail(expected: String, term: Term): Left[DecodeError, Nothing] = 
    Left(DecodeError("expected " + expected + ", got " + term.show))

  given Decoder[Term] with
    def decode(term: Term) = Right(term)

  given Decoder[BigDecimal] with
    def decode(term: Term) = 
      term match
        case Num(n) => Right(n)
        case other => fail("a number", other)

  given Decoder[Int] with
    def decode(term: Term) = 
      term match
        case Num(n) if n.isValidInt => Right(n.toInt)
        case other => fail("an Int", other)

  given Decoder[Long] with
    def decode(term: Term) = 
      term match
        case Num(n) if n.isValidLong => Right(n.toLong)
        case other => fail("a Long", other)

  given Decoder[Double] with
    def decode(term: Term) = 
      term match
        case Num(n) => Right(n.toDouble)
        case other => fail("a Double", other)

  given Decoder[String] with
    def decode(term: Term) = 
      term match
        case Atom(name) => Right(name)
        case Predicate(name, Nil) => Right(name)
        case other => fail("an atom", other)

  given Decoder[Boolean] with
    def decode(term: Term) = 
      term match
        case Atom("true") | Predicate("true", Nil) => Right(true)
        case Atom("false") | Predicate("false", Nil) => Right(false)
        case other => fail("true or false", other)

  given [T](using item: Decoder[T]): Decoder[List[T]] with
    def decode(term: Term) = 
      @tailrec
      def loop(t: Term, acc: List[T]): Either[DecodeError, List[T]] = 
        t match
          case Atom("[]") => Right(acc.reverse)
          case Predicate(".", head :: tail :: Nil) =>
            item.decode(head) match
              case Right(value) => loop(tail, value :: acc)
              case Left(error) => Left(error)
          case _ => fail("a list", term)
      loop(term, Nil)
