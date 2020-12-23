package mmuschalik.predicate

import scala.language.implicitConversions

implicit def fromInt(a: Int): Term = atom(a)

implicit def fromString(a: String): Term = atom(a)

implicit def fromBool(a: Boolean): Predicate = 
  if a then 
    Predicate("true") 
  else 
    Predicate("false")

implicit def fromPredicate(p: Predicate): Clause = Clause(p)

implicit def fromVariable(v: Variable): Predicate = call(v)