package mmuschalik.predicate

val A = variable("A")
val B = variable("B")
val C = variable("C")
val X = variable("X")
val Y = variable("Y")
val Z = variable("Z")

def atom(name: String): Atom = Atom(name)

def num(value: BigDecimal): Num = Num(value)

def variable(name: String): Variable = 
  Variable(name, 0)

def predicate(name: String, terms: Term*): Predicate = 
  Predicate(name, terms.toList)

def query(goals: Goal*): Query = 
  Query(goals.toList)



val cut = predicate("cut")

def not(t: Term) = predicate("not", t)

def call(t: Term) = predicate("call", t)

def eql(l: Term, r: Term) = predicate("=", l, r)

def is(l: Term, r: Term) = predicate("is", l, r)

def plus(l: Term, r: Term) = predicate("+", l, r)

def minus(l: Term, r: Term) = predicate("-", l, r)

def multiply(l: Term, r: Term) = predicate("*", l, r)

def divide(l: Term, r: Term) = predicate("/", l, r)

def mod(l: Term, r: Term) = predicate("mod", l, r)

def intDiv(l: Term, r: Term) = predicate("//", l, r)

def abs(t: Term) = predicate("abs", t)

def min(l: Term, r: Term) = predicate("min", l, r)

def max(l: Term, r: Term) = predicate("max", l, r)

val fail = predicate("fail")

def between(low: Term, high: Term, t: Term) = predicate("between", low, high, t)

def isVar(t: Term) = predicate("var", t)

def isNonVar(t: Term) = predicate("nonvar", t)

def isAtom(t: Term) = predicate("atom", t)

def isNumber(t: Term) = predicate("number", t)

def isCompound(t: Term) = predicate("compound", t)
