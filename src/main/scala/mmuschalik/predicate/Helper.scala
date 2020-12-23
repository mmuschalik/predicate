package mmuschalik.predicate

val A = variable("A")
val B = variable("B")
val C = variable("C")
val X = variable("X")
val Y = variable("Y")
val Z = variable("Z")

def atom[T](t: T): Term = Atom(t)

def variable(name: String): Variable = 
  Variable(name, 0)

def predicate(name: String, terms: Term*): Predicate = 
  Predicate(name, terms.toList)

def query(goals: Goal*): Query = 
  Query(goals.toList)



val cut = predicate("cut")

def not(t: Term) = predicate("not", t)

def call(t: Term) = predicate("call", t)
