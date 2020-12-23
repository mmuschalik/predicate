package mmuschalik.test

import mmuschalik.predicate._

def f(terms: Term*) = predicate("f", terms :_*)
def g(terms: Term*) = predicate("g", terms :_*)
def h(terms: Term*) = predicate("h", terms :_*)
val a = "a"
val b = "b"
val c = "c"