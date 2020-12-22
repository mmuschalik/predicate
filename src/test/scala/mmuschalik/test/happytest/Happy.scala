package mmuschalik.test.happytest

import mmuschalik.ADT._

val jean = "jean"
val fred = "fred"
val pat = "pat"

val happyProgram = Program
  .build
  .appendFacts(
                  woman(jean),
                  woman(pat),
                  man(fred),
                  wealthy(fred),
                  wealthy(pat),
                  wise(jean)
  )
  .append(
                  happy(X) := woman(X) && wealthy(X),
                  happy(X) := woman(X) && wise(X)
  )


// --Helpers

def woman(name: Term) = predicate("woman", name)
def man(name: Term) = predicate("man", name)
def wealthy(name: Term) = predicate("wealthy", name)
def wise(name: Term) = predicate("wise", name)
def happy(name: Term) = predicate("happy", name)