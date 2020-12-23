package mmuschalik.test.happytest

import mmuschalik.predicate._

val jean = "jean"
val fred = "fred"
val pat = "pat"

val happyProgram = 
  Program
    .build
    .append(
              woman(jean),
              woman(pat),
              man(fred),
              wealthy(fred),
              wealthy(pat),
              wise(jean),

              happy(X) := woman(X) && wealthy(X),
              happy(X) := woman(X) && wise(X)
    )


def woman(name: Term) = predicate("woman", name)
def man(name: Term) = predicate("man", name)
def wealthy(name: Term) = predicate("wealthy", name)
def wise(name: Term) = predicate("wise", name)
def happy(name: Term) = predicate("happy", name)