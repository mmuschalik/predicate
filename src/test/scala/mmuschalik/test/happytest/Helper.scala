package mmuschalik.test.happytest

import Prolog.ADT._

case class Woman(name: String)
def woman(name: Term): Predicate = predicate("woman", name)
given BuildPredicate[Woman] {
  def build(a: Woman): Predicate = woman(atom(a.name))
}

case class Man(name: String)
def man(name: Term): Predicate = predicate("man", name)
given BuildPredicate[Man] {
  def build(a: Man): Predicate = man(atom(a.name))
}

case class Wealthy(name: String)
def wealthy(name: Term): Predicate = predicate("wealthy", name)
given BuildPredicate[Wealthy] {
  def build(a: Wealthy): Predicate = wealthy(atom(a.name))
}

case class Wise(name: String)
def wise(name: Term): Predicate = predicate("wise", name)
given BuildPredicate[Wise] {
  def build(a: Wise): Predicate = wise(atom(a.name))
}

def happy(name: Term): Predicate = predicate("happy", name)

val jean = "jean"
val fred = "fred"
val pat = "pat"