package mmuschalik.test.foodtest

import Prolog.ADT._


case class Food(name: String)
def food(name: Term): Predicate = predicate("food", name)
given BuildPredicate[Food] {
  def build(a: Food): Predicate = food(atom(a.name))
}

case class Lunch(name: String)
def lunch(name: Term): Predicate = predicate("lunch", name)
given BuildPredicate[Lunch] {
  def build(a: Lunch): Predicate = lunch(atom(a.name))
}

case class Dinner(name: String)
def dinner(name: Term): Predicate = predicate("dinner", name)
given BuildPredicate[Dinner] {
  def build(a: Dinner): Predicate = dinner(atom(a.name))
}

case class Meal(name: String)
def meal(name: Term): Predicate = predicate("meal", name)
given BuildPredicate[Meal] {
  def build(a: Meal): Predicate = dinner(atom(a.name))
}