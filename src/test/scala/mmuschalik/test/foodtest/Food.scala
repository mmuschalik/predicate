package mmuschalik.test.foodtest

import mmuschalik.predicate._

val burger = "burger"
val sandwich = "sandwich"
val pizza = "pizza"


val foodProgram = Program
  .build
  .appendFacts(
                  food(burger),
                  food(sandwich),
                  food(pizza),
                  lunch(sandwich)
  )
  .append(
                  meal(X) := food(X)
  )


def food(name: Term): Predicate = predicate("food", name)
def lunch(name: Term): Predicate = predicate("lunch", name)
def dinner(name: Term): Predicate = predicate("dinner", name)
def meal(name: Term): Predicate = predicate("meal", name)