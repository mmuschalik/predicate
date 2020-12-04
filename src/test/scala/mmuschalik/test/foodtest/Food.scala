package mmuschalik.test.foodtest

import Prolog.ADT._

val foods = 
  Food("burger") :: 
  Food("sandwich") ::
  Food("pizza") ::
  Nil

val lunchs = Lunch("sandwich") :: Nil

val foodProgram = Program
  .build
  .append(foods)
  .append(lunchs)
  .append(meal(X) := food(X))

