package mmuschalik.test.happytest

import Prolog.ADT._

val happyProgram = Program
  .build
  .append(Woman(jean) :: Woman(pat) :: Nil)
  .append(Man(fred) :: Nil)
  .append(Wealthy(fred) :: Wealthy(pat) :: Nil)
  .append(Wise(jean) :: Nil)
  .append(happy(X) := woman(X) && wealthy(X))
  .append(happy(X) := woman(X) && wise(X))