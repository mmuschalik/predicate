import zio._
import zio.console._
import zio.stream._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import Prolog.ADT._
import Prolog._


object TestProlog extends DefaultRunnableSpec {

  val foods = 
    Food("burger") :: 
    Food("sandwich") ::
    Food("pizza") ::
    Nil

  val lunchs = Lunch("sandwich") :: Nil

  given program as Program = Program
    .build
    .append(foods)
    .append(lunchs)
    .append(meal(X) := food(X))

  val maxCount = 100

  def spec = suite("Test Prolog")(
    testM("ensure all basic facts are solutions") {
      for {
        solution      <- solve(query(food(A))).take(maxCount).runCount
      } yield assert(solution)(equalTo(foods.size))
    },
    testM("ensure basic clause can be solved") {
      for {
        solution      <- solve(query(meal(A))).take(maxCount).runCount
      } yield assert(solution)(equalTo(foods.size))
    },
    testM("test query with multiple goals") {
      for {
        solution      <- solve(query(meal(A), lunch(A))).runHead //.aggregate(ZTransducer.fold((0,0))(_ => true)((a, b) => if b == Set("sandwich" / A) then (a._1+1,a._2) else (a._1, a._2+1))).runHead
      } yield assert(solution)(equalTo(Some(Set("sandwich" / Variable("A", 1)))))
    }
  )
}

