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
    opTests,
    testM("ensure all basic facts are solutions") {
      for {
        solution      <- solve(query(food(A))).take(maxCount).runCount
      } yield assert(solution)(equalTo(foods.size))
    },
    testM("ensure basic clause can be solved") {
      for {
        solution      <- solve(query(meal(A))).take(maxCount).runCount
      } yield assert(solution)(equalTo(foods.size))
    }//,
    //testM("test query with multiple goals") {
    //  for {
    //    solution      <- solve(query(meal(A), lunch(A))).runHead //.aggregate(ZTransducer.fold((0,0))(_ => true)((a, b) => if b == Set("sandwich" / A) then (a._1+1,a._2) else (a._1, a._2+1))).runHead
    //  } yield assert(solution)(equalTo(Some(Set("sandwich" / Variable("A", 1)))))
    //}
  )

  def f(terms: Term*) = predicate("f", terms :_*)
  def g(terms: Term*) = predicate("g", terms :_*)
  def h(terms: Term*) = predicate("h", terms :_*)
  val a = "a"
  val b = "b"
  val c = "c"

  val opTests = suite("Test Term Operations")(
    test("merge bindings 1") {
      val b1 = Set("a" /X)
      val b2 = Set("b" /Y)
      assert(merge(b1, b2))(equalTo(b1 ++ b2))
    },
    test("merge bindings 2") {
      val b1 = Set(Y /X)
      val b2 = Set("b" /Y)
      assert(merge(b1, b2))(equalTo(Set("b" /X, "b" /Y)))
    },
    test("successfull unification") {
      val t1 = f(g(X, h(X, b)), Z)
      val t2 = f(g(a, Z), Y)
      assert(unify(t1, t2))(equalTo(Some(Set(a /X, h(a, b) /Z, h(a, b) /Y))))
    },
    test("failed unification") {
      val t1 = f(a, Y, b)
      val t2 = f(X, X, Y)
      assert(unify(t1, t2))(equalTo(None))
    }
  )
}

