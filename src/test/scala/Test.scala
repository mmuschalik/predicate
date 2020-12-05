import zio.console._
import zio.stream._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import Prolog.ADT._
import Prolog._
import Prolog.Operation._
import mmuschalik.test.foodtest._
import mmuschalik.test.happytest._


object TestProlog extends DefaultRunnableSpec {
  val maxCount = 100

  def spec = suite("Test Prolog")(
    opTests,
    solveTests
  )

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
    },
    test("substitutions") {
      val t = f(g(X, h(X, b)), Z)
      val sub = Set(a /X, h(a, b) /Z)
      assert(t.substitute(sub))(equalTo(f(g(a, h(a, b)), h(a, b))))
    }
  )

  val solveTests = suite("Test solving goals")(
    testM("ensure all basic facts are solutions") {
      foodProgram
        .solve(food(A))
        .take(maxCount)
        .runCount
        .map(solution => assert(solution)(equalTo(3)))
    },
    testM("ensure basic clause can be solved") {
      foodProgram
        .solve(meal(A))
        .runCount
        .map(solution => assert(solution)(equalTo(3)))
    },
    testM("test query with multiple goals") {
      foodProgram
        .solve(meal(A) && lunch(A))
        .runHead
        .map(solution => assert(solution)(equalTo(Some(
          Set(sandwich / A)
        ))))
    },
    testM("test simple conjunction and disjunction") {
      happyProgram
        .solve(happy(A))
        .runCollect
        .map(solution => assert(solution.toSet)(equalTo(Set(
          Set(pat /A), 
          Set(jean /A)
        ))))
    }

  )
}

