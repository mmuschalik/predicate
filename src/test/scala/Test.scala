import zio.console._
import zio.stream._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import mmuschalik.ADT._
import mmuschalik.Operation._
import mmuschalik.test.foodtest._
import mmuschalik.test.happytest._


object TestProlog extends DefaultRunnableSpec {

  def spec = suite("Test All")(
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
    testProgram("ensure all basic facts are solutions")(
      foodProgram,
      food(A), 
        Set(burger /A),
        Set(sandwich /A),
        Set(pizza /A),
    ),
    testProgram("ensure basic clause can be solved")(
      foodProgram,
      meal(A), 
        Set(burger /A),
        Set(sandwich /A),
        Set(pizza /A),
    ), 
    testProgram("test query with multiple goals")(
      foodProgram,
      meal(A) && lunch(A),
        Set(sandwich / A)
    ) ,
    testProgram("test simple conjunction and disjunction")(
      happyProgram,
      happy(A),
        Set(pat /A), 
        Set(jean /A)
    ),
    testProgram("basic cut test 1")(
      happyProgram,
      woman(A) && cut, 
        Set(jean /A),
    ),
    testProgram("basic cut test 2")(
      happyProgram,
      wealthy(A) && cut && man(A), 
        Set(fred /A)
    ),
    testProgram("test false")(
      happyProgram,
      wealthy(A) && false
    ),
    testProgram("basic not")(
      happyProgram,
      wealthy(A) && mmuschalik.ADT.not(man(A)), 
        Set(pat /A)
    )
  )

  def testProgram(msg: String)(program: Program, query: Goal, set: Set[Binding]*) = testM(msg) {
    program
      .solve(query)
      .flatMap(_.runCollect)
      .map(s => assert(s.toSet)(equalTo(set.toSet)))
  }

  def testProgram(msg: String)(program: Program, query: Query, set: Set[Binding]*) = testM(msg) {
    program
      .solve(query)
      .flatMap(_.runCollect)
      .map(s => assert(s.toSet)(equalTo(set.toSet)))
  }
}

