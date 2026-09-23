package mmuschalik.test

import zio.test.*
import zio.test.Assertion.equalTo
import mmuschalik.predicate.*
import mmuschalik.predicate.engine.*
import mmuschalik.test.foodtest.*
import mmuschalik.test.happytest.*


object TestProlog extends ZIOSpecDefault {

  def spec = suite("Test All")(
    opTests,
    algebraTests,
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
    },
    test("merge substitutes into nested terms") {
      assert(merge(Set(f(Y) /X), Set("b" /Y)))(equalTo(Set(f("b") /X, "b" /Y)))
    },
    test("unification resolves bindings inside nested terms") {
      assert(unify(f(X, Y), f(g(Y), a)))(equalTo(Some(Set(g(a) /X, a /Y))))
    },
    test("occurs check distinguishes renamed variables") {
      val x1 = Variable("_X", 1)
      val x2 = Variable("_X", 2)
      assert(unify(x1, f(x2)))(equalTo(Some(Set(f(x2) /x1))))
    },
    test("occurs check rejects cyclic terms") {
      assert(unify(X, f(X)))(equalTo(None))
    }
  )

  val algebraTests = suite("Test simple algebra")(
    testProgram("addition")(
      Program.build,
      (A is 1) && (B is 1) && (Z is (A + B)), 
        Set(1 /A, 1 /B, 2 /Z)
    ),
    testProgram("is with an already bound left side")(
      Program.build,
      (A is 1) && (A is 1),
        Set(1 /A)
    ),
    testProgram("is fails when a bound left side differs")(
      Program.build,
      (A is 1) && (A is 2)
    ),
    testProgram("is with a number on the left side")(
      Program.build,
      is(3, plus(1, 2)),
        Set()
    ),
    testProgram("is updates bindings that point at its variable")(
      Program.build,
      (A =* B) && (B is 1),
        Set(1 /A, 1 /B)
    )
  )

  val solveTests = suite("Test solving goals")(
    testProgram("simple equal (unify)")(
      Program.build,
      A =* 0, 
        Set(0 /A)
    ),
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
    testProgram("true succeeds")(
      Program.build,
      (A =* 1) && true,
        Set(1 /A)
    ),
    testProgram("bindings stay consistent through nested terms")(
      Program.build,
      (f(X, Y) =* f(g(Y), a)) && (X =* g(Z)),
        Set(g(a) /X, a /Y, a /Z)
    ),
    testProgram("appending a list of facts keeps existing clauses")(
      {
        given BuildPredicate[String] with
          def build(name: String) = woman(name)
        Program.build.append(woman(jean)).append(List(pat))
      },
      woman(A),
        Set(jean /A),
        Set(pat /A)
    ),
    testProgram("basic not")(
      happyProgram,
      wealthy(A) && not(man(A)), 
        Set(pat /A)
    )
  )

  def testProgram(msg: String)(program: Program, query: Goal, set: Set[Binding]*) = test(msg) {
    program
      .solve(query)
      .flatMap(_.runCollect)
      .map(s => assert(s.toSet)(equalTo(set.toSet)))
  }

  def testProgram(msg: String)(program: Program, query: Query, set: Set[Binding]*) = test(msg) {
    program
      .solve(query)
      .flatMap(_.runCollect)
      .map(s => assert(s.toSet)(equalTo(set.toSet)))
  }
}

