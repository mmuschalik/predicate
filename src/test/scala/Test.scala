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
    solveTests,
    cutTests,
    streamTests
  )

  val opTests = suite("Test Term Operations")(
    test("resolve follows bindings through variables") {
      assert(resolve(X, Map(X -> Y, Y -> "b")))(equalTo(atom("b")))
    },
    test("resolve substitutes inside nested terms") {
      assert(resolve(f(X), Map(X -> g(Y), Y -> "b")))(equalTo(f(g("b"))))
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

  def nat(t: Term) = predicate("nat", t)
  def s(t: Term) = predicate("s", t)
  def first(t: Term) = predicate("first", t)
  def peano(n: Int): Term = (1 to n).foldLeft(atom("z"): Term)((t, _) => s(t))

  val natProgram =
    Program.build.append(
      nat("z"),
      nat(s(X)) := nat(X)
    )

  def count(t: Term) = predicate("count", t)

  val countProgram =
    Program.build.append(
      count(0) := cut,
      count(X) := (Y is X - 1) && count(Y)
    )

  val firstProgram =
    happyProgram.append(
      first(X) := woman(X) && cut
    )

  val cutTests = suite("Test cut")(
    testProgram("cut inside a clause only prunes that clause's goal")(
      firstProgram,
      first(A),
        Set(jean /A)
    ),
    testProgram("cut inside a clause does not prune goals before it")(
      firstProgram,
      wealthy(B) && first(A),
        Set(fred /B, jean /A),
        Set(pat /B, jean /A)
    ),
    testProgram("cut inside a clause does not prune goals after it")(
      firstProgram,
      first(A) && wealthy(B),
        Set(jean /A, fred /B),
        Set(jean /A, pat /B)
    ),
    testProgram("call is opaque to cut")(
      happyProgram,
      wealthy(A) && call(cut),
        Set(fred /A),
        Set(pat /A)
    ),
    testProgram("not of a goal with several solutions")(
      happyProgram,
      not(woman(A)),
    )
  )

  val streamTests = suite("Test solution stream")(
    test("infinite search is lazy and can be cut short") {
      natProgram
        .solve(nat(A))
        .take(3)
        .runCollect
        .map(r => assert(r.toList)(equalTo(List(
          Set("z" /A),
          Set(s("z") /A),
          Set(s(s("z")) /A)))))
    },
    testProgram("deep search with flat terms")(
      countProgram,
      count(100000),
        Set()
    ),
    testProgram("deeply nested terms")(
      natProgram,
      nat(peano(5000)),
        Set()
    ),
    test("arithmetic on an unbound variable fails the stream") {
      Program.build
        .solve(A is (B + 1))
        .runCollect
        .either
        .map(r => assert(r)(equalTo(Left(ExpectingNumber(B)))))
    }
  )

  def testProgram(msg: String)(program: Program, query: Goal, set: Set[Binding]*): Spec[Any, SolveError] =
    testProgram(msg)(program, Query(List(query)), set*)

  def testProgram(msg: String)(program: Program, query: Query, set: Set[Binding]*): Spec[Any, SolveError] = test(msg) {
    program
      .solve(query)
      .runCollect
      .map(s => assert(s.toSet)(equalTo(set.toSet)))
  }
}
