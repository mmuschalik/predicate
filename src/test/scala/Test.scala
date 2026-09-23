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
    streamTests,
    builtinTests,
    listTests,
    controlTests
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
    test("numbers unify by value regardless of Scala type") {
      assert(unify(f(2), f(2.0)))(equalTo(Some(Set())))
    },
    test("an atom never unifies with a number") {
      assert(unify(f("1"), f(1)))(equalTo(None))
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
    testProgram("numeric literals of every Scala type convert")(
      Program.build,
      (A is 1 + 2L) && (B is A * 1.5) && (C is B - 0.5f) && (X is BigDecimal(1) + C) && (Y is 1 + X) && (Z is 2.5 * Y),
        Set(3 /A, 4.5 /B, 4 /C, 5 /X, 6 /Y, 15 /Z)
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
  )

  val countdownProgram =
    Program.build.append(
      count(0),
      count(X) := (X > 0) && (Y is X - 1) && count(Y)
    )

  val builtinTests = suite("Test built-ins")(
    testProgram("arithmetic comparisons")(
      Program.build,
      (X is 3) && (X > 2) && (X < 4) && (X <= 3) && (X >= 3) && (X =:= 3.0) && (X =\= 4),
        Set(3 /X)
    ),
    testProgram("a false comparison fails")(
      Program.build,
      num(3) > 4
    ),
    testProgram("comparisons evaluate both sides")(
      Program.build,
      (X is 2) && (X * 2 =:= X + 2),
        Set(2 /X)
    ),
    testProgram("recursion guarded by a comparison instead of a cut")(
      countdownProgram,
      count(3),
        Set()
    ),
    testProgram("integer arithmetic follows Prolog rounding")(
      Program.build,
      (A is num(-7) % 2) && (B is intDiv(-7, 2)) && (C is abs(-3)) && (X is min(2, 5)) && (Y is max(2, 5)) && (Z is -num(4)),
        Set(1 /A, -3 /B, 3 /C, 2 /X, 5 /Y, -4 /Z)
    ),
    testProgram("between enumerates integers")(
      Program.build,
      between(1, 3, A),
        Set(1 /A),
        Set(2 /A),
        Set(3 /A)
    ),
    testProgram("between checks a bound value")(
      Program.build,
      between(1, 3, 5)
    ),
    testProgram("not unifiable")(
      Program.build,
      (atom("a") !=* atom("b")) && (X !=* 1),
    ),
    testProgram("not unifiable succeeds for different terms")(
      Program.build,
      atom("a") !=* atom("b"),
        Set()
    ),
    testProgram("identity does not bind variables")(
      Program.build,
      (X === X) && (X =!= Y) && (X =* 1) && (X === 1),
        Set(1 /X)
    ),
    testProgram("identity fails for distinct unbound variables")(
      Program.build,
      X === Y
    ),
    testProgram("type checks")(
      Program.build,
      isVar(X) && isNumber(1) && isAtom("a") && isAtom(predicate("foo")) && isCompound(f(a)) && (X =* 1) && isNonVar(X),
        Set(1 /X)
    ),
    testProgram("type checks fail on the wrong kind of term")(
      Program.build,
      isCompound("a")
    ),
    testError("arithmetic on an unbound variable")(
      A is (B + 1),
      InstantiationError
    ),
    testError("arithmetic on an atom")(
      A is (atom("foo") + 1),
      TypeError("evaluable", atom("foo"))
    ),
    testError("division by zero")(
      A is (1 / num(0)),
      EvaluationError("zero_divisor")
    ),
    testError("mod needs integers")(
      A is (num(1.5) % 1),
      TypeError("integer", 1.5)
    ),
    testError("between needs bound limits")(
      between(1, A, 2),
      InstantiationError
    )
  )

  val listTests = suite("Test lists")(
    test("lists display in Prolog notation") {
      assert((list(1, 2).show, (X :: Y).show, list().show))(equalTo(("[1, 2]", "[X|Y]", "[]")))
    },
    testProgram("destructure a list")(
      Program.build,
      list(1, 2, 3) =* (X :: Y),
        Set(1 /X, list(2, 3) /Y)
    ),
    testProgram("append two lists")(
      Program.build,
      append(list(1, 2), list(3), A),
        Set(list(1, 2, 3) /A)
    ),
    testProgram("append splits a list every way")(
      Program.build,
      append(A, B, list(1, 2)),
        Set(nil /A, list(1, 2) /B),
        Set(list(1) /A, list(2) /B),
        Set(list(1, 2) /A, nil /B)
    ),
    testProgram("member enumerates elements")(
      Program.build,
      member(A, list("a", "b", "c")),
        Set(atom("a") /A),
        Set(atom("b") /A),
        Set(atom("c") /A)
    ),
    testProgram("member checks an element")(
      Program.build,
      member("b", list("a", "b", "c")),
        Set()
    ),
    testProgram("reverse")(
      Program.build,
      reverse(list(1, 2, 3), A),
        Set(list(3, 2, 1) /A)
    ),
    testProgram("nth0, nth1 and last")(
      Program.build,
      nth0(1, list("a", "b", "c"), A) && nth1(1, list("a", "b", "c"), B) && nth0(C, list("a", "b"), "b") && last(list(1, 2, 3), X),
        Set(atom("b") /A, atom("a") /B, 1 /C, 3 /X)
    ),
    testProgram("sum of a list")(
      Program.build,
      sumList(list(1, 2, 3), A),
        Set(6 /A)
    ),
    testProgram("length of a list")(
      Program.build,
      length(list("a", "b"), A),
        Set(2 /A)
    ),
    testProgram("length builds a list of fresh variables")(
      Program.build,
      length(A, 2) && (A =* list(1, 2)),
        Set(list(1, 2) /A)
    ),
    test("length enumerates lists when both sides are unbound") {
      Program.build
        .solve(length(A, B))
        .map(_.find(_.variable == B).map(_.term))
        .take(3)
        .runCollect
        .map(r => assert(r.toList)(equalTo(List(Some(num(0)), Some(num(1)), Some(num(2))))))
    },
    test("long lists in answers") {
      Program.build
        .solve(reverse(list((1 to 10000).map(num(_))*), A) && nth1(1, A, B))
        .map(_.find(_.variable == B).map(_.term))
        .runCollect
        .map(r => assert(r.toList)(equalTo(List(Some(num(10000))))))
    },
    testProgram("long lists")(
      Program.build,
      length(list((1 to 10000).map(num(_))*), A) && sumList(list((1 to 10000).map(num(_))*), B),
        Set(10000 /A, 50005000 /B)
    )
  )

  def person(t: Term) = predicate("person", t)
  def firstPerson(t: Term) = predicate("first_person", t)
  def sign(n: Term, t: Term) = predicate("sign", n, t)
  def chosen(t: Term) = predicate("chosen", t)
  def localCut(t: Term) = predicate("local_cut", t)

  val controlProgram =
    happyProgram.append(
      person(X) := woman(X) || man(X),
      firstPerson(X) := (woman(X) && cut) || man(X),
      sign(X, Y) := ifThenElse(X > 0, Y =* "pos", ifThenElse(X < 0, Y =* "neg", Y =* "zero")),
      chosen(X) := ifThenElse(true, woman(X) && cut, fail),
      chosen("other"),
      localCut(X) := ifThenElse(cut, woman(X), fail),
      localCut("other")
    )

  val controlTests = suite("Test control flow")(
    testProgram("disjunction in a query")(
      happyProgram,
      woman(A) || man(A),
        Set(jean /A),
        Set(pat /A),
        Set(fred /A)
    ),
    testProgram("disjunction in a clause")(
      controlProgram,
      person(A),
        Set(jean /A),
        Set(pat /A),
        Set(fred /A)
    ),
    testProgram("a cut in one branch prunes the other branch and the clause")(
      controlProgram,
      firstPerson(A),
        Set(jean /A)
    ),
    testProgram("if-then-else takes the first solution of the condition")(
      happyProgram,
      ifThenElse(woman(A), B =* "yes", B =* "no"),
        Set(jean /A, atom("yes") /B)
    ),
    testProgram("if-then-else runs the else branch when the condition fails")(
      happyProgram,
      ifThenElse(man("jean"), B =* 1, B =* 2),
        Set(2 /B)
    ),
    testProgram("if-then without else fails when the condition fails")(
      happyProgram,
      ifThen(man("jean"), true)
    ),
    testProgram("nested if-then-else")(
      controlProgram,
      sign(5, A) && sign(-1, B) && sign(0, C),
        Set(atom("pos") /A, atom("neg") /B, atom("zero") /C)
    ),
    testProgram("a cut in a branch belongs to the clause")(
      controlProgram,
      chosen(A),
        Set(jean /A)
    ),
    testProgram("a cut in the condition stays local")(
      controlProgram,
      localCut(A),
        Set(jean /A),
        Set(pat /A),
        Set(atom("other") /A)
    ),
    testProgram("not does not bind variables")(
      happyProgram,
      not(man(A) && woman(A)) && man(A),
        Set(fred /A)
    ),
    testProgram("catch a thrown term")(
      Program.build,
      catching(raise("oops"), X, Y =* 1),
        Set(atom("oops") /X, 1 /Y)
    ),
    testProgram("catch a built-in error")(
      Program.build,
      catching(A is B + 1, error(C), true),
        Set(atom("instantiation_error") /C)
    ),
    testProgram("bindings made before the error are undone")(
      Program.build,
      catching((A =* 1) && raise("e"), "e", true),
        Set()
    ),
    testProgram("solutions before the error are kept")(
      Program.build,
      catching(member(A, list(1, 2)) && ifThenElse(A > 1, raise("big"), true), "big", A =* 0),
        Set(1 /A),
        Set(0 /A)
    ),
    testError("an uncaught throw")(
      raise("oops"),
      UncaughtThrow(atom("oops"))
    ),
    testError("a catcher that does not match rethrows")(
      catching(raise("a"), "b", true),
      UncaughtThrow(atom("a"))
    ),
    testError("errors in the continuation are not caught")(
      catching(true, X, true) && raise("late"),
      UncaughtThrow(atom("late"))
    ),
    testError("calling an unbound variable")(
      call(A),
      InstantiationError
    )
  )

  def testError(msg: String)(query: Goal, error: SolveError): Spec[Any, Nothing] = test(msg) {
    Program.build
      .solve(query)
      .runCollect
      .either
      .map(r => assert(r)(equalTo(Left(error))))
  }

  def testProgram(msg: String)(program: Program, query: Goal, set: Set[Binding]*): Spec[Any, SolveError] =
    testProgram(msg)(program, Query(List(query)), set*)

  def testProgram(msg: String)(program: Program, query: Query, set: Set[Binding]*): Spec[Any, SolveError] = test(msg) {
    program
      .solve(query)
      .runCollect
      .map(s => assert(s.toSet)(equalTo(set.toSet)))
  }
}
