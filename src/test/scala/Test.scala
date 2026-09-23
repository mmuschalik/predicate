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
    controlTests,
    apiTests
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
      assert(unify(t1, t2))(equalTo(Some(Answer(X -> a, Z -> h(a, b), Y -> h(a, b)))))
    },
    test("failed unification") {
      val t1 = f(a, Y, b)
      val t2 = f(X, X, Y)
      assert(unify(t1, t2))(equalTo(None))
    },
    test("unification resolves bindings inside nested terms") {
      assert(unify(f(X, Y), f(g(Y), a)))(equalTo(Some(Answer(X -> g(a), Y -> a))))
    },
    test("occurs check distinguishes renamed variables") {
      val x1 = Variable("_X", 1)
      val x2 = Variable("_X", 2)
      assert(unify(x1, f(x2)))(equalTo(Some(Answer(x1 -> f(x2)))))
    },
    test("numbers unify by value regardless of Scala type") {
      assert(unify(f(2), f(2.0)))(equalTo(Some(Answer())))
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
        Answer(A -> 1, B -> 1, Z -> 2)
    ),
    testProgram("numeric literals of every Scala type convert")(
      Program.build,
      (A is 1 + 2L) && (B is A * 1.5) && (C is B - 0.5f) && (X is BigDecimal(1) + C) && (Y is 1 + X) && (Z is 2.5 * Y),
        Answer(A -> 3, B -> 4.5, C -> 4, X -> 5, Y -> 6, Z -> 15)
    ),
    testProgram("is with an already bound left side")(
      Program.build,
      (A is 1) && (A is 1),
        Answer(A -> 1)
    ),
    testProgram("is fails when a bound left side differs")(
      Program.build,
      (A is 1) && (A is 2)
    ),
    testProgram("is with a number on the left side")(
      Program.build,
      is(3, plus(1, 2)),
        Answer()
    ),
    testProgram("is updates bindings that point at its variable")(
      Program.build,
      (A =* B) && (B is 1),
        Answer(A -> 1, B -> 1)
    )
  )

  val solveTests = suite("Test solving goals")(
    testProgram("simple equal (unify)")(
      Program.build,
      A =* 0, 
        Answer(A -> 0)
    ),
    testProgram("ensure all basic facts are solutions")(
      foodProgram,
      food(A), 
        Answer(A -> burger),
        Answer(A -> sandwich),
        Answer(A -> pizza),
    ),
    testProgram("ensure basic clause can be solved")(
      foodProgram,
      meal(A), 
        Answer(A -> burger),
        Answer(A -> sandwich),
        Answer(A -> pizza),
    ), 
    testProgram("test query with multiple goals")(
      foodProgram,
      meal(A) && lunch(A),
        Answer(A -> sandwich)
    ) ,
    testProgram("test simple conjunction and disjunction")(
      happyProgram,
      happy(A),
        Answer(A -> pat), 
        Answer(A -> jean)
    ),
    testProgram("basic cut test 1")(
      happyProgram,
      woman(A) && cut, 
        Answer(A -> jean),
    ),
    testProgram("basic cut test 2")(
      happyProgram,
      wealthy(A) && cut && man(A), 
        Answer(A -> fred)
    ),
    testProgram("test false")(
      happyProgram,
      wealthy(A) && false
    ),
    testProgram("true succeeds")(
      Program.build,
      (A =* 1) && true,
        Answer(A -> 1)
    ),
    testProgram("bindings stay consistent through nested terms")(
      Program.build,
      (f(X, Y) =* f(g(Y), a)) && (X =* g(Z)),
        Answer(X -> g(a), Y -> a, Z -> a)
    ),
    testProgram("appending a list of facts keeps existing clauses")(
      {
        given BuildPredicate[String] with
          def build(name: String) = woman(name)
        Program.build.append(woman(jean)).append(List(pat))
      },
      woman(A),
        Answer(A -> jean),
        Answer(A -> pat)
    ),
    testProgram("basic not")(
      happyProgram,
      wealthy(A) && not(man(A)), 
        Answer(A -> pat)
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
        Answer(A -> jean)
    ),
    testProgram("cut inside a clause does not prune goals before it")(
      firstProgram,
      wealthy(B) && first(A),
        Answer(B -> fred, A -> jean),
        Answer(B -> pat, A -> jean)
    ),
    testProgram("cut inside a clause does not prune goals after it")(
      firstProgram,
      first(A) && wealthy(B),
        Answer(A -> jean, B -> fred),
        Answer(A -> jean, B -> pat)
    ),
    testProgram("call is opaque to cut")(
      happyProgram,
      wealthy(A) && call(cut),
        Answer(A -> fred),
        Answer(A -> pat)
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
          Answer(A -> "z"),
          Answer(A -> s("z")),
          Answer(A -> s(s("z")))))))
    },
    testProgram("deep search with flat terms")(
      countProgram,
      count(100000),
        Answer()
    ),
    testProgram("deeply nested terms")(
      natProgram,
      nat(peano(5000)),
        Answer()
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
        Answer(X -> 3)
    ),
    testProgram("a false comparison fails")(
      Program.build,
      num(3) > 4
    ),
    testProgram("comparisons evaluate both sides")(
      Program.build,
      (X is 2) && (X * 2 =:= X + 2),
        Answer(X -> 2)
    ),
    testProgram("recursion guarded by a comparison instead of a cut")(
      countdownProgram,
      count(3),
        Answer()
    ),
    testProgram("integer arithmetic follows Prolog rounding")(
      Program.build,
      (A is num(-7) % 2) && (B is intDiv(-7, 2)) && (C is abs(-3)) && (X is min(2, 5)) && (Y is max(2, 5)) && (Z is -num(4)),
        Answer(A -> 1, B -> -3, C -> 3, X -> 2, Y -> 5, Z -> -4)
    ),
    testProgram("between enumerates integers")(
      Program.build,
      between(1, 3, A),
        Answer(A -> 1),
        Answer(A -> 2),
        Answer(A -> 3)
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
        Answer()
    ),
    testProgram("identity does not bind variables")(
      Program.build,
      (X === X) && (X =!= Y) && (X =* 1) && (X === 1),
        Answer(X -> 1)
    ),
    testProgram("identity fails for distinct unbound variables")(
      Program.build,
      X === Y
    ),
    testProgram("type checks")(
      Program.build,
      isVar(X) && isNumber(1) && isAtom("a") && isAtom(predicate("foo")) && isCompound(f(a)) && (X =* 1) && isNonVar(X),
        Answer(X -> 1)
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
        Answer(X -> 1, Y -> list(2, 3))
    ),
    testProgram("append two lists")(
      Program.build,
      append(list(1, 2), list(3), A),
        Answer(A -> list(1, 2, 3))
    ),
    testProgram("append splits a list every way")(
      Program.build,
      append(A, B, list(1, 2)),
        Answer(A -> nil, B -> list(1, 2)),
        Answer(A -> list(1), B -> list(2)),
        Answer(A -> list(1, 2), B -> nil)
    ),
    testProgram("member enumerates elements")(
      Program.build,
      member(A, list("a", "b", "c")),
        Answer(A -> atom("a")),
        Answer(A -> atom("b")),
        Answer(A -> atom("c"))
    ),
    testProgram("member checks an element")(
      Program.build,
      member("b", list("a", "b", "c")),
        Answer()
    ),
    testProgram("reverse")(
      Program.build,
      reverse(list(1, 2, 3), A),
        Answer(A -> list(3, 2, 1))
    ),
    testProgram("nth0, nth1 and last")(
      Program.build,
      nth0(1, list("a", "b", "c"), A) && nth1(1, list("a", "b", "c"), B) && nth0(C, list("a", "b"), "b") && last(list(1, 2, 3), X),
        Answer(A -> atom("b"), B -> atom("a"), C -> 1, X -> 3)
    ),
    testProgram("sum of a list")(
      Program.build,
      sumList(list(1, 2, 3), A),
        Answer(A -> 6)
    ),
    testProgram("length of a list")(
      Program.build,
      length(list("a", "b"), A),
        Answer(A -> 2)
    ),
    testProgram("length builds a list of fresh variables")(
      Program.build,
      length(A, 2) && (A =* list(1, 2)),
        Answer(A -> list(1, 2))
    ),
    test("length enumerates lists when both sides are unbound") {
      Program.build
        .solve(length(A, B))
        .map(_.get(B))
        .take(3)
        .runCollect
        .map(r => assert(r.toList)(equalTo(List(Some(num(0)), Some(num(1)), Some(num(2))))))
    },
    test("long lists in answers") {
      Program.build
        .solve(reverse(list((1 to 10000).map(num(_))*), A) && nth1(1, A, B))
        .map(_.get(B))
        .runCollect
        .map(r => assert(r.toList)(equalTo(List(Some(num(10000))))))
    },
    testProgram("long lists")(
      Program.build,
      length(list((1 to 10000).map(num(_))*), A) && sumList(list((1 to 10000).map(num(_))*), B),
        Answer(A -> 10000, B -> 50005000)
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
        Answer(A -> jean),
        Answer(A -> pat),
        Answer(A -> fred)
    ),
    testProgram("disjunction in a clause")(
      controlProgram,
      person(A),
        Answer(A -> jean),
        Answer(A -> pat),
        Answer(A -> fred)
    ),
    testProgram("a cut in one branch prunes the other branch and the clause")(
      controlProgram,
      firstPerson(A),
        Answer(A -> jean)
    ),
    testProgram("if-then-else takes the first solution of the condition")(
      happyProgram,
      ifThenElse(woman(A), B =* "yes", B =* "no"),
        Answer(A -> jean, B -> atom("yes"))
    ),
    testProgram("if-then-else runs the else branch when the condition fails")(
      happyProgram,
      ifThenElse(man("jean"), B =* 1, B =* 2),
        Answer(B -> 2)
    ),
    testProgram("if-then without else fails when the condition fails")(
      happyProgram,
      ifThen(man("jean"), true)
    ),
    testProgram("nested if-then-else")(
      controlProgram,
      sign(5, A) && sign(-1, B) && sign(0, C),
        Answer(A -> atom("pos"), B -> atom("neg"), C -> atom("zero"))
    ),
    testProgram("a cut in a branch belongs to the clause")(
      controlProgram,
      chosen(A),
        Answer(A -> jean)
    ),
    testProgram("a cut in the condition stays local")(
      controlProgram,
      localCut(A),
        Answer(A -> jean),
        Answer(A -> pat),
        Answer(A -> atom("other"))
    ),
    testProgram("not does not bind variables")(
      happyProgram,
      not(man(A) && woman(A)) && man(A),
        Answer(A -> fred)
    ),
    testProgram("catch a thrown term")(
      Program.build,
      catching(raise("oops"), X, Y =* 1),
        Answer(X -> atom("oops"), Y -> 1)
    ),
    testProgram("catch a built-in error")(
      Program.build,
      catching(A is B + 1, error(C), true),
        Answer(C -> atom("instantiation_error"))
    ),
    testProgram("bindings made before the error are undone")(
      Program.build,
      catching((A =* 1) && raise("e"), "e", true),
        Answer()
    ),
    testProgram("solutions before the error are kept")(
      Program.build,
      catching(member(A, list(1, 2)) && ifThenElse(A > 1, raise("big"), true), "big", A =* 0),
        Answer(A -> 1),
        Answer(A -> 0)
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

  val parent = Functor("parent")
  val grandparent = Functor("grandparent")

  val familyProgram =
    Program.build.append(
      parent("tom", "bob"),
      parent("bob", "ann"),
      parent("bob", "pat"),
      grandparent(X, Z) := parent(X, Y) && parent(Y, Z)
    )

  val apiTests = suite("Test answer API")(
    testProgram("predicates declared with Functor")(
      familyProgram,
      grandparent("tom", A),
        Answer(A -> "ann"),
        Answer(A -> "pat")
    ),
    testProgram("division by a variable")(
      Program.build,
      (A is 6) && (B is 2) && (C is A / B),
        Answer(A -> 6, B -> 2, C -> 3)
    ),
    test("look up and decode answers") {
      Program.build
        .solve((A is 6) && (B =* "bob") && (C =* list(1, 2, 3)) && (X =* true))
        .runCollect
        .map { r =>
          val answer = r.head
          assertTrue(
            answer(A) == num(6),
            answer.get(Y).isEmpty,
            answer.as[Int](A) == Right(6),
            answer.as[Double](A) == Right(6.0),
            answer.as[String](B) == Right("bob"),
            answer.as[List[Int]](C) == Right(List(1, 2, 3)),
            answer.as[Boolean](X) == Right(true),
            answer.as[Int](B) == Left(DecodeError("expected an Int, got bob")),
            answer.as[Int](Y) == Left(DecodeError("Y is unbound"))
          )
        }
    },
    test("README example") {
      val woman   = Functor("woman")
      val man     = Functor("man")
      val wealthy = Functor("wealthy")
      val wise    = Functor("wise")
      val happy   = Functor("happy")

      Program
        .build
        .append(
          woman("jean"),
          woman("pat"),
          man("fred"),
          wealthy("fred"),
          wealthy("pat"),
          wise("jean"),

          happy(X) := woman(X) && wealthy(X),
          happy(X) := woman(X) && wise(X)
        )
        .solve(happy(A))
        .map(_.as[String](A))
        .runCollect
        .map(r => assertTrue(r.toList == List(Right("pat"), Right("jean"))))
    },
    test("answers display as bindings") {
      Program.build
        .solve((B =* list(1, 2)) && (A is 1 + 1))
        .runCollect
        .map(r => assertTrue(r.head.show == "A = 2, B = [1, 2]"))
    }
  )

  def testError(msg: String)(query: Goal, error: SolveError): Spec[Any, Nothing] = test(msg) {
    Program.build
      .solve(query)
      .runCollect
      .either
      .map(r => assert(r)(equalTo(Left(error))))
  }

  def testProgram(msg: String)(program: Program, query: Goal, set: Answer*): Spec[Any, SolveError] =
    testProgram(msg)(program, Query(List(query)), set*)

  def testProgram(msg: String)(program: Program, query: Query, set: Answer*): Spec[Any, SolveError] = test(msg) {
    program
      .solve(query)
      .runCollect
      .map(s => assert(s.toSet)(equalTo(set.toSet)))
  }
}
