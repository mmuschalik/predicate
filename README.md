## Welcome to predicate

This project is inspired by the logic programming language prolog. It's main objective is to bring the same problem solving capabilities prolog has to the JVM, using a execution strategy called chronological backtracking. 

Requires Scala 3 and ZIO 2. Run the tests with `sbt test`.

### Usage

```scala
import mmuschalik.predicate.*

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
    .solve(happy(A))          // ZStream[Any, SolveError, Answer]
    .map(_.as[String](A))
    .runCollect
```

The result will yield the solutions `Right("pat")` and `Right("jean")`. Answers are produced lazily, so `.take(n)` on an infinite search is fine.

### The language

| Prolog | predicate |
| --- | --- |
| `head :- a, b.` | `head := a && b` |
| `a ; b` | `a \|\| b` |
| `( C -> T ; E )` | `ifThenElse(c, t, e)`, `ifThen(c, t)` |
| `!`, `\+ G`, `call(G)` | `cut`, `not(g)`, `call(g)` |
| `throw(B)`, `catch(G, C, R)` | `raise(b)`, `catching(g, c, r)` |
| `X = Y`, `X \= Y` | `X =* Y`, `X !=* Y` |
| `X == Y`, `X \== Y` | `X === Y`, `X =!= Y` |
| `X is A + B` | `X is A + B` (also `- * / %`, `intDiv`, `abs`, `min`, `max`) |
| `<  >  =<  >=  =:=  =\=` | `<  >  <=  >=  =:=  =\=` |
| `[1, 2 \| T]`, `[]` | `1 :: 2 :: T`, `list(1, 2)`, `nil` |

Built-in predicates: `true`, `fail`, `between`, `length`, `append`, `member`, `reverse`, `nth0`, `nth1`, `last`, `sumList`, and the type checks `isVar`, `isNonVar`, `isAtom`, `isNumber`, `isCompound`.

Constants are atoms (`Atom`, from a `String`) or numbers (`Num`, from any Scala number). Errors such as `InstantiationError`, `TypeError`, `EvaluationError` and `UncaughtThrow` fail the stream, and can be caught inside a program with `catching(goal, error(E), recovery)`.

An `Answer` maps the query's bound variables to terms: `answer(A)` gives the term, and `answer.as[T](A)` decodes it to `Int`, `Long`, `Double`, `BigDecimal`, `String`, `Boolean` or `List[T]`.
