## Welcome to predicate

This project is inspired by the logic programming language prolog. It's main objective is to bring the same problem solving capabilities prolog has to the JVM, using a execution strategy called chronological backtracking.

### Usage

```scala
Program
    .build
    .append(
                woman(jean),
                woman(pat),
                man(fred),
                wealthy(fred),
                wealthy(pat),
                wise(jean),

                happy(X) := woman(X) && wealthy(X),
                happy(X) := woman(X) && wise(X)
    )
    .solve(happy(A))

```

The result will yield a set of solutions:

```
pat /A
jean /A
```