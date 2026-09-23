package mmuschalik.predicate

// standard predicates defined in the language itself
object Library:

  private val H = variable("H")
  private val T = variable("T")
  private val L = variable("L")
  private val R = variable("R")
  private val E = variable("E")
  private val I = variable("I")
  private val N = variable("N")
  private val N1 = variable("N1")
  private val Acc = variable("Acc")
  private val Acc1 = variable("Acc1")

  private def nth(l: Term, base: Term, index: Term, item: Term) = predicate("$nth", l, base, index, item)
  private def reverse(l: Term, acc: Term, reversed: Term) = predicate("reverse", l, acc, reversed)
  private def sum(l: Term, acc: Term, total: Term) = predicate("$sum_list", l, acc, total)

  val lists: List[Clause] = List(
    append(nil, L, L),
    append(H :: T, L, H :: R) := append(T, L, R),

    member(E, E :: T),
    member(E, H :: T) := member(E, T),

    mmuschalik.predicate.reverse(L, R) := reverse(L, nil, R),
    reverse(nil, Acc, Acc),
    reverse(H :: T, Acc, R) := reverse(T, H :: Acc, R),

    nth0(I, L, E) := nth(L, 0, I, E),
    nth1(I, L, E) := nth(L, 1, I, E),
    nth(E :: T, N, N, E),
    nth(H :: T, N, I, E) := (N1 is N + 1) && nth(T, N1, I, E),

    last(E :: nil, E),
    last(H :: T, E) := last(T, E),

    sumList(L, N) := sum(L, 0, N),
    sum(nil, Acc, Acc),
    sum(H :: T, Acc, N) := (Acc1 is Acc + H) && sum(T, Acc1, N)
  )
