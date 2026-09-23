package mmuschalik.predicate.engine

import mmuschalik.predicate.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

// triangular substitution: a variable may map to a term that contains further bound variables
type Subst = Map[Variable, Term]

// the term traversals below use loops or an explicit work list rather than recursion on term depth,
// because lists are nested one cell per element and would otherwise overflow the stack

@tailrec
def walk(term: Term, s: Subst): Term =
  term match
    case v: Variable =>
      s.get(v) match
        case Some(t) => walk(t, s)
        case None => v
    case t => t

def resolve(term: Term, s: Subst): Term =
  walk(term, s) match
    case Predicate(".", _ :: _ :: Nil) =>
      // loop along the list cells, only recursing into the elements
      val items = ListBuffer[Term]()
      var rest = walk(term, s)
      while rest match { case Predicate(".", _ :: _ :: Nil) => true; case _ => false } do
        val cell = rest.asInstanceOf[Predicate]
        items += resolve(cell.list.head, s)
        rest = walk(cell.list(1), s)
      items.foldRight(resolve(rest, s))((head, tail) => Predicate(".", List(head, tail)))
    case Predicate(name, list) => Predicate(name, list.map(resolve(_, s)))
    case t => t

def occurs(variable: Variable, term: Term, s: Subst): Boolean =
  @tailrec
  def loop(pending: List[Term]): Boolean =
    pending match
      case Nil => false
      case t :: rest =>
        walk(t, s) match
          case v: Variable => if v == variable then true else loop(rest)
          case p: Predicate => loop(p.list ::: rest)
          case _ => loop(rest)
  loop(List(term))

def unify(x: Term, y: Term, s: Subst): Option[Subst] =
  @tailrec
  def loop(pending: List[(Term, Term)], s: Subst): Option[Subst] =
    pending match
      case Nil => Some(s)
      case (l, r) :: rest =>
        (walk(l, s), walk(r, s)) match
          case (v: Variable, w: Variable) if v == w => loop(rest, s)
          case (v: Variable, t) => if occurs(v, t, s) then None else loop(rest, s + (v -> t))
          case (t, v: Variable) => if occurs(v, t, s) then None else loop(rest, s + (v -> t))
          case (a: Predicate, b: Predicate) if a.name == b.name && a.list.size == b.list.size =>
            loop((a.list zip b.list) ::: rest, s)
          case (a, b) => if sameConstant(a, b) then loop(rest, s) else None
  loop(List((x, y)), s)

// structural identity without binding anything (==/2)
def identical(x: Term, y: Term, s: Subst): Boolean =
  @tailrec
  def loop(pending: List[(Term, Term)]): Boolean =
    pending match
      case Nil => true
      case (l, r) :: rest =>
        (walk(l, s), walk(r, s)) match
          case (v: Variable, w: Variable) => if v == w then loop(rest) else false
          case (a: Predicate, b: Predicate) if a.name == b.name && a.list.size == b.list.size =>
            loop((a.list zip b.list) ::: rest)
          case (a, b) => if sameConstant(a, b) then loop(rest) else false
  loop(List((x, y)))

// a zero-argument predicate and an atom of the same name are the same constant
private def sameConstant(a: Term, b: Term): Boolean =
  (a, b) match
    case (Atom(n), Atom(m)) => n == m
    case (Num(n), Num(m)) => n == m
    case (Atom(n), Predicate(m, Nil)) => n == m
    case (Predicate(m, Nil), Atom(n)) => n == m
    case _ => false

def unify(x: Term, y: Term): Option[Set[Binding]] =
  unify(x, y, Map()).map(s => s.keySet.map(v => Binding(resolve(v, s), v)))

// the distinct variables of a term, in order of first appearance
def variables(term: Term): List[Variable] =
  @tailrec
  def loop(pending: List[Term], found: List[Variable]): List[Variable] =
    pending match
      case Nil => found.reverse.distinct
      case (v: Variable) :: rest => loop(rest, v :: found)
      case (p: Predicate) :: rest => loop(p.list ::: rest, found)
      case _ :: rest => loop(rest, found)
  loop(List(term), Nil)
