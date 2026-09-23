package mmuschalik.predicate.engine

import mmuschalik.predicate.*

// triangular substitution: a variable may map to a term that contains further bound variables
type Subst = Map[Variable, Term]

def walk(term: Term, s: Subst): Term =
  term match
    case v: Variable => s.get(v).fold(v)(walk(_, s))
    case t => t

def resolve(term: Term, s: Subst): Term =
  walk(term, s) match
    case Predicate(name, list) => Predicate(name, list.map(resolve(_, s)))
    case t => t

def occurs(variable: Variable, term: Term, s: Subst): Boolean =
  walk(term, s) match
    case v: Variable => v == variable
    case p: Predicate => p.list.exists(occurs(variable, _, s))
    case _ => false

def unify(x: Term, y: Term, s: Subst): Option[Subst] =
  (walk(x, s), walk(y, s)) match
    case (l, r) if l == r => Some(s)
    case (v: Variable, t) => if occurs(v, t, s) then None else Some(s + (v -> t))
    case (t, v: Variable) => if occurs(v, t, s) then None else Some(s + (v -> t))
    case (l: Predicate, r: Predicate) if l.name == r.name && l.list.size == r.list.size =>
      (l.list zip r.list).foldLeft(Option(s))((acc, pair) => acc.flatMap(unify(pair._1, pair._2, _)))
    case _ => None

def unify(x: Term, y: Term): Option[Set[Binding]] =
  unify(x, y, Map()).map(s => s.keySet.map(v => Binding(resolve(v, s), v)))

def variables(term: Term): List[Variable] =
  term match
    case v: Variable => List(v)
    case p: Predicate => p.list.flatMap(variables)
    case _ => Nil
