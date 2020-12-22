package mmuschalik.Operation

import mmuschalik.ADT._

def unify(x: Term, y: Term): Option[Set[Binding]] =
  unify(List((x, y)), Set())

def unify(stack: List[(Term, Term)], bindings: Set[Binding]): Option[Set[Binding]] =
  stack
    .headOption
    .fold(Some(bindings))(pop => 
      pop match
        case (x: Atom[_], y) if x == y => unify(stack.tail, bindings)
        case (x: Variable, y) if !y.contains(x) => unify(substitute(stack.tail, Binding(y, x)), merge(bindings, Binding(y, x)))
        case (x: Variable, y) if x == y => unify(stack.tail, bindings)
        case (x: Atom[_], y: Variable) => unify((y,x) :: stack.tail, bindings)
        case (x: Predicate, y: Variable) => unify((y,x) :: stack.tail, bindings)
        case (l: Predicate, r: Predicate) 
          if l.name == r.name && l.list.size == r.list.size => unify((l.list zip r.list) ++ stack.tail, bindings)
        case _ => None)

def substitute(stack: List[(Term, Term)], binding: Binding): List[(Term, Term)] = 
  stack.map(m => (m._1.substitute(binding), m._2.substitute(binding)))

def merge(set: Set[Binding], binding: Binding): Set[Binding] = 
  set.map(s => if s.term == binding.variable then Binding(binding.term, s.variable) else s) + binding

def merge(left: Set[Binding], right: Set[Binding]): Set[Binding] =
  right.foldLeft(left)(merge(_, _))

def substitute(list: List[(Term, Term)], sub: Set[Binding]): List[(Term, Term)] =
  substituteTerm(list.map(_._1), sub) zip substituteTerm(list.map(_._2), sub)

def substituteTerm(list: List[Term], sub: Set[Binding]): List[Term] =
  list.map(m => (sub.foldLeft(m)((a, b) => a.substitute(b))))

def substitutePredicate(list: List[Predicate], sub: Set[Binding]): List[Predicate] =
  list.map(m => (sub.foldLeft(m)((a, b) => a.substitute(b))))
  