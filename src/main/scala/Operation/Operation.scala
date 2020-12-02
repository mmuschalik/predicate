package Prolog.Operation

import Prolog.ADT._

def unify(x: Term, y: Term): (Set[Binding], Option[Term]) =
  unify(List((x, y)), Set())
    .fold((Set(), None))(b => (b, Some(substitute(x, b))))

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

def substitute(term: Term, bindings: Set[Binding]): Term = 
  bindings.foldLeft(term)((t, b) => t.substitute(b))

def merge(set: Set[Binding], binding: Binding): Set[Binding] = 
  set.map(s => if s.term == binding.variable then Binding(binding.term, s.variable) else s) + binding