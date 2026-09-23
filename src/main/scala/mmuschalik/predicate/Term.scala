package mmuschalik.predicate

sealed trait Term:

  type This >: this.type <: Term
  type Substitution >: this.type <: Term

  def /(variable: Variable): Binding = 
    Binding(this, variable)

  def show: String

  def substitute(binding: Binding): Substitution

  def contains(variable: Variable): Boolean

  def rename(newVersion: Int): This

  def =*(other: Term): Predicate = eql(this, other)

  def +(other: Term): Predicate = plus(this, other)

  def -(other: Term): Predicate = minus(this, other)

  def *(other: Term): Predicate = multiply(this, other)

  def /(other: Term): Predicate = divide(this, other)

  def %(other: Term): Predicate = mod(this, other)

  def unary_- : Predicate = predicate("-", this)

  // unification and structural identity
  def !=*(other: Term): Predicate = predicate("\\=", this, other)

  def ===(other: Term): Predicate = predicate("==", this, other)

  def =!=(other: Term): Predicate = predicate("\\==", this, other)

  // arithmetic comparison, both sides are evaluated
  def <(other: Term): Predicate = predicate("<", this, other)

  def >(other: Term): Predicate = predicate(">", this, other)

  def <=(other: Term): Predicate = predicate("=<", this, other)

  def >=(other: Term): Predicate = predicate(">=", this, other)

  def =:=(other: Term): Predicate = predicate("=:=", this, other)

  def =\=(other: Term): Predicate = predicate("=\\=", this, other)

  // list cell, right associative: H :: T
  def ::(head: Term): Predicate = cons(head, this)

case class Atom(name: String) extends Term:

  type This = Atom
  type Substitution = This

  def show: String = name

  def substitute(binding: Binding): Substitution = this

  def contains(variable: Variable): Boolean = false

  def rename(newVersion: Int): This = this

case class Num(value: BigDecimal) extends Term:

  type This = Num
  type Substitution = This

  def show: String = value.toString

  def substitute(binding: Binding): Substitution = this

  def contains(variable: Variable): Boolean = false

  def rename(newVersion: Int): This = this

case class Variable(name: String, version: Int) extends Term:

  type This = Variable
  type Substitution = Term

  def show: String = 
    if version == 0 then 
      name 
    else 
      name + version.toString

  def substitute(binding: Binding): Substitution = 
    if binding.variable == this then 
      binding.term 
    else 
      this

  def contains(variable: Variable): Boolean = 
    this == variable

  def rename(newVersion: Int): This = 
    if version == 0 then 
      Variable("_" + name, newVersion) 
    else 
      this
  
  infix def is(other: Term): Predicate = predicate("is", this, other)

case class Predicate(name: String, list: List[Term] = Nil) extends Term:

  type This = Predicate
  type Substitution = Predicate

  def key: String = 
    name + list.size.toString

  def show: String = 
    this match
      case Predicate(".", _ :: _ :: Nil) =>
        val (items, tail) = spine
        val end = tail match
          case Atom("[]") => ""
          case t => "|" + t.show
        "[" + items.map(_.show).mkString(", ") + end + "]"
      case _ =>
        name + "(" + list.map(_.show).mkString(", ") + ")"

  // the elements of a list cell chain and whatever ends it (nil for a proper list)
  private def spine: (List[Term], Term) =
    @annotation.tailrec
    def loop(t: Term, acc: List[Term]): (List[Term], Term) =
      t match
        case Predicate(".", head :: tail :: Nil) => loop(tail, head :: acc)
        case end => (acc.reverse, end)
    loop(this, Nil)

  def contains(variable: Variable): Boolean = 
    list.exists(_.contains(variable))

  def substitute(binding: Binding): Substitution = 
    Predicate(name, list.map(m => m.substitute(binding)))

  def substitute(binding: Set[Binding]): Predicate = 
    binding.foldLeft(this)((a, b) => a.substitute(b))

  def rename(newVersion: Int): This = 
    Predicate(name, list.map(m => m.rename(newVersion)))

  def &&(right: Predicate): Predicate = 
    Predicate(",", List(this, right))

  def ||(right: Predicate): Predicate = 
    Predicate(";", List(this, right))

  def :=(body: Predicate) = 
    Clause(this, body :: Nil)

  def :=(query: Query) = 
    Clause(this, query.goals)

end Predicate

type Goal = Predicate