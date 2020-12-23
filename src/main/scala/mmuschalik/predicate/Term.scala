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

case class Atom[T](a: T) extends Term:

  type This = Atom[T]
  type Substitution = This

  def show: String = a.toString

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

case class Predicate(name: String, list: List[Term] = Nil) extends Term:

  type This = Predicate
  type Substitution = Predicate

  def key: String = 
    name + list.size.toString

  def show: String = 
    name + "(" + list.map(_.show).mkString(", ") + ")"

  def contains(variable: Variable): Boolean = 
    list.find(
      _ match
        case term: Variable => term.name == variable.name
        case term: Predicate => term.contains(variable)
        case _ => false
    ).isDefined

  def substitute(binding: Binding): Substitution = 
    Predicate(name, list.map(m => m.substitute(binding)))

  def substitute(binding: Set[Binding]): Predicate = 
    binding.foldLeft(this)((a, b) => a.substitute(b))

  def rename(newVersion: Int): This = 
    Predicate(name, list.map(m => m.rename(newVersion)))

  def &&(right: Predicate) = 
    Query(List(this,right))

  def :=(body: Predicate) = 
    Clause(this, body :: Nil)

  def :=(query: Query) = 
    Clause(this, query.goals)

end Predicate

type Goal = Predicate