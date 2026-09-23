package mmuschalik.predicate

sealed trait Term:

  def show: String

  // gives the clause variables a fresh version so each use of a clause is independent
  def rename(newVersion: Int): Term

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

  def show: String = name

  def rename(newVersion: Int): Atom = this

case class Num(value: BigDecimal) extends Term:

  def show: String = value.toString

  def rename(newVersion: Int): Num = this

case class Variable(name: String, version: Int) extends Term:

  def show: String = 
    if version == 0 then 
      name 
    else 
      name + version.toString

  def rename(newVersion: Int): Variable = 
    if version == 0 then 
      Variable("_" + name, newVersion) 
    else 
      this
  
  infix def is(other: Term): Predicate = predicate("is", this, other)

case class Predicate(name: String, list: List[Term] = Nil) extends Term:

  def key: (String, Int) = 
    (name, list.size)

  def show: String = 
    this match
      case Predicate(".", _ :: _ :: Nil) =>
        val (items, tail) = spine
        val end = tail match
          case Atom("[]") => ""
          case t => "|" + t.show
        "[" + items.map(_.show).mkString(", ") + end + "]"
      case Predicate(name, Nil) =>
        name
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

  def rename(newVersion: Int): Predicate = 
    this match
      case Predicate(".", _ :: _ :: Nil) =>
        // loop along list cells so long lists in clauses don't overflow the stack
        val (items, tail) = spine
        val renamedTail = tail.rename(newVersion)
        items
          .map(_.rename(newVersion))
          .foldRight(renamedTail)((head, rest) => Predicate(".", List(head, rest)))
          .asInstanceOf[Predicate]
      case _ =>
        Predicate(name, list.map(_.rename(newVersion)))

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
