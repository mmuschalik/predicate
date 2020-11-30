import Prolog._
import Prolog.ADT._

case class Name(name: String, age: Int)
def name(name: Term, age: Term): Predicate = predicate("name", name, age)
given BuildPredicate[Name] {
  def build(a: Name): Predicate = name(atom(a.name), atom(a.age))
}

case class User(name: String)
def user(name: Term): Predicate = predicate("user", name)
given BuildPredicate[User] {
  def build(a: User): Predicate = user(atom(a.name))
}

object Main {

  def main(args: Array[String]): Unit = {
    val names = 
      Name("maurice", 36) :: 
      Name("reika", 36) ::
      Name("sophie", 6) ::
      Nil
  
    val users = 
      User("ellie") :: 
      User("fabi") :: 
      Nil

    given program as Program = Program
      .build
      .append(names)
      .append(users)
      .append(name(X, 0) := user(X))

    val myQuery = query(name(A, B))

    val r = next(myQuery)

    display(r)

  }


  //================================== DISPLAY ========================================

  trait Show[T] {
    def show(t: T): String
  }

  given Show[Term] {
    def show(t: Term): String = 
      t match
        case Atom(a) => a.toString
        case Variable(n, version) => if version == 0 then n else n + version.toString
        case p: Predicate => p.name
  }

  given (using Show[Term]) as Show[Binding] {
    def show(b: Binding): String = summon[Show[Term]].show(b.variable) + " = " + summon[Show[Term]].show(b.term)
  }

  def display(result: Option[Result])(using Program, Show[Binding]): Unit =
    result.foreach(r => {
      r.solution.foreach(s => println(s.map(x => summon[Show[Binding]].show(x)).mkString(", ")))
      display(next(r.stack))
    })

}
