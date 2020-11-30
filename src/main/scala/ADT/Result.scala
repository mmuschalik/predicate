package Prolog.ADT


case class State(
  query: Query, 
  index: Int, 
  solution: Set[Binding], 
  depth: Int)

case class Stack[T](stack: List[T]) {

  def pop: Stack[T] = 
    if stack.isEmpty then
      this 
    else 
      Stack(stack.tail)

  def pop(size: Int): Stack[T] = Stack(stack.drop(size))
  
  def push(t: T): Stack[T] = Stack(t :: stack)

  def peek: Option[T] = stack.headOption
}

object Stack {
  def empty[T] = Stack[T](Nil)
}

case class Result(
  stack: Stack[State], 
  solution: Option[Set[Binding]],
  isCut: Boolean = false)