package net.foggin.rules

trait Input[+A, Context <: Input[A, Context]] extends Iterable[A] { self : Context =>

  def next : Result[A, Context]

  def elements = new Iterator[A] {
    private var input : Context = Input.this
    private var result = input.next
 
    def hasNext = result isSuccess
    def next = {
      val Success(value, input) = result
      this.input = input
      this.result = input.next
      value
    }
  }
}


class ArrayInput[A](val array : Array[A], val index : Int) extends Input[A, ArrayInput[A]] {
  def this(array : Array[A]) = this(array, 0)

  lazy val next = if (index >= array.length) Failure[ArrayInput[A]]
      else Success(array(index), new ArrayInput[A](array, index + 1))
   
  override lazy val toString = elements.mkString("\"", "", "\"")
}


class IterableInput[A](iterator : Iterator[A]) extends Input[A, IterableInput[A]] {
  def this(iterable : Iterable[A]) = this(iterable.elements)

  lazy val next = if (!iterator.hasNext) Failure[IterableInput[A]]
    else Success(iterator.next, new IterableInput(iterator))
 
  override lazy val toString = elements.mkString("\"", "", "\"")
}


/** View one type of input as another based on a transformation rule */
class View[A, B, Context <: Input[A, Context]](
    transform : Context => Result[B, Context],
    val input : Context)
    extends Input[B, View[A, B, Context]] {

  def next = transform(input) match {
    case Success(b, context) => Success(b, new View(transform, context))
    case _ => Failure[View[A, B, Context]]
  }
}


