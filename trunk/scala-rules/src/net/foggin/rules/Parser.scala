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


/**
 * A Parser is used to define rules that operate on sequential input
 */
trait Parser[A] extends Rules {
  type Context <: Input[A, Context]

  /** Succeeds with the first element of the input unless input is empty. */
  val item = createRule { input => input.next }

  implicit def elem(a : A) = item filter (_ == a)

  def readSeq[C <% Seq[A]](seq : C) : Rule[C] =
      seq.map(elem(_)).reduceLeft[Rule[A]](_ -~ _) ^^^ seq

  def choice[C <% Seq[A]](seq : C) : Rule[A] =
      seq.map(elem(_)).reduceLeft[Rule[A]](_ | _)

  def view[B](transform : Rule[B])(input : Context) = new View[A, B, Context](transform, input)
}

   
/**
 * A Scanner is a parser for character input.
 */
trait Scanner extends Parser[Char] {
  implicit def readString(string : String) : Rule[String] = readSeq(string)
  implicit def stringToInput(string : String) : ArrayInput[Char] = new ArrayInput[Char](string.toArray)

  def literal(seq : Seq[Any]) = seq.mkString("")

  import Character._
  def letter = item filter isLetter
  def digit = item filter isDigit
  def whitespace = item filter isWhitespace *
  def newline = "\r\n" | "\n" | "\r"

  def trim[A](rule : Rule[A]) = whitespace -~ rule ~- whitespace
}


object ArithmeticEvaluator extends Scanner with Application {
  type Context = ArrayInput[Char]
  
  lazy val expr : Rule[Int] = term ~*~ (op('+', _ + _) | op('-', _ - _))
  lazy val term : Rule[Int] = factor ~*~ (op('*', _ * _) | op('/', _ / _))
  lazy val factor : Rule[Int] = trim(number | '(' -~ expr ~- ')')
  lazy val number = (digit+) ^^ literal ^^ (_ toInt)
  
  private def op(r : Rule[Any], f : (Int, Int) => Int) = r ^^ { any => f }

  def evaluate(string : String) = {
    expect(expr ~- !item | error("Invalid expression: " + string))(string)
  }
  
  println(evaluate("7 + 5 * (5+ 6 / 2 - 1)"))
  println(evaluate("10 / 5 / 2"))
}
