package net.foggin.rules


/**
 * A Parser is used to define rules that operate on sequential input
 */
trait Parser[A] extends Rules {
  type Context <: Input[A, Context]

  /** Succeeds with the first element of the input unless input is empty. */
  val item = createRule { input => input.next }

  implicit def elem(a : A) = item filter (_ == a)

  def readSeq[C <% Seq[A]](seq : C) : Rule[C] = if (seq isEmpty) success(seq)
      else seq.map(elem(_)).reduceLeft[Rule[A]](_ -~ _) -^ seq

  def choice[C <% Seq[A]](seq : C) : Rule[A] = select(seq.map(elem))

  /** Allows rules like 'a' to 'z' */
  implicit def iteratorToChoice[B <: Iterator[A]](iterator : B) : Rule[A] = choice(iterator.toList)

  def view[B](transform : Rule[B])(input : Context) = new View[A, B, Context](transform, input, 0)
}

   
/**
 * A Scanner is a parser for character input.
 */
trait Scanner extends Parser[Char] {
  implicit def readString(string : String) : Rule[String] = readSeq(string)
  implicit def stringToInput(string : String) : ArrayInput[Char] = new ArrayInput[Char](string.toArray)

  def toString(seq : Seq[Any]) = seq.mkString("")
  
  import Character._
  def whitespace = item filter isWhitespace *
  def newline = "\r\n" | "\n" | "\r"

  def trim[A](rule : Rule[A]) = whitespace -~ rule ~- whitespace
}

