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

  def view[B](transform : Rule[B])(input : Context) = new View[A, B, Context](transform, input)
}

   
/**
 * A Scanner is a parser for character input.
 */
trait Scanner extends Parser[Char] {
  implicit def readString(string : String) : Rule[String] = readSeq(string)
  implicit def stringToInput(string : String) : ArrayInput[Char] = new ArrayInput[Char](string.toArray)

  def toString(seq : Seq[Any]) = seq.mkString("")
  
  def range(from : Char, to : Char) = item filter { ch => ch >= from && ch <= to }

  import Character._
  def whitespace = item filter isWhitespace *
  def newline = "\r\n" | "\n" | "\r"

  def trim[A](rule : Rule[A]) = whitespace -~ rule ~- whitespace
}


object ArithmeticEvaluator extends Scanner with Application {
  type Context = ArrayInput[Char]
  
  lazy val expr : Rule[Int] = term ~*~ (op('+', _ + _) | op('-', _ - _))
  lazy val term : Rule[Int] = factor ~*~ (op('*', _ * _) | op('/', _ / _))
  lazy val factor : Rule[Int] = trim(number | '(' -~ expr ~- ')')
  lazy val number = (range('0', '9')+) ^^ toString ^^ (_ toInt)
  
  private def op(r : Rule[Any], f : (Int, Int) => Int) = r ^^ { any => f }

  def evaluate(string : String) = {
    expect(expr ~- !item | error("Invalid expression: " + string))(string)
  }
  
  println(evaluate("7 + 5 * (5+ 6 / 2 - 1)"))
  println(evaluate("10 / 5 / 2"))
}
