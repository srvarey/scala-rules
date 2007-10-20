package net.foggin.rules


trait Input[+A, Context] { self : Context =>
  def next : Result[A, Context]
}

class ArrayInput[A](val array : Array[A], val index : Int) extends Input[A, ArrayInput[A]] {
  def this(array : Array[A]) = this(array, 0)
  
  def next = 
      if (index >= array.length) Failure 
      else Success(array(index), new ArrayInput[A](array, index + 1))
}



/** 
 * A Parser is used to define rules that operate on sequential input
 */
trait Parser[A] extends Rules {
  type Context <: Input[A, Context]

  /** Succeeds with the first element of the input unless input is empty.
   */
  val item = createRule { input => input.next }
  
  implicit def elem(a : A) = for (x <- item if x == a) yield x
  
  final def readSeq[C <% Seq[A]](seq : C) : Rule[C] = 
    seq.map(elem(_)).reduceLeft[Rule[A]](_ -~ _) ^^ (any => seq)
     
  def readIf(condition : A => Boolean) = for (x <- item if condition(x)) yield x
}

/** 
 * A Scanner is a parser for character input.
 */
trait Scanner extends Parser[Char] {
     
  implicit def readString(string : String) : Rule[String] = readSeq(string)
  
  implicit def stringToInput(string : String) : ArrayInput[Char] = new ArrayInput[Char](string.toArray)
  
  def literal(seq : Seq[Char]) = seq.mkString("")
    
  val letter = readIf(_ isLetter)
  val digit = readIf(_ isDigit)
  val whitespace = readIf(_ isWhitespace)*
  val newline = "\r\n" | "\n" | "\r"
  
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
