package net.foggin.rules.scala

case class ParserState(multipleStatementsAllowed : Boolean, lastTokenCanEndStatement : Boolean) 

class ScalaInput[T <: Input[Char, T] with Memoisable[T]](val input : T, val state : ParserState) 
    extends Input[Char, ScalaInput[T]] with Memoisable[ScalaInput[T]]  {

  def this(input : T) = this(input, ParserState(true, false))
  
  def next = input.next match {
    case Success(ch, input) => Success(ch, new ScalaInput(input, state))
    case _ => Failure[ScalaInput[T]]
  }
  
  def state_=(state : ParserState) = new ScalaInput(input, state)

  def multipleStatementsAllowed = state.multipleStatementsAllowed
  def multipleStatementsAllowed_=(value : Boolean) = state_=(ParserState(value, lastTokenCanEndStatement))

  def lastTokenCanEndStatement = state.lastTokenCanEndStatement
  def lastTokenCanEndStatement_=(value : Boolean) = state_=(ParserState(multipleStatementsAllowed, value))

  def memo[B](key : AnyRef, f : ScalaInput[T] => Result[B, ScalaInput[T]]) : Result[B, ScalaInput[T]] = {
    // Uses the underlying input's memo function by augmenting both the key and the result with the parser state
    val result = input.memo((key, state), input => f(this) match {
      case Success(b, context) => Success((b, context), context.input)
      case _ => Failure[T]
    })
    result match {
      case Success((b, context), input) => Success(b, context)
      case _ => Failure[ScalaInput[T]]
    }
  }

  //override def toString = input.mkString("")
}

