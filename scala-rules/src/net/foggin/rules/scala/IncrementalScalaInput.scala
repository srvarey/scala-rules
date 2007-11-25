package net.foggin.rules.scala

class IncrementalScalaInput(val input : DefaultIncrementalInput, val state : ParserState) 
    extends ScalaInput[IncrementalScalaInput] 
    with Input[Char, IncrementalScalaInput] {

  def this(input : DefaultIncrementalInput) = this(input, ParserState(true, false))
  
  def next = input.next match {
    case Success(ch, input) => Success(ch, new IncrementalScalaInput(input, state))
    case _ => Failure[IncrementalScalaInput]
  }

  def multipleStatementsAllowed = state.multipleStatementsAllowed
  def multipleStatementsAllowed_=(value : Boolean) = new IncrementalScalaInput(input, ParserState(value, lastTokenCanEndStatement))

  def lastTokenCanEndStatement = state.lastTokenCanEndStatement
  def lastTokenCanEndStatement_=(value : Boolean) = new IncrementalScalaInput(input, ParserState(multipleStatementsAllowed, value))

  def memo[B](key : AnyRef, f : IncrementalScalaInput => Result[B, IncrementalScalaInput]) : Result[B, IncrementalScalaInput] =
    input.memo((key, state), input => f(this) match {
      case Success(b, context) => Success((b, context), context.input)
      case _ => Failure[DefaultIncrementalInput]
    }) match {
      case Success((b, context), input) => Success(b, context)
      case _ => Failure[IncrementalScalaInput]
    }

  //override def toString = input.mkString("")
}

