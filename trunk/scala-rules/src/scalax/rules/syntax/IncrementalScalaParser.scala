// -----------------------------------------------------------------------------
//
//  Scalax - The Scala Community Library
//  Copyright (c) 2005-8 The Scalax Project. All rights reserved.
//
//  The primary distribution site is http://scalax.scalaforge.org/
//
//  This software is released under the terms of the Revised BSD License.
//  There is NO WARRANTY.  See the file LICENSE for the full text.
//
// -----------------------------------------------------------------------------

package scalax.rules.syntax;

/** 
 *
 * @author Andrew Foggin
 */
class IncrementalScalaParser extends MemoisableRules with ScalaParser {
  type S = ScalaInput
  
  def nextChar = rule { _.next }

  def multiple(allow : Boolean) = read(_.multipleStatementsAllowed) ~- update(_.multipleStatementsAllowed = allow)
  val multipleStatementsAllowed = cond(_.multipleStatementsAllowed)

  def lastTokenCanEndStatement(value : Boolean) = update(_.lastTokenCanEndStatement = value) 
  val lastTokenCanEndStatement = cond(_.lastTokenCanEndStatement)
    
  val position = read { ctx => () => ctx.index }
}

case class ParserState(multipleStatementsAllowed : Boolean, lastTokenCanEndStatement : Boolean) 

class ScalaInput(val input : IncrementalInput[Char], val state : ParserState) 
    extends Input[Char] with Memoisable  {

  def this(input : IncrementalInput[Char]) = this(input, ParserState(true, false))
  
  def index = input.index
  
  def next : Result[ScalaInput, Char, Nothing] = input.next match {
    case Success(input, ch) => Success(new ScalaInput(input, state), ch)
    case _ => Failure
  }
  
  def state_=(state : ParserState) = new ScalaInput(input, state)

  def multipleStatementsAllowed = state.multipleStatementsAllowed
  def multipleStatementsAllowed_=(value : Boolean) = state_=(ParserState(value, lastTokenCanEndStatement))

  def lastTokenCanEndStatement = state.lastTokenCanEndStatement
  def lastTokenCanEndStatement_=(value : Boolean) = state_=(ParserState(multipleStatementsAllowed, value))

  def memo[A](key : AnyRef, a : => A) : A = {
    // Uses the underlying input's memo function by augmenting both the key and the result with the parser state
    val result = input.memo((key, state), a match {
      case Success(context : ScalaInput, a) => Success(context.input, (a, context))
      case other => other
    })
    result match {
      case Success(input, (a, context)) => Success(context, a).asInstanceOf[A]
      case other => other.asInstanceOf[A]
    }
  }

  //override def toString = state + input.toString
}
