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

package scalax.rules.example

trait ArithmeticEvaluator extends Scanners {
  lazy val expr : Parser[Int] = term ~*~ (op('+', _ + _) | op('-', _ - _)) as "expr"
  lazy val term : Parser[Int] = factor ~*~ (op('*', _ * _) | op('/', _ / _)) as "term"
  lazy val factor : Parser[Int] = trim(number | '(' -~ expr ~- ')') as "factor" 
  lazy val number = (('0' to '9')+) ^^ toString ^^ (_ toInt) as "number"
  
  private def op(r : Parser[Any], f : (Int, Int) => Int) = r -^ f

  def evaluate = expr ~- !item | error("Invalid expression")
}
  
object ExampleUsage extends ArithmeticEvaluator with StringScanners with Application {
  println(evaluate("7 + 5 * (5+ 6 / 2 - 1)"))
}


object ExampleUsage2 extends ArithmeticEvaluator with IncrementalScanners with Application {
  type X = Nothing
  DefaultMemoisable.debug = true
  
  // set up initial text and evaluate
  val input = new IncrementalInput[Char]
  input.edit(0, 0, "7 + 5 * (5+ 6 / 2 - 1)")
  println(evaluate(input))
  
   // change to "7 + (5 + 1) * (5+ 6 / 2 - 1)"
  input.edit(4, 1, "(5 + 1)")
  println(evaluate(input))
  
   // change to "(5 + 1) * (5+ 6 / 2 - 1)"
  input.edit(0, 4, "")
  println(evaluate(input))
}
