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

package scalax.rules

trait Name {
  def name : String
  override def toString = name
}

/** A factory for rules.
  * 
  * @author Andrew Foggin
  *
  * Inspired by the Scala parser combinator.
  */
trait Rules {
  implicit def rule[In, Out, A, X](f : In => Result[Out, A, X]) : Rule[In, Out, A, X] = new DefaultRule(f)

  implicit def inRule[In, Out, A, X](rule : Rule[In, Out, A, X]) : InRule[In, Out, A, X] = new InRule(rule)
  implicit def seqRule[In, A, X](rule : Rule[In, In, A, X]) : SeqRule[In, A, X] = new SeqRule(rule)
  
  def from[In] = new {
    def apply[Out, A, X](f : In => Result[Out, A, X]) = rule(f)
  }
  
  def state[s] = new StateRules {
    type S = s
    val factory = Rules.this
  }
  
  def success[Out, A](out : Out, a : A) = rule { in : Any => Success(out, a) }
  
  def failure = rule { in : Any => Failure }
  
  def error[In] = rule { in : In => Error(in) }
  def error[X](err : X) = rule { in : Any => Error(err) }
      
  def oneOf[In, Out, A, X](rules : Seq[Rule[In, Out, A, X]]) : Rule[In, Out, A, X] = rules.reduceLeft[Rule[In, Out, A, X]](_ | _)

  def ruleWithName[In, Out, A, X](_name : String, f : In => Result[Out, A, X]) : Rule[In, Out, A, X] with Name = 
    new DefaultRule(f) with Name {
      val name = _name
    }

  class DefaultRule[In, Out, A, X](f : In => Result[Out, A, X]) extends Rule[In, Out, A, X] {
    val factory = Rules.this
    def apply(in : In) = f(in)
  }
  
  /** Converts a rule into a function that throws an Exception on failure. */
  def expect[In, Out, A, Any](rule : Rule[In, Out, A, Any]) : In => A = (in) => rule(in) match {
    case Success(_, a) => a
    case Failure => throw new RuntimeException("Unexpected failure")
    case Error(x) => throw new RuntimeException("Unexpected error: " + x)
  }
}

/** A factory for rules that apply to a particular context.
  *
  * @requires S the context to which rules apply.
  *
  * @author Andrew Foggin
  *
  * Inspired by the Scala parser combinator.
  */
trait StateRules {
  type S
  type Rule[+A, +X] = rules.Rule[S, S, A, X]
  
  val factory : Rules
  import factory._
  
  def apply[A, X](f : S => Result[S, A, X]) = rule(f)

  def unit[A](a : => A) = apply { s => Success(s, a) }
  def read[A](f : S => A) = apply { s => Success(s, f(s)) }
  
  def get = apply { s => Success(s, s) }
  def set(s : => S) = apply { oldS => Success(s, oldS) }

  def update(f : S => S) = apply { s => Success(s, f(s)) }
  
  def nil = unit(Nil)
  def none = unit(None)
  
  /** Create a rule that suceeds if f(in) is true. */
  def cond(f : S => Boolean) = get filter f

  /** Create a rule that succeeds if all of the given rules succeed.
      @param rules the rules to apply in sequence.
      @return the results as a list.
  */
  def allOf[A, X](rules : Seq[Rule[A, X]]) = 
    rules.foldRight[Rule[List[A], X]](nil)(_ ~++ _)

  
}

trait RulesWithState extends Rules with StateRules {
  val factory = this
}