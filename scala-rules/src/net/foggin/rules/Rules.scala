package net.foggin.rules

/** Defines Rules that apply to a particular Context.
 * 
 * The result may be:
 * - Success, with a resulting Context and a value of some type.  Several successful rules may be applied in sequence and their values combined.
 * - Failure. A failure may result in some alternative rule being applied.
 * - Error, indicated by throwing a RuleException.  No further rules should be attempted.
 *
 * @requires Context the type of context to which rules apply.
 *
 * @author Andrew Foggin
 * @author inspired by the Scala parser combinator
 */
trait Rules {
  type Context
  type Result[+A] = rules.Result[A, Context]
  type Rule[+A] = rules.Rule[Context, A]
  
  /** Converts a function into a rule. */
  implicit def createRule[A](f : Context => Result[A]) = Rule(f)
	  
  /** Converts a PartialFunction into a Rule. */
  implicit def partialFunctionToRule[A](pf : PartialFunction[Context, (A, Context)]) = createRule[A] {
    pf andThen { case (a, ctx) => Success(a, ctx) } orElse { case ctx => Failure[Context] }
  }

  /** Creates a Rule that always succeeds with the specified value. */
  def success[A](a : A) = Rule.unit[Context, A](a)
  
  /** Primitive rule that always suceeds and returns the context with which it is called. */
  lazy val context = Rule.get[Context]
  
  /** Create a rule that suceeds if f(context) is true.  The value returned is the context. */
  def predicate(f : Context => Boolean) = for (ctx <- context if f(ctx)) yield ctx

  def toException(f : Context => String) = createRule[Nothing] { ctx => throw new RuleException(ctx, f(ctx)) }
  def exception(message : String) = toException { ctx => message }
  
  /** Converts a rule into a function that throws a RuleException on failure.
    */
  implicit def expect[A](rule : Rule[A]) : Context => A = (context) => rule(context) match {
    case Success(a, _) => a
    case _ => throw new RuleException(context, "Unexpected failure")
  }
}
