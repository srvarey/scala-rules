package net.foggin.rules

/** Represents the combined value of two rules applied in sequence.
 *
 * @see the Scala parser combinator
 */
case class ~[+A, +B](_1 : A, _2 : B)
  

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
  
  val FAILURE = Failure[Context]

  /** Converts a function into a rule. */
  implicit def createRule[A](f : Context => Result[A, Context]) = new Rule[A] {
    def apply(ctx : Context) = f(ctx)
  }
	  
  /** Converts a PartialFunction into a Rule. */
  implicit def partialFunctionToRule[A](pf : PartialFunction[Context, (A, Context)]) = createRule[A] {
    pf andThen { case (a, ctx) => Success(a, ctx) } orElse { case ctx => FAILURE }
  }

  /** Creates a Rule that always succeeds with the specified value. */
  def success[A](a : A) = createRule { ctx : Context => Success(a, ctx) }
  
  /**
   * A Rule is a Function from Context to Result[A, Context] with operations to allow for comprehensions and combinations.
   */
  abstract class Rule[+A] extends (Context => Result[A, Context]) {
   
    def flatMap[B](f : A => Context => Result[B, Context]) = mapRule[B] {
      case (_, Success(a, c2)) => f(a)(c2)
      case _ => FAILURE
    }

    def map[B](f : A => B) = flatMap { a => ctx => Success(f(a), ctx) }

    def filter(f : A => Boolean) = flatMap { a => ctx => if (f(a)) Success(a, ctx) else FAILURE }

    def | [B >: A](alt : => Context => Result[B, Context]) = mapRule[B] { 
      case (c1, FAILURE) => alt(c1)
      case (_, success) => success
    }

    def ^^[B](f : A => B) = map(f)
  
    def >>[B](f : A => Context => Result[B, Context]) = flatMap(f)
  
    def ~[B](next : => Rule[B]) = for (a <- this; b <- next) yield new ~(a, b)
  
    def ~-[B](next : => Rule[B]) = for (a <- this; b <- next) yield a
  
    def -~[B](next : => Rule[B]) = for (a <- this; b <- next) yield b
  
    def ~++[B >: A](next : => Rule[Seq[B]]) = for (a <- this; b <- next) yield a :: b.toList
  
    def ? : Rule[Option[A]] = ^^ (Some(_)) | success(None)

    def * = createRule[List[A]] { c =>
      // tail-recursive function with reverse list accumulator
      def rep(acc : List[A], c : Context) : (List[A], Context) = apply(c) match {
        case FAILURE => (acc, c)
        case Success(a, c) => rep(a :: acc, c)
      }
      rep(Nil, c) match { case (list, c) => Success(list.reverse, c) }
    }

    def + = this ~++ *

    def ~*~[B >: A](join : Rule[(B, B) => B]) : Rule[B] = {
      val rhs = for (f <- join; a <- this) yield f(_ : B, a)
      for (a <- this; fs <- rhs*) yield fs.foldLeft[B](a) { (b, f) => f(b) }
    }
    
    /** Creates a rule that suceeds only if this rule would fail on the given context. */
    def unary_! = mapRule { 
      case (c, FAILURE) => Success(c, c) 
      case _ => FAILURE
    }
     
    /** Creates a rule that suceeds if this rule would succeed but returns an unmodified context. */
    def unary_& = mapRule { 
      case (c, Success(_, _)) => Success(c, c) 
      case _ => FAILURE
    }
  
    private def mapApply[B](f : (Context, Result[A, Context]) => B)(c : Context) = f(c, apply(c))

    private def mapRule[B](f : (Context, Result[A, Context]) => Result[B, Context]) = createRule[B](mapApply(f))

  }

  /** Primitive rule that always suceeds and returns the context with which it is called.
   */
  lazy val context = createRule { ctx => Success(ctx, ctx) }
  
  /** Create a rule that always suceeds with the given value.
   */
  def value[A](a : A) = createRule { ctx => Success(a, ctx) }
    
  /** Create a rule that suceeds if f(context) is true.  The value returned is the context.
   */
  def predicate(f : Context => Boolean) = for (ctx <- context if f(ctx)) yield ctx

  /** Create a rule that suceeds if pf isDefinedAt context.  
   * The resulting context is the result of applying pf.
   * The value returned is the initial context.
   */
  def action(pf : PartialFunction[Context, Context]) = createRule { ctx => 
    if (pf isDefinedAt ctx) Success(pf(ctx), ctx)
    else FAILURE
  }

  def get[A](pf : PartialFunction[Context, A]) = for (ctx <- context if pf isDefinedAt ctx) yield pf(ctx)
  
  def toException(f : Context => String) = createRule[Nothing] { ctx => throw new RuleException(ctx, f(ctx)) }
  def exception(message : String) = toException { ctx => message }
  
  /** Converts a rule into a function that throws a RuleException on failure.
    */
  implicit def expect[A](rule : Rule[A]) : Context => A = (context) => rule(context) match {
    case Success(a, _) => a
    case FAILURE => throw new RuleException(context, "Unexpected failure")
  }
}
