package net.foggin.rules

/** Represents the combined value of two rules applied in sequence.
 *
 * @see the Scala parser combinator
 */
case class ~[+A, +B](_1 : A, _2 : B)
  
object Rule {
  trait SM[S] { type M[+A] = Rule[S, A] }
    
  def apply[S, A](f : S => Result[A, S]) = new Rule(f)
    
  def unit[S, A](a : => A) = apply { s : S => Success(a, s) }
    
  def get[S] = apply { s : S => Success(s, s) }
  
  def update[S](f : S => S) = apply { s : S => Success(s, f(s)) }
}
  
class Rule[S, +A](f : S => Result[A, S]) extends (S => Result[A, S])
    with MonadPlus[A, Rule.SM[S]#M] {
  
  val companion = new MonadCompanion[Rule.SM[S]#M] {
    def unit[A](a : => A) = Rule.unit[S, A](a)
    override def zero = Rule { s : S => Failure[S] }
  }
  import companion._
  
  def apply(s : S) = f(s)
  
  def flatMap[B](f : A => Rule[S, B]) = Rule[S, B] { 
    s : S => apply(s) match { 
      case Success(a, s) => f(a)(s) 
      case _ => Failure[S]
    }
  }
  
  def plus[B >: A](other : => Rule[S, B]) = Rule { 
    s : S => apply(s).plus(other(s))
  }

  def |[B >: A](other : => Rule[S, B]) = plus(other)

  def ^^[B](f : A => B) = map(f)
  
  def ^^^[B](b : B) = map { any => b }
  
  def >>[B](f : A => Rule[S, B]) = flatMap(f)
  
  def ~[B](next : => Rule[S, B]) = for (a <- this; b <- next) yield new ~(a, b)
  
  def ~-[B](next : => Rule[S, B]) = for (a <- this; b <- next) yield a
  
  def -~[B](next : => Rule[S, B]) = for (a <- this; b <- next) yield b
  
  def ~++[B >: A](next : => Rule[S, Seq[B]]) = for (a <- this; b <- next) yield a :: b.toList
  
  def ? : Rule[S, Option[A]] = ^^ (Some(_)) | unit(None)

  def * = Rule[S, List[A]] { c =>
    // tail-recursive function with reverse list accumulator
    def rep(acc : List[A], c : S) : (List[A], S) = apply(c) match {
       case Success(a, c) => rep(a :: acc, c)
       case _ => (acc, c)
    }
    rep(Nil, c) match { case (list, c) => Success(list.reverse, c) }
  }

  def + = this ~++ *

  def ~*~[B >: A](join : Rule[S, (B, B) => B]) : Rule[S, B] = {
    val rhs = for (f <- join; a <- this) yield f(_ : B, a)
    for (a <- this; fs <- rhs*) yield fs.foldLeft[B](a) { (b, f) => f(b) }
  }
  
  /** Repeats this rule one or more times with a separator (which is discarded) */
  def ~+~(sep : Rule[S, Any]) = this ~++ (sep -~ this *)

  /** Creates a rule that suceeds only if this rule would fail on the given context. */
  def unary_! = for (s <- Rule.get[S] if !apply(s).isSuccess) yield s
       
  /** Creates a rule that suceeds if this rule would succeed but returns an unmodified context. 
   * N.B. won't work properly because of deprecated syntax for unapplied method? */
  def unary_& = for (s <- Rule.get[S] if apply(s).isSuccess) yield s
  
  def -(exclude : => Rule[S, Any]) = !exclude -~ this
    
  def *~-(end : => Rule[S, Any]) = (this - end *) ~- end
  def +~-(end : => Rule[S, Any]) = (this - end +) ~- end
}
