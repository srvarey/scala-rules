package net.foggin.rules;

/** 
 * Throw this exception or a subclass to indicate a failure without alternatives
 */
class RuleException[Context](val context : Context, message : String) extends Exception(message)

object Result {
  trait SM[S] { type M[+A] = Result[A, S] }
}
  
/** 
 * A Result is either Success or Failure.
 *
 * @author Andrew Foggin
 */
sealed abstract class Result[+A, S] extends MonadPlus[A, Result.SM[S]#M]
  
/** 
 * Result of a rule that was successfully applied.  
 */
case class Success[+A, S](value : A, rest : S) extends Result[A, S] {
  val companion = new MonadCompanion[Result.SM[S]#M] {
    def unit[A](a : => A) : Result[A, S] = Success(a, rest)
    override def zero = Failure[S]
 }
    
  def flatMap[B](f : A => Result[B, S]) = f(value)
  def plus[B >: A](other : => Result[B, S]) = this
}
  
/** 
 * Result of a rule that could not be applied.
 */
case class Failure[S] extends Result[Nothing, S] with MonadZero[Result.SM[S]#M] {
  val companion = new MonadCompanion[Result.SM[S]#M] {
    def unit[A](a : => A) : Result[A, S] = Failure[S]
  }
}