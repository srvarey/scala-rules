package net.foggin.rules;

/** 
 * A Result is either Success or Failure.
 *
 * @author Andrew Foggin
 */
sealed abstract class Result[+A, +Context]

/** Result of a rule was successfully applied.  
 *
 * @requires A the type of value contained by the result
 * @requires Context the type of resulting context
 *
 * @param value the result value
 * @param context the resulting Context
 */
case class Success[+A, +Context](value : A, context : Context) extends Result[A, Context]

/** 
 * Result of a rule that could not be applied.
 */
case object Failure extends Result[Nothing, Nothing]

/** Throw this exception or a subclass to indicate a failure without alternatives
 */
class RuleException[Context](val context : Context, message : String) extends Exception(message)
