package net.foggin.rules

  trait Functor[+A, M[+_]] { 
    self : M[A] =>
    def map[B >: A](f : A => B) : M[B]
  }

  trait MonadCompanion[M[+_]] {
    def unit[A](a : => A) : M[A]
    def zero : M[Nothing] = error("Not available")
  }
  
  trait Monad[+A, M[+_]] extends Functor[A, M] { 
    self : M[A] =>
    
    val companion : MonadCompanion[M]
    import companion._
    
    def flatMap[B >: A](f : A => M[B]) : M[B]
    
    def map[B >: A](f : A => B) = flatMap { a => unit(f(a)) }
    def filter[B >: A](f : A => Boolean) = flatMap { a => if (f(a)) unit(a) else zero }
  }

  trait MonadPlus[+A, M[+_]] extends Monad[A, M] {
    self : M[A] =>
    def plus[B >: A](other : => M[B]) : M[B]
  }

  trait MonadZero[M[+_]] extends MonadPlus[Nothing, M] {
    self : M[Nothing] =>
    def plus[B](other : => M[B]) = other
    def flatMap[B](f : Nothing => M[B]) : M[B] = this
  }


  //This leads for example to a re-implementation of (part of) Option as:
 /*
  object Option extends MonadCompanion[Option] {
    def unit[B](b : => B) = Some(b)
    override def zero = None
  }
  
  sealed abstract class Option[+A] extends MonadPlus[A, Option] {
    val companion = Option
  }

  case class Some[+A](value : A) extends Option[A] {
    def flatMap[B](f : A => Option[B]) = f(value)
    def plus[B >: A](other : => Option[B]) = this
  }

  case object None extends Option[Nothing] with MonadZero[Option]
*/

