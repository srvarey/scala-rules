package scalax.rules.example;

//An example re-implementation of (part of) scala.Option:

object Option extends Monads with Zero {
  type M[+A] = Option[A]
    
  override def unit[A](a : => A) = Some(a)
  override def zero = None
}
  
sealed abstract class Option[+A] extends Option.Monad[A] with OrElse[A]

case class Some[+A](value : A) extends Option[A] {
  def flatMap[B](f : A => Option[B]) = f(value)
  def orElse[B >: A](other : => Option[B]) = this
}

case object None extends Option[Nothing] with Option.ZeroMonad

/*
  trait MonadArrows extends Arrows {
    type Arr[-A, +B] <: MonadArrow[A, B]

    trait MonadArrow[-A, +B] extends Arrow[A, B] with (A => Monads#Monad[B]) { this : Arr[A, B] =>
      def comp[C](bc : => Arr[B, C]) = arrow[A, C](a => for (b <- apply(a); c <- bc(b)) yield c) //(a => f(a) flatMap bc.f)
      def fst[C] = arrow[(A, C), (B, C)]({ case (a,c) => for(b <- apply(a)) yield (b,c) }) //({ case (a, c) => f(a) map { b => (b, c) } })
    }
  }
  */




//  trait MonadicMonoidalFunctors extends Monads with MonoidalFunctors {
//    type M[+A] <: MonadicMonoidalFunctor[A]
  //
//    class MonadicMonoidalFunctor[+A](val fa : M[A]) extends Monad[A] with MonoidalFunctor[A] { self : M[A] =>
//     def flatMap[B](a2fb : A => M[B]) : M[B] = fa flatMap a2fb
//     override def and[B](fb : => M[B]) : M[(A,B)] = for (a <- fa;  b <- fb) yield (a,b)
//    }
//  }

