package net.foggin.rules


/*  

  object Rule {
    trait SM[S] { type M[+A] = Rule[S, A] }
    
    def apply[S, A](f : S => Result[A, S]) = new Rule(f)
    
    def unit[S, A](a : => A) = apply { s : S => Success(a, s) }
    
    def get[S] = apply { s : S => Success(s, s) }
  }
  
  
  class Rule[S, +A](f : S => Result[A, S]) extends (S => Result[A, S])
          with MonadPlus[A, Rule.SM[S]#M] {
    
    val companion = new MonadCompanion[Rule.SM[S]#M] {
      def unit[A](a : => A) = Rule.unit[S, A](a)
      override def zero = Rule { s : S => Failure[S] }
    }
    
    def apply(s : S) = f(s)
    
    def flatMap[B >: A](f : A => Rule[S, B]) = Rule[S, B] { 
      s : S => apply(s) match { 
        case Success(a, s) => f(a)(s) 
        case _ => Failure[S]
      }
    }
    
    def plus[B >: A](other : => Rule[S, B]) = Rule { 
      s : S => apply(s).plus(other(s))
    }
    
    def >>=[B >: A](f : A => Rule[S, B]) = flatMap(f) // a.k.a. '>>'
    def ^^[B](f : A => B) = map(f)
    def |[B >: A](other : => Rule[S, B]) = plus(other)
    def ~[B](next : => Rule[S, B]) = for (a <- this; b <- next) yield (a, b)
    // etc...
  }


  object StateTransformer {
    trait SM[S] { type M[+A] = StateTransformer[S, A] }
    
    def apply[S, A](f : S => (A, S)) = new StateTransformer(f)
    
    def unit[S, A](a : => A) = apply { s : S => (a, s) }
    
    def get[S] = apply { s : S => (s, s) }
  }
  
  
  class StateTransformer[S, +A](f : S => (A, S)) extends (S => (A, S))
          with Monad[A, StateTransformer.SM[S]#M] {
    
    val companion = new MonadCompanion[StateTransformer.SM[S]#M] {
      def unit[A](a : => A) = StateTransformer.unit[S, A](a)
    }
    
    def apply(s : S) : (A, S) = f(s)
    
    def flatMap[B](f : A => StateTransformer[S, B]) = StateTransformer { 
      s : S => apply(s) match { case (a, s) => f(a)(s) }
    }
  }
*/