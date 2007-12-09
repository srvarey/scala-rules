package net.foggin.rules

import _root_.scala.collection.mutable.HashMap

trait MemoisableRules extends Rules {
  type Context <: Memoisable[Context]
  
  def memo[A](key : AnyRef, f : Context => Result[A]) : Rule[A] = createRule[A] { ctx => ctx.memo(key, f) }
}

trait Memoisable[Context] {
  def memo[A](key : AnyRef, f : Context => Result[A, Context]) : Result[A, Context]
}

trait DefaultMemoisable[Context <: Memoisable[Context]] extends Memoisable[Context] {
  
  self : Context =>

  protected val map = new HashMap[AnyRef, Result[Any, Context]]

  def memo[T](key : AnyRef, f : Context => Result[T, Context]) = {
    map.getOrElseUpdate(key, compute(key, f)).asInstanceOf[Result[T, Context]]
  }
  
  protected def compute[T](key : AnyRef, f : Context => Result[T, Context]) = f(this) match {
    case success @ Success(value, context) => onSuccess(key, success); success
    case failure => failure
  }
  
  protected def onSuccess[T](key : AnyRef,  result : Success[T, Context]) { }
}


