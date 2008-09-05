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

package scalax.rules.example;

case class Name(name : String)

sealed abstract class Type
case object BooleanType extends Type
case class FunctionType(from : Type, to : Type) extends Type

sealed abstract class Term
case object True extends Term
case object False extends Term
case class Variable(name: Name) extends Term
case class Function(argName : Name, argType : Type, body : Term) extends Term
case class App(function : Term, arg : Term) extends Term


class BindingRules[T] extends RulesWithState {
  type S = Map[Name, T]
  val empty : S = Map.empty[Name, T]
  
  def bind(name : Name, value : T) = apply { ctx => Success(ctx(name) = value, value) }
  def boundValue(name : Name) = apply { ctx => if (ctx.contains(name)) Success(ctx, ctx(name)) else Failure }
}

class Typer extends BindingRules[Type] {
  def typeOf(term : Term) : Rule[Type, Nothing] = unit(term) >> {
    case True | False => unit(BooleanType)
    case Variable(name) => boundValue(name)
    case Function(n, t, body) => bind(n, t) ~ typeOf(body) ^~^ FunctionType &
    case App(function, arg) => typeOf(function) ~ typeOf(arg) >>? {
      case FunctionType(from, to) ~ argType if (from == argType) => unit(to)
    }
  }
}


object TestTyper extends Typer with Application {

  def check(pairs : (Term, Type)*) {
    for ((term, expected) <- pairs) typeOf(term)(empty) match {
      case Success(_ , actual) => if (actual != expected) Predef.error("Term: " + term + 
          "\nExpected type: " + expected +
          "\nActual type: " + actual)
      case _ => Predef.error("Term: " + term + 
          "\nExpected type: " + expected +
          "\nDid not typecheck")
    }
  }
  
  val fx = Function(Name("x"), BooleanType, Variable(Name("x")))
  
  check(
      True -> BooleanType,
      False -> BooleanType,
      fx -> FunctionType(BooleanType, BooleanType),
      App(fx, True) -> BooleanType
      )
  
  println("STLC Typer tests passed")
}
