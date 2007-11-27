package net.foggin.rules.example;

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


class BindingRules[T] extends Rules {
  type Context = _root_.scala.collection.immutable.Map[Name, T]
  val empty : Context = _root_.scala.collection.immutable.Map.empty[Name, T]
  
  def bind(name : Name, value : T) = createRule { ctx => Success(value, ctx(name) = value) }
  def boundValue(name : Name) = createRule { ctx => if (ctx.contains(name)) Success(ctx(name), ctx) else Failure[Context] }
}

class Typer extends BindingRules[Type] {
  def typeOf(term : Term) : Rule[Type] = success(term) >> {
    case True | False => success(BooleanType)
    case Variable(name) => boundValue(name)
    case Function(n, t, body) => bind(n, t) ~ typeOf(body) ^~^ FunctionType &
    case App(function, arg) => typeOf(function) ~ typeOf(arg) >>? {
      case FunctionType(from, to) ~ argType if (from == argType) => success(to)
    }
  }
}


object TestTyper extends Typer with Application {

  def check(pairs : (Term, Type)*) {
    for ((term, expected) <- pairs) typeOf(term)(empty) match {
      case Success(actual, _) => if (actual != expected) error("Term: " + term + 
          "\nExpected type: " + expected +
          "\nActual type: " + actual)
      case _ => error("Term: " + term + 
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