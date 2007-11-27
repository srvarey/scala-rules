package net.foggin.rules.stlc;

case class Name(name : String)

sealed abstract class Type
case object BooleanType extends Type
case class FunctionType(from : Type, to : Type) extends Type

sealed abstract class Term
case object True extends Term
case object False extends Term
case class Variable(name: Name) extends Term
case class Function(argName : Name, argType : Type, body : Term) extends Term
case class Application(function : Term, arg : Term) extends Term


class BindingRules[T] extends Rules {
  type Context = _root_.scala.collection.immutable.Map[Name, T]
  
  def bind(name : Name, value : T) = read { ctx => ctx(name) = value }
  def withBinding[A](name : Name, value : T, rule : Rule[A]) = bind(name, value) >>> rule
  def boundValue(name : Name) = createRule { ctx => if (ctx.contains(name)) Success(ctx(name), ctx) else Failure[Context] }
}

class Typer extends BindingRules[Type] {
  def typeOf(term : Term) : Rule[Type] = success(term) >> {
    case True | False => success(BooleanType)
    case Variable(name) => boundValue(name)
    case Function(n, t, body) => success(t) ~ withBinding(n, t, typeOf(body)) ^~^ FunctionType
    case Application(function, arg) => typeOf(function) ~ typeOf(arg) >>? {
      case FunctionType(from, to) ~ argType if (from == argType) => success(to)
    }
  }
}
