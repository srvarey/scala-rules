package scalax.rules.scalasig

abstract class Type

case object NoType extends Type
case object NoPrefixType extends Type

case class ThisType(symbol : Symbol) extends Type
case class SingleType(typeRef : Type, symbol : Symbol) extends Type
case class ConstantType(constant : Any) extends Type
case class TypeRefType(prefix : Type, symbol : Symbol, typeArgs : Seq[Type]) extends Type
case class TypeBoundsType(lower : Type, upper : Type) extends Type
case class RefinedType(classSymRef : Int, typeRefs : List[Type]) extends Type
case class ClassInfoType(symbol : Symbol, typeRefs : Seq[Type]) extends Type
case class MethodType(resultType : Type, paramTypes : Seq[Type]) extends Type
case class PolyType(typeRef : Type, symbols : Seq[TypeSymbol]) extends Type
case class ImplicitMethodType(resultType : Type, paramTypes : Seq[Type]) extends Type
case class AnnotatedType(typeRef : Type, attribTreeRefs : List[Int]) extends Type
case class AnnotatedWithSelfType(typeRef : Type, symbol : Symbol, attribTreeRefs : List[Int]) extends Type
case class DeBruijnIndexType(typeLevel : Int, typeIndex : Int) extends Type
case class ExistentialType(typeRef : Type, symbols : Seq[Symbol]) extends Type
  
