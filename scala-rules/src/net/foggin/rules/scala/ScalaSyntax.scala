package net.foggin.rules.scala;

object Element {
  type Position = Input[Char, _]
}

import Element._

trait Element[T] {
  def value : T
  def start : Int
  def length : Int
  
  override def equals(other : Any) = other match {
    case element : Element[_] => value == element.value
    case _ => false
  }
  
  override def hashCode = value.hashCode
}

case class ScalaElement[T](from : Position, value : T, to : Position) extends Element[T] {
  val start = from.index
  val length = to.index - start
}

trait Statement
case object EmptyStatement extends Statement

trait Expression extends Statement

sealed abstract class Literal extends Expression

case object Null extends Literal
case object True extends Literal
case object False extends Literal

case class CharacterLiteral(char : Char) extends Literal
case class StringLiteral(string : String) extends Literal
case class SymbolLiteral(symbol : Symbol) extends Literal
case class IntegerLiteral(value : Int) extends Literal
case class LongLiteral(value : Long) extends Literal
case class FloatLiteral(value : Float) extends Literal
case class DoubleLiteral(value : Double) extends Literal


abstract class PathElement extends Expression

case object This extends PathElement with Expression
case class Super(classQualifier : Option[String]) extends PathElement with Expression
case class Name(id : String) extends PathElement with Expression

case object Underscore extends Expression
case class TupleExpression(exprs : List[Expression]) extends Expression
case class DotExpression(expr1 : Expression, expr2 : PathElement) extends Expression
case class ExpressionTypeArgs(expr : Expression, typeArgs : List[Type]) extends Expression
case class ApplyExpression(expr : Expression, args : List[Expression]) extends Expression
case class Unapplied(expr : Expression) extends Expression
case class InstanceCreation(template : Expression) extends Expression

case class SimpleAssignment(id : String, value : Expression) extends Expression
case class DotAssignment(expr : Expression, id : String, value : Expression) extends Expression
case class Update(expr : Expression, args : List[Expression], value : Expression) extends Expression

case class IfExpression(cond : Expression, expr : Expression, elseExpr : Option[Expression]) extends Expression
case class WhileExpression(cond : Expression, expr : Expression) extends Expression
case class DoExpression(expr : Expression, cond : Expression) extends Expression

case class ForComprehension(enumerators : List[Enumerator], generator : Boolean, expr : Expression) extends Expression
case class MatchExpression(expr : Expression, caseClauses : CaseClauses) extends Expression
case class TryCatchFinally(block : Block, catchClause : Option[CaseClauses], finallyClause : Option[Expression]) extends Expression

case class Throw(expr : Expression) extends Expression
case class Return(expr : Option[Expression]) extends Expression

case class PostfixExpression(expr : Expression, id : String) extends Expression
case class InfixExpression(id : String, left : Expression, right : Expression) extends Expression
case class PrefixExpression(id : String, expr : Expression) extends Expression

case class Block(statements : List[Statement], result : Option[Expression]) extends Expression

case class CaseClause(pattern : Expression, guard : Option[Expression], block : Block)
case class CaseClauses(clauses : List[CaseClause]) extends Expression

trait Enumerator
case class Generator(pattern : Expression, expr : Expression, guard : Option[Expression]) extends Enumerator
case class Guard(guard : Expression) extends Enumerator
case class ValEnumerator(pattern : Expression, expr : Expression) extends Enumerator

case class Binding(id  : String, typeSpec : Option[Type])
case class FunctionExpression(bindings : List[Binding], expr : Expression) extends Expression

case class TypedExpression(expr : Expression, typeSpec : Type) extends Expression
case class AnnotatedExpression(expr : Expression, annotations : List[Annotation]) extends Expression
case class VarArgExpression(expr : Expression) extends Expression


case class VariablePattern(id : String) extends Expression
case class StableIdPattern(path : List[PathElement], args : Option[List[Expression]], varArgs : Boolean) extends Expression
case class AtPattern(id : String, pattern : Expression) extends Expression
case class TypedVariablePattern(id : String, typeSpec : Type) extends Expression
case class TypePattern(typeSpec : Type) extends Expression
case class OrPattern(pattern : Expression, orPattern : Expression) extends Expression

case class NodeList(nodes : List[Expression]) extends Expression
case class TextNode(text : String) extends Expression
case class CData(text : String) extends Expression
case class XMLComment(ext : String) extends Expression
case class XMLElement(name : String, attributes : List[Attribute])(content : Option[Expression]) extends Expression
case class Attribute(name : String, value : Expression)
case class ProcessingInstruction(name : String, text : String) extends Expression
case class EntityRef(name : String) extends Expression

case class XMLPattern(name : String)(content : Option[Expression]) extends Expression


abstract class Type
case class FunctionType(parameterTypes : List[ParameterType], resultType : Type) extends Type
case class ExistentialType(infixType : Type, declarations : List[Declaration]) extends Type
case class InfixType(left : Type, id : String, right : Type) extends Type
case class CompoundType(baseType : Type, withTypes : List[Type], refinement : Option[Refinement]) extends Type
case class Refinement(statements : List[Statement]) extends Type
case class AnnotatedType(simpleType : Type, annotations : List[Annotation]) extends Type
case class SingletonType(path : List[PathElement]) extends Type
case class TypeDesignator(path : List[PathElement], id : String) extends Type
case class TupleType(types : Seq[Type]) extends Type
case class TypeProjection(simpleType : Type)(id : String) extends Type
case class ParameterizedType(simpleType : Type)(typeArgs : Seq[Type]) extends Type

case class Annotation(typeSpec : Type, args : List[List[Expression]], values : List[(String, Expression)])

trait Declaration extends Statement
case class ValDeclaration(ids : List[String], typeSpec : Type) extends Declaration
case class VarDeclaration(ids : List[String], typeSpec : Type) extends Declaration

case class FunctionDeclaration(id : String, 
    typeParamClause : Option[List[TypeParameter]], 
    paramClauses : List[List[Parameter]], 
    implicitParamClause : Option[List[Parameter]], 
    returnType : Option[Type]) extends Declaration
    
case class TypeDeclaration(id : String, 
    typeParameters : Option[List[VariantTypeParameter]], 
    lowerBound : Option[Type], 
    upperBound : Option[Type]) extends Declaration

case class TypeParameter(id : String, typeParameters : Option[List[VariantTypeParameter]], lowerBound : Option[Type], upperBound : Option[Type], viewBound : Option[Type])
case class VariantTypeParameter(variance : Variance, typeParam : TypeParameter)

sealed abstract class Variance
case object Invariant extends Variance
case object Covariant extends Variance
case object Contravariant extends Variance

case class Parameter(
    annotations : List[Annotation], 
    id : String, 
    paramType : Option[ParameterType])

case class ClassParameter(
    annotations : List[Annotation], 
    modifiers : Option[ClassParameterModifiers], 
    id : String, 
    paramType : Option[ParameterType])

case class ParameterType(byName : Boolean, typeSpec : Type, repeated : Boolean)

trait ClassParameterModifiers
case class ValParameterModifiers(modifiers : List[Modifier]) extends ClassParameterModifiers
case class VarParameterModifiers(modifiers : List[Modifier]) extends ClassParameterModifiers


case class ImportStatement(imports : List[Import]) extends Statement
case class Import(path : List[PathElement], selectors : List[ImportSelector])
case class ImportSelector(id : String, as : Option[String])

sealed abstract class Modifier

case object Override extends Modifier
case object Abstract extends Modifier
case object Final extends Modifier
case object Sealed extends Modifier
case object Implicit extends Modifier
case object Lazy extends Modifier

case class Private(qualifier : Option[PathElement]) extends Modifier
case class Protected(qualifier : Option[PathElement]) extends Modifier

trait Definition extends Statement

case class ImplicitDefinition(definition : Definition) extends Definition
case class ValPatternDefinition(patterns : List[Expression], typeSpec : Option[Type], expr : Expression) extends Definition
case class VarPatternDefinition(patterns : List[Expression], typeSpec : Option[Type], expr : Expression) extends Definition
case class VarDefaultDefinition(ids : List[String], typeSpec : Type) extends Definition

case class FunctionDefinition(id : String, 
    typeParamClause : Option[List[TypeParameter]], 
    paramClauses : List[List[Parameter]], 
    implicitParamClause : Option[List[Parameter]], 
    returnType : Option[Type],
    expr : Expression) extends Definition
    
case class ProcedureDefinition(id : String, 
    typeParamClause : Option[List[TypeParameter]], 
    paramClauses : List[List[Parameter]], 
    implicitParamClause : Option[List[Parameter]], 
    expr : Expression) extends Definition

case class ConstructorExpression(
    selfInvocationArguments : List[List[Expression]], 
    statements : List[Statement])
    
case class ConstructorDefinition(
    paramClauses : List[List[Parameter]], 
    implicitParamClause : Option[List[Parameter]], 
    expr : ConstructorExpression) extends Definition
    
case class TypeDefinition(id : String, 
    typeParameters : Option[List[VariantTypeParameter]], 
    typeSpec : Type) extends Definition
    
case class ClassDefinition(caseClass : Boolean,
    id : String,
    typeParameters : Option[List[VariantTypeParameter]], 
    annotations : List[Annotation],
    accessModifier : Option[Modifier],
    paramClauses : List[List[ClassParameter]], 
    implicitParamClause : Option[List[ClassParameter]], 
    classTemplate : ClassTemplate) extends Definition
    
case class ClassTemplate(
    earlyDefs : Option[List[AnnotatedDefinition]], 
    parent : Option[Type], 
    arguments : List[List[Expression]], 
    otherParents : List[Type], 
    templateBody : Option[TemplateBody]) extends Expression
    
case class ObjectDefinition(caseObject : Boolean,
    id : String,
    classTemplate : ClassTemplate) extends Definition
    
case class TraitDefinition(id : String,
    typeParameters : Option[List[VariantTypeParameter]], 
    earlyDefs : Option[List[AnnotatedDefinition]], 
    parents : List[Type], 
    templateBody : Option[TemplateBody]) extends Definition
    
    
case class TemplateBody(alias : Option[String], selfType : Option[Type], statements : List[Element[Statement]]) extends Expression

case class AnnotatedDeclaration(annotations : List[Annotation], modifiers : List[Modifier], declaration : Declaration) extends Statement
case class AnnotatedDefinition(annotations : List[Annotation], modifiers : List[Modifier], definition : Definition) extends Statement

case class Packaging(qualId : List[String], statements : List[Element[Statement]]) extends Statement

case class CompilationUnit(qualId : Option[List[String]], statements : List[Element[Statement]]) extends Statement
    
    