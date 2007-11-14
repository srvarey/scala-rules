package net.foggin.rules;

trait Statement
trait Expression extends Statement

abstract class PathElement extends Expression

case object This extends PathElement with Expression
case class Super(classQualifier : Option[String]) extends PathElement with Expression
case class Name(id : String) extends PathElement with Expression

case object Underscore extends Expression
//case object UnderscoreRepeated extends Expression
case class Literal[T](value : T) extends Expression
case class TupleExpression(exprs : List[Expression]) extends Expression
case class DotExpression(expr1 : Expression, expr2 : PathElement) extends Expression
case class ExpressionTypeArgs(expr : Expression, typeArgs : List[Type]) extends Expression
case class ApplyExpression(expr : Expression, args : List[Expression]) extends Expression
case class Unapplied(expr : Expression) extends Expression

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

case class Block(statements : List[Statement], resultExpr : Option[Expression]) extends Expression

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
case class InfixPattern(left : Expression, rest : List[(String, Expression)]) extends Expression
case class AtPattern(id : String, pattern : Expression) extends Expression
case class TypedVariablePattern(id : String, typeSpec : Type) extends Expression
case class TypePattern(typeSpec : Type) extends Expression
case class OrPattern(patterns : List[Expression]) extends Expression


abstract class Type
abstract class ExistentialType extends Type

case class FunctionType(parameterTypes : Seq[(Type, Boolean)], resultType : Type) extends Type

case class InfixType(id : String, left : Type, right : Type) extends Type
case class CompoundType(annotTypes : List[Type], refinement : Option[Refinement]) extends Type
case class Refinement extends Type
case class AnnotatedType(annotations : List[Annotation], annotated : SimpleType) extends Type

abstract class SimpleType extends Type
case class SingletonType(path : List[PathElement]) extends SimpleType
case class TypeDesignator(path : List[PathElement], id : String) extends SimpleType
case class TupleType(types : Seq[Type]) extends SimpleType
case class TypeProjection(simpleType : SimpleType, id : String) extends SimpleType
case class ParameterizedType(simpleType : SimpleType, typeArgs : Seq[Type]) extends SimpleType

abstract class Annotation

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

case class TypeParameter(id : String, lowerBound : Option[Type], upperBound : Option[Type], viewBound : Option[Type])
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
    
case class TraitDefinition(id : String,
    typeParameters : Option[List[VariantTypeParameter]], 
    traitTemplate : TraitTemplate) extends Definition
    
case class ClassTemplate(
    earlyDefs : Option[List[AnnotatedDefinition]], 
    parent : Option[Type], 
    arguments : List[List[Expression]], 
    otherParents : List[Type], 
    templateBody : Option[TemplateBody])
    
case class TraitTemplate(
    earlyDefs : Option[List[AnnotatedDefinition]], 
    parents : List[Type], 
    templateBody : Option[TemplateBody])
    
case class TemplateBody(alias : Option[String], selfType : Option[Type], statements : List[Statement])

case class AnnotatedDeclaration(annotations : List[Annotation], modifiers : List[Modifier], declaration : Declaration) extends Statement
case class AnnotatedDefinition(annotations : List[Annotation], modifiers : List[Modifier], definition : Definition) extends Statement

    
    
    