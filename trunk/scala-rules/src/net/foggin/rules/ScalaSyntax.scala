package net.foggin.rules;

trait Statement
trait Expression extends Statement

abstract class PathElement extends Expression

case object This extends PathElement with Expression
case class Super(classQualifier : Option[String]) extends PathElement with Expression
case class Name(id : String) extends PathElement with Expression

case object Underscore extends Expression
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
    typeParameters : List[VariantTypeParameter], 
    lowerBound : Option[Type], 
    upperBound : Option[Type]) extends Declaration

case class TypeParameter(id : String, lowerBound : Option[Type], upperBound : Option[Type], viewBound : Option[Type])
case class VariantTypeParameter(id : String, lowerBound : Option[Type], upperBound : Option[Type], viewBound : Option[Type], variance : Variance)

sealed abstract class Variance
case object Invariant extends Variance
case object Covariant extends Variance
case object Contravariant extends Variance

case class Parameter(id : String, byName : Boolean, typeSpec : Option[Type], varArgs : Boolean, annotations : List[Annotation])

case class ImportStatement(imports : List[Import]) extends Statement
case class Import(path : List[PathElement], selectors : List[ImportSelector])
case class ImportSelector(id : String, as : Option[String])