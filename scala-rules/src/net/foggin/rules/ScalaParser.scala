package net.foggin.rules;

case class Literal[T](value : T)

/** Rules based on Scala Language Specification 2.6.0 
 *
 * UNDER CONSTRUCTION!
 */
abstract class ScalaParser extends ScalaScanner {
  
  def round[T](rule : Rule[T]) = token('(') -~ rule ~- token(')')
  def square[T](rule : Rule[T]) = token('[') -~ rule ~- token(']')
  def curly[T](rule : Rule[T]) = token('{') -~ rule ~- token('}')
  
  /** QualId ::= id {‘.’ id} */
  val qualId = id ~ ('.' -~ id *)
  
  /** ids ::= id {‘,’ id} */
  val ids = id ~ (',' -~ id *)
  
  /** Path ::= StableId
   *    | [id ‘.’] this */
  lazy val path : Rule[Any] = stableId | (id ~- '.' ?) ~ `this`
    
  /** StableId ::= id
   *    | Path ‘.’ id
   *    | [id ’.’] super [ClassQualifier] ‘.’ id */
  lazy val stableId : Rule[Any] = id | path ~- '.' ~ id | (id ~- '.' ?) ~ `super` ~ (classQualifier ?) ~- '.' ~ id
  
  /** ClassQualifier ::= ‘[’ id ‘]’ */
  val classQualifier = '[' -~ id ~- ']'
    
    
  /** Type ::= InfixType ‘=>’ Type
   *    | ‘(’ [‘=>’ Type] ‘)’ ‘=>’ Type
   *    | InfixType [ExistentialClause] */
  lazy val typeSpec : Rule[Any] = 
      infixType ~ `=>` ~ typeSpec |
      round(`=>` ~ typeSpec ?) ~ `=>` ~ typeSpec | // don't think this is right - should be multiple parameter types with optional lazy?
      infixType ~ existentialClause
    
  /** ExistentialClause ::= forSome ‘{’ ExistentialDcl {semi ExistentialDcl}} ‘}’ */
    // note typo above (double }})
  lazy val existentialClause = `forSome` ~  curly(existentialDcl ~ (semi -~ existentialDcl *))
      
  /** ExistentialDcl ::= type TypeDcl | val ValDcl */
  def existentialDcl : Rule[Any]
        
  /** InfixType ::= CompoundType {id [nl] CompoundType} */
  lazy val infixType = compoundType ~ (id ~- (nl?) ~ compoundType *)
          
  /** CompoundType ::= AnnotType {with AnnotType} [Refinement]
   *    | Refinement */
  def compoundType : Rule[Any]
            
 /** AnnotType ::= {Annotation} SimpleType
SimpleType ::= SimpleType TypeArgs
| SimpleType ‘#’ id
| StableId
| Path ‘.’ type
| ‘(’ Types [‘,’] ’)’
TypeArgs ::= ‘[’ Types ‘]’
Types ::= Type {‘,’ Type}
Refinement ::= [nl] ‘{’ RefineStat {semi RefineStat} ‘}’
RefineStat ::= Dcl
| type TypeDef
|
TypePat ::= Type
Ascription ::= ‘:’ CompoundType
| ‘:’ Annotation {Annotation}
| ‘:’ ‘_’ ‘*’
Expr ::= (Bindings | id) ‘=>’ Expr
| Expr1
Expr1 ::= if ‘(’ Expr ‘)’ {nl} Expr [[semi] else Expr]
| while ‘(’ Expr ‘)’ {nl} Expr
| try ‘{’ Block ‘}’ [catch ‘{’ CaseClauses ‘}’]
[finally Expr]
| do Expr [semi] while ‘(’ Expr ’)’
| for (‘(’ Enumerators ‘)’ | ‘{’ Enumerators ‘}’)
{nl} [yield] Expr
| throw Expr
| return [Expr]
| [SimpleExpr ‘.’] id ‘=’ Expr
| SimpleExpr1 ArgumentExprs ‘=’ Expr
| PostfixExpr
| PostfixExpr Ascription
| PostfixExpr match ‘{’ CaseClauses ‘}’
PostfixExpr ::= InfixExpr [id [nl]]
InfixExpr ::= PrefixExpr
| InfixExpr id [nl] InfixExpr
PrefixExpr ::= [‘’
| ‘+’ | ‘~’ | ‘!’] SimpleExpr
SimpleExpr ::= new (ClassTemplate | TemplateBody)
| BlockExpr
| SimpleExpr1 [‘_’]
SimpleExpr1 ::= Literal
| Path
| ‘_’
| ‘(’ [Exprs [‘,’]] ‘)’
| SimpleExpr ‘.’ id
| SimpleExpr TypeArgs
| SimpleExpr1 ArgumentExprs
| XmlExpr

Exprs ::= Expr {‘,’ Expr}
ArgumentExprs ::= ‘(’ [Exprs [‘,’]] ’)’
| [nl] BlockExpr
BlockExpr ::= ‘{’ CaseClauses ‘}’
| ‘{’ Block ‘}’
Block ::= {BlockStat semi} [ResultExpr]
BlockStat ::= Import
| [implicit] Def
| {LocalModifier} TmplDef
| Expr1
|
ResultExpr ::= Expr1
| (Bindings | id ‘:’ CompoundType) ‘=>’ Block
Enumerators ::= Generator {semi Enumerator}
Enumerator ::= Generator
| Guard
| val Pattern1 ‘=’ Expr
Generator ::= Pattern1 ‘<’
Expr [Guard]
CaseClauses ::= CaseClause { CaseClause }
CaseClause ::= case Pattern [Guard] ‘=>’ Block
Guard ::= ‘if’ PostfixExpr
Pattern ::= Pattern1 { ‘|’ Pattern1 }
Pattern1 ::= varid ‘:’ TypePat
| ‘_’ ‘:’ TypePat
| Pattern2
Pattern2 ::= varid [‘@’ Pattern3]
| Pattern3
Pattern3 ::= SimplePattern
| SimplePattern { id [nl] SimplePattern }
SimplePattern ::= ‘_’
| varid
| Literal
| StableId
| StableId ‘(’ [Patterns [‘,’]] ‘)’
| StableId ‘(’ [Patterns ‘,’] ‘_’ ‘*’ ‘)’
| ‘(’ [Patterns [‘,’]] ‘)’
| XmlPattern
Patterns ::= Pattern [‘,’ Patterns]
| ‘_’ *
TypeParamClause ::= ‘[’ VariantTypeParam {‘,’ VariantTypeParam} ‘]’
FunTypeParamClause::= ‘[’ TypeParam {‘,’ TypeParam} ‘]’
VariantTypeParam ::= [‘+’ | ‘’]
TypeParam
TypeParam ::= id [>: Type] [<: Type] [<% Type]
ParamClauses ::= {ParamClause} [[nl] ‘(’ implicit Params ‘)’]
ParamClause ::= [nl] ‘(’ [Params] ’)’}

Params ::= Param {‘,’ Param}
Param ::= {Annotation} id [‘:’ ParamType]
ParamType ::= Type
| ‘=>’ Type
| Type ‘*’
ClassParamClauses ::= {ClassParamClause}
[[nl] ‘(’ implicit ClassParams ‘)’]
ClassParamClause ::= [nl] ‘(’ [ClassParams] ’)’
ClassParams ::= ClassParam {‘’ ClassParam}
ClassParam ::= {Annotation} [{Modifier} (‘val’ | ‘var’)]
id [‘:’ ParamType]
Bindings ::= ‘(’ Binding {‘,’ Binding ‘)’
Binding ::= id [‘:’ Type]
Modifier ::= LocalModifier
| AccessModifier
| override
LocalModifier ::= abstract
| final
| sealed
| implicit
| lazy
AccessModifier ::= (private | protected) [AccessQualifier]
AccessQualifier ::= ‘[’ (id | this) ‘]’
Annotation ::= ‘@’ AnnotationExpr [nl]
AnnotationExpr ::= Constr [[nl] ‘{’ {NameValuePair} ‘}’]
NameValuePair ::= val id ‘=’ PrefixExpr
TemplateBody ::= [nl] ‘{’ [id [‘:’ Type] ‘=>’]
TemplateStat {semi TemplateStat} ‘}’
TemplateStat ::= Import
| {Annotation} {Modifier} Def
| {Annotation} {Modifier} Dcl
| Expr
|
Import ::= import ImportExpr {‘,’ ImportExpr}
ImportExpr ::= StableId ‘.’ (id | ‘_’ | ImportSelectors)
ImportSelectors ::= ‘{’ {ImportSelector ‘,’} (ImportSelector | ‘_’) ‘}’
ImportSelector ::= id [‘=>’ id | ‘=>’ ‘_’]
Dcl ::= val ValDcl
| var VarDcl
| def FunDcl
| type {nl} TypeDcl
ValDcl ::= ids ‘:’ Type
VarDcl ::= ids ‘:’ Type

FunDcl ::= FunSig [‘:’ Type]
FunSig ::= id [FunTypeParamClause] ParamClauses
TypeDcl ::= id [TypeParamClause] [‘>:’ Type] [‘<:’ Type]
Def ::= val PatDef
| var VarDef
| def FunDef
| type {nl} TypeDef
| TmplDef
PatDef ::= Pattern2 {‘,’ Pattern2} [‘:’ Type] ‘=’ Expr
VarDef ::= PatDef
| ids ‘:’ Type ‘=’ ‘_’
FunDef ::= FunSig ‘:’ Type ‘=’ Expr
| FunSig [nl] ‘{’ Block ‘}’
| this ParamClause ParamClauses
(‘=’ ConstrExpr | [nl] ConstrBlock)
TypeDef ::= id [TypeParamClause] ‘=’ Type
TmplDef ::= [case] class ClassDef
| [case] object ObjectDef
| trait TraitDef
ClassDef ::= id [TypeParamClause] {Annotation} [AccessModifier]
ClassParamClauses [requires AnnotType] ClassTemplateOpt
TraitDef ::= id [TypeParamClause] [requires AnnotType] TraitTemplateOpt
ObjectDef ::= id ClassTemplateOpt
ClassTemplateOpt ::= extends ClassTemplate | [[extends] TemplateBody]
TraitTemplateOpt ::= extends TraitTemplate | [[extends] TemplateBody]
ClassTemplate ::= [EarlyDefs] ClassParents [TemplateBody]
TraitTemplate ::= [EarlyDefs] TraitParents [TemplateBody]
ClassParents ::= Constr {with AnnotType}
TraitParents ::= AnnotType {with AnnotType}
Constr ::= AnnotType {ArgumentExprs}
EarlyDefs ::= ‘{’ [EarlyDef {semi EarlyDef}] ‘}’ with
EarlyDef ::= Annotations Modifiers PatDef
ConstrExpr ::= SelfInvocation
| ConstrBlock
ConstrBlock ::= ‘{’ SelfInvocation {semi BlockStat} ‘}’
SelfInvocation ::= this ArgumentExprs {ArgumentExprs}
TopStatSeq ::= TopStat {semi TopStat}
TopStat ::= {Annotation} {Modifier} TmplDef
| Import
| Packaging
|
Packaging ::= package QualId [nl] ‘{’ TopStatSeq ‘}’
CompilationUnit ::= [package QualId semi] TopStatSeq

*/
}
