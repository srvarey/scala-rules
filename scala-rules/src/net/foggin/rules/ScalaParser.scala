package net.foggin.rules;

case class Literal[T](value : T)

/** Rules based on Scala Language Specification 2.6.0 
 * (copied from Syntax Summary section)
 *
 * UNDER CONSTRUCTION!
 */
abstract class ScalaParser extends ScalaScanner {
  
  // TODO: allow {nl} within round and square
  def round[T](rule : Rule[T]) = token('(') -~ rule ~- token(')')
  def square[T](rule : Rule[T]) = token('[') -~ rule ~- token(']')
  def curly[T](rule : Rule[T]) = token('{') -~ rule ~- token('}')
  
  /** QualId ::= id {‘.’ id} */
  val qualId = id ~+~ dot
  
  /** ids ::= id {‘,’ id} */
  val ids = id ~+~ comma
  
  /** Path ::= StableId
   *    | [id ‘.’] this */
  lazy val path : Rule[Any] = stableId | (id ~- dot ?) ~ `this`
    
  /** StableId ::= id
   *    | Path ‘.’ id
   *    | [id ’.’] super [ClassQualifier] ‘.’ id */
  lazy val stableId : Rule[Any] = id | path ~- dot ~ id | (id ~- dot ?) ~ `super` ~ (classQualifier ?) ~- dot ~ id
  
  /** ClassQualifier ::= ‘[’ id ‘]’ */
  val classQualifier = square(id)
    
    
  /** Type ::= InfixType ‘=>’ Type
   *    | ‘(’ [‘=>’ Type] ‘)’ ‘=>’ Type
   *    | InfixType [ExistentialClause] */
  lazy val typeSpec : Rule[Any] = 
      infixType ~ `=>` ~ typeSpec |
        round(`=>` ~ typeSpec ?) ~ `=>` ~ typeSpec | // don't think this is right - should be multiple parameter types with optional lazy?
      infixType ~ existentialClause
    
  /** ExistentialClause ::= forSome ‘{’ ExistentialDcl {semi ExistentialDcl}} ‘}’ */
    // note typo above (double }})
  lazy val existentialClause = `forSome` ~  curly(existentialDcl ~+~ semi)
      
  /** ExistentialDcl ::= type TypeDcl | val ValDcl */
  lazy val existentialDcl = (
      `type` ~ typeDcl 
      |`val` ~ valDcl)
        
  /** InfixType ::= CompoundType {id [nl] CompoundType} */
  // should be able to use ~*~ operator here: 
  //   type1 infixType type2 ... => infixType[type1, type2] ...
  lazy val infixType = compoundType ~ (id ~- (nl?) ~ compoundType *)
          
  /** CompoundType ::= AnnotType {with AnnotType} [Refinement]
   *    | Refinement */
  lazy val compoundType = annotType ~+~ `with` ~ (refinement ?) |
      refinement
            
  /** AnnotType ::= {Annotation} SimpleType */
  lazy val annotType = (annotation *) ~ simpleType

  // left-recursive definition
  /** SimpleType ::= SimpleType TypeArgs
| SimpleType ‘#’ id
| StableId
| Path ‘.’ type
| ‘(’ Types [‘,’] ’)’ */
  lazy val simpleType = (stableId | path ~ dot ~ `type` | round(types ~ (comma?))) ~ (typeArgs | `#` ~ id *)

  /** TypeArgs ::= ‘[’ Types ‘]’ */
  lazy val typeArgs = square(types)

  /** Types ::= Type {‘,’ Type} */
  lazy val types = typeSpec ~+~ comma

  /** Refinement ::= [nl] ‘{’ RefineStat {semi RefineStat} ‘}’ */
  lazy val refinement = (nl?) -~ curly(refineStat ~+~ semi) 

  /** RefineStat ::= Dcl
| type TypeDef
| */
  lazy val refineStat = dcl | `type` ~ typeDef

  /** TypePat ::= Type */
  val typePat = typeSpec
  
  /** Ascription ::= ‘:’ CompoundType
| ‘:’ Annotation {Annotation}
| ‘:’ ‘_’ ‘*’ */
  lazy val ascription = `:` ~ (compoundType | (annotation *) | `_` ~ `*`)

  /** Expr ::= (Bindings | id) ‘=>’ Expr
| Expr1 */
  lazy val expr : Rule[Any] = (bindings | id) ~ `=>` ~ expr | expr1

  /** Expr1 ::= if ‘(’ Expr ‘)’ {nl} Expr [[semi] else Expr]
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
| PostfixExpr match ‘{’ CaseClauses ‘}’ */
  lazy val expr1 : Rule[Any] = (
      `if` ~ round(expr) ~ (nl*) ~ expr ~ ((semi?) ~ `else` ~ expr)
      | `while` ~ round(expr)  ~ (nl*) ~ expr
      | `try` ~ curly(block) ~ (`catch` ~ curly(caseClauses) ?) ~ (`finally` ~ expr)
      | `do` ~ expr ~ (semi?) ~ `while` ~ round(expr)
      | `for` ~ (round(enumerators) | curly(enumerators))  ~ (nl*) ~ (`yield`?) ~ expr
      | `throw` ~ expr
      | `return` ~ (expr?)
      | (simpleExpr ~ dot ?) ~ id ~ `=` ~ expr
      | simpleExpr1 ~ argumentExprs ~ `=` ~ expr
      | postfixExpr
      | postfixExpr ~ ascription
      | postfixExpr ~ `match`~ curly(caseClauses))

  /** PostfixExpr ::= InfixExpr [id [nl]] */
  lazy val postfixExpr = infixExpr ~ (id ~ (nl?) ?)

  /** InfixExpr ::= PrefixExpr
| InfixExpr id [nl] InfixExpr */
  lazy val infixExpr : Rule[Any] = prefixExpr | infixExpr ~ id ~ (nl?) ~ infixExpr

  /** PrefixExpr ::= [‘-’ | ‘+’ | ‘~’ | ‘!’] SimpleExpr */
  lazy val prefixExpr = ((plus | minus | bang | tilde) ?) ~ simpleExpr

  /** SimpleExpr ::= new (ClassTemplate | TemplateBody)
| BlockExpr
| SimpleExpr1 [‘_’] */
  lazy val simpleExpr = (
      `new` ~ (classTemplate | templateBody)
      | blockExpr
      | simpleExpr1 ~ `_`)

  /** 
SimpleExpr1 ::= Literal
| Path
| ‘_’
| ‘(’ [Exprs [‘,’]] ‘)’
| SimpleExpr ‘.’ id
| SimpleExpr TypeArgs
| SimpleExpr1 ArgumentExprs
| XmlExpr */
  lazy val simpleExpr1 : Rule[Any] = (
      literal
      | path
      | `_`
      | round(exprs ~ (comma?) ?)
      | simpleExpr ~ dot ~ id
      | simpleExpr ~ typeArgs
      | simpleExpr1 ~ argumentExprs
      | xmlExpr)
      
  // NB XmlExpr is not in the syntax summary of SLS 2.6.0
  def xmlExpr : Rule[Any]

  /** Exprs ::= Expr {‘,’ Expr} */
  lazy val exprs = expr ~+~ comma

  /** ArgumentExprs ::= ‘(’ [Exprs [‘,’]] ’)’
| [nl] BlockExpr */
  lazy val argumentExprs = round(exprs ~ (comma?) ?) | (nl?) ~ blockExpr

  /** BlockExpr ::= ‘{’ CaseClauses ‘}’
| ‘{’ Block ‘}’ */
  lazy val blockExpr = curly(caseClauses | block)

  /** Block ::= {BlockStat semi} [ResultExpr] */
  lazy val block : Rule[Any] = (blockStat ~ semi *) ~ (resultExpr?)

  /** BlockStat ::= Import
| [implicit] Def
| {LocalModifier} TmplDef
| Expr1
| */
  lazy val blockStat  : Rule[Any] = (
      importStat
      | (`implicit`?) ~ definition
      | (localModifier *) ~ tmplDef
      | expr1)

  /** ResultExpr ::= Expr1
| (Bindings | id ‘:’ CompoundType) ‘=>’ Block */
  lazy val resultExpr = expr1 | (bindings | id ~ `:` ~ compoundType) ~ `=>` ~ block

  /** Enumerators ::= Generator {semi Enumerator} */
  lazy val enumerators = generator ~ (semi ~ enumerator *)

  /** Enumerator ::= Generator
| Guard
| val Pattern1 ‘=’ Expr */
  lazy val enumerator = generator | guard | `val` ~ pattern1 ~ `=` ~ expr

  /** Generator ::= Pattern1 ‘<’
Expr [Guard] */
  lazy val generator = pattern1 ~ `<-` ~ expr ~ (guard?)

  /** CaseClauses ::= CaseClause { CaseClause } */
  lazy val caseClauses = caseClause+

  /** CaseClause ::= case Pattern [Guard] ‘=>’ Block */
  lazy val caseClause = `case` ~ pattern ~ (guard?) ~ `=>` ~ block

  /** Guard ::= ‘if’ PostfixExpr */
  lazy val guard = `if` ~ postfixExpr

  /** Pattern ::= Pattern1 { ‘|’ Pattern1 } */
  lazy val pattern = pattern1 ~+~ `|` 

  /** Pattern1 ::= varid ‘:’ TypePat
| ‘_’ ‘:’ TypePat
| Pattern2 */
  lazy val pattern1 = (
    varid ~ `:` ~ typePat
    | `_` ~ `:` ~ typePat
    | pattern2)

  /** Pattern2 ::= varid [‘@’ Pattern3]
| Pattern3 */
  lazy val pattern2 = varid ~ `@` ~ pattern3 | pattern3

  /** Pattern3 ::= SimplePattern
| SimplePattern { id [nl] SimplePattern } */
  // should be able to use ~*~ operator here: 
  lazy val pattern3 = simplePattern ~ (id ~- (nl?) ~ simplePattern *)

  /** SimplePattern ::= ‘_’
| varid
| Literal
| StableId
| StableId ‘(’ [Patterns [‘,’]] ‘)’
| StableId ‘(’ [Patterns ‘,’] ‘_’ ‘*’ ‘)’
| ‘(’ [Patterns [‘,’]] ‘)’
| XmlPattern */
  lazy val simplePattern : Rule[Any] = (
      varid
      | literal
      | stableId
      | stableId ~ round(patterns ~ (comma?) ?)
      | stableId ~ round((patterns ~ comma?) ~ `_` ~ `*`)
      | round(patterns ~ (comma?) ?)
      | xmlPattern)

  // NB XmlPattern is not in the syntax summary of SLS 2.6.0
  def xmlPattern : Rule[Any]

  /** Patterns ::= Pattern [‘,’ Patterns]
| ‘_’ * */
  lazy val patterns = pattern ~+~ comma | `_`

  /** TypeParamClause ::= ‘[’ VariantTypeParam {‘,’ VariantTypeParam} ‘]’ */
  lazy val typeParamClause = square(variantTypeParam ~+~ comma)

  /** FunTypeParamClause::= ‘[’ TypeParam {‘,’ TypeParam} ‘]’ */
  lazy val funTypeParamClause = square(typeParam ~+~ comma)

  /** VariantTypeParam ::= [‘+’ | ‘-’]
TypeParam */
  lazy val variantTypeParam = ((plus | minus) ?) ~ typeParam

  /** TypeParam ::= id [>: Type] [<: Type] [<% Type] */
  lazy val typeParam = id ~ (`>:` ~ typeSpec) ~ (`<:` ~ typeSpec) ~ (`<%` ~ typeSpec)

  /** ParamClauses ::= {ParamClause} [[nl] ‘(’ implicit Params ‘)’] */
  lazy val paramClauses = (paramClause*) ~ ((nl?) ~ round(`implicit` ~ params) ?)

  /** ParamClause ::= [nl] ‘(’ [Params] ’)’} */
    // typo above - extra }
  lazy val paramClause = (nl?) ~ round(params?)

  /** Params ::= Param {‘,’ Param} */
  lazy val params = param ~+~ comma

  /** Param ::= {Annotation} id [‘:’ ParamType] */
  lazy val param = (annotation*) ~ id ~ (`:` ~ paramType ?)

  /** ParamType ::= Type
| ‘=>’ Type
| Type ‘*’ */
  lazy val paramType = typeSpec ~ (`*`?) | `=>` ~ typeSpec 

  /** ClassParamClauses ::= {ClassParamClause}
[[nl] ‘(’ implicit ClassParams ‘)’] */
  lazy val classParamClauses = (classParamClause*) ~ ((nl?) ~ round(`implicit` ~ classParams) ?)

  /** ClassParamClause ::= [nl] ‘(’ [ClassParams] ’)’ */
  lazy val classParamClause = (nl?) ~ round(classParams?)

  /** ClassParams ::= ClassParam {‘’ ClassParam} */
  lazy val classParams = classParam ~+~ comma

  /** ClassParam ::= {Annotation} [{Modifier} (‘val’ | ‘var’)]
id [‘:’ ParamType] */
  lazy val classParam = (annotation*) ~ ((modifier*) ~ (`val` | `var`) ?) ~ id ~ (`:` ~ paramType ?)

  /** Bindings ::= ‘(’ Binding {‘,’ Binding ‘)’ */
  lazy val bindings = round(binding ~+~ comma)

  /** Binding ::= id [‘:’ Type] */
  lazy val binding = id ~ (`:` ~ typeSpec ?)

  /** Modifier ::= LocalModifier
| AccessModifier
| override */
  lazy val modifier = localModifier | accessModifier | `override`

  /** LocalModifier ::= abstract
| final
| sealed
| implicit
| lazy */
  lazy val localModifier = `final` | `sealed` | `implicit` | `lazy`

  /** AccessModifier ::= (private | protected) [AccessQualifier] */
  lazy val accessModifier = (`private` | `protected`) ~ (accessQualifier?)

  /** AccessQualifier ::= ‘[’ (id | this) ‘]’ */
  lazy val accessQualifier = square(id | `this`)

  /** Annotation ::= ‘@’ AnnotationExpr [nl] */
  lazy val annotation : Rule[Any] = `@` ~ annotationExpr ~ (nl?)

  /** AnnotationExpr ::= Constr [[nl] ‘{’ {NameValuePair} ‘}’] */
  lazy val annotationExpr = constr ~ ((nl?) ~ curly(nameValuePair*) ?)

  /** NameValuePair ::= val id ‘=’ PrefixExpr */
  lazy val nameValuePair = `val` ~ id ~ prefixExpr

  /** TemplateBody ::= [nl] ‘{’ [id [‘:’ Type] ‘=>’]
TemplateStat {semi TemplateStat} ‘}’ */
  lazy val templateBody : Rule[Any] = (nl?) ~ curly((id ~ (`:` ~ typeSpec ?) ~ `=>` ?) ~ (templateStat ~+~ semi))

  /** TemplateStat ::= Import
| {Annotation} {Modifier} Def
| {Annotation} {Modifier} Dcl
| Expr
| */
  lazy val templateStat = (
      importStat
      | (annotation*) ~ (modifier*) ~ definition
      | (annotation*) ~ (modifier*) ~ dcl
      | expr)

  /** Import ::= import ImportExpr {‘,’ ImportExpr} */
  lazy val importStat = `import` ~ (importExpr ~+~ comma)

  /** ImportExpr ::= StableId ‘.’ (id | ‘_’ | ImportSelectors) */
  lazy val importExpr = stableId ~ dot ~ (id | `_` | importSelectors)

  /** ImportSelectors ::= ‘{’ {ImportSelector ‘,’} (ImportSelector | ‘_’) ‘}’ */
  lazy val importSelectors = curly((importSelector ~- comma *) ~ (importSelector | `_`))

  /** ImportSelector ::= id [‘=>’ id | ‘=>’ ‘_’] */
  lazy val importSelector = id ~ (`=>` ~ (id | `_`) ?)

  /** Dcl ::= val ValDcl
| var VarDcl
| def FunDcl
| type {nl} TypeDcl */
  lazy val dcl = (
      `val` ~ valDcl
      | `var` ~ varDcl
      | `def` ~ funDcl
      | `type` ~ (nl*) ~ typeDcl)

  /** ValDcl ::= ids ‘:’ Type */
  lazy val valDcl = ids ~ `:` ~ typeSpec

  /** VarDcl ::= ids ‘:’ Type */
  lazy val varDcl = ids ~ `:` ~ typeSpec

  /** FunDcl ::= FunSig [‘:’ Type] */
  lazy val funDcl = funSig ~ (`:` ~ typeSpec ?)

  /** FunSig ::= id [FunTypeParamClause] ParamClauses */
  lazy val funSig = id ~ (funTypeParamClause?) ~ paramClauses

  /** TypeDcl ::= id [TypeParamClause] [‘>:’ Type] [‘<:’ Type] */
  lazy val typeDcl = id ~ (typeParamClause?) ~ (`>:` ~ typeSpec ?) ~ (`<:` ~ typeSpec ?)

  /** Def ::= val PatDef
| var VarDef
| def FunDef
| type {nl} TypeDef
| TmplDef */
  lazy val definition = (
      `val` ~ patDef
      | `var` ~ varDef
      | `def` ~ funDef
      | `type` ~ (nl*) ~ typeDef
      | tmplDef)


  /** PatDef ::= Pattern2 {‘,’ Pattern2} [‘:’ Type] ‘=’ Expr */
  lazy val patDef = pattern2 ~+~ pattern2 ~ (`:` ~ typeSpec ?) ~ `=` ~ expr

  /** VarDef ::= PatDef
| ids ‘:’ Type ‘=’ ‘_’ */
  lazy val varDef = patDef | ids ~ `:` ~ typeSpec ~ `=` ~ `_`

  /** FunDef ::= FunSig ‘:’ Type ‘=’ Expr
| FunSig [nl] ‘{’ Block ‘}’
| this ParamClause ParamClauses
(‘=’ ConstrExpr | [nl] ConstrBlock) */
  lazy val funDef = (
      funSig ~ `:` ~ typeSpec ~ `=` ~ expr
      | funSig ~ (nl?) ~ curly(block)
      | `this` ~ paramClause ~ paramClauses ~ (`=` ~ constrExpr | (nl?) ~ constrBlock))

  /** TypeDef ::= id [TypeParamClause] ‘=’ Type */
  lazy val typeDef = id ~ (typeParamClause?) ~ `=` ~ typeSpec

  /** TmplDef ::= [case] class ClassDef
| [case] object ObjectDef
| trait TraitDef */
  lazy val tmplDef = (
      (`case`?) ~ `class` ~ classDef
      | (`case`?) ~ `object` ~ objectDef
      | `trait` ~ traitDef)

  /** ClassDef ::= id [TypeParamClause] {Annotation} [AccessModifier]
ClassParamClauses [requires AnnotType] ClassTemplateOpt */
  lazy val classDef = (id 
      ~ (typeParamClause?) 
      ~ (annotation*) 
      ~ (accessModifier?)
      ~ classParamClauses
      ~ (`requires` ~ annotType ?)
      ~ classTemplateOpt)

  /** TraitDef ::= id [TypeParamClause] [requires AnnotType] TraitTemplateOpt */
  lazy val traitDef = (id 
      ~ (typeParamClause?) 
      ~ (`requires` ~ annotType ?)
      ~ traitTemplateOpt)

  /** ObjectDef ::= id ClassTemplateOpt */
  lazy val objectDef = id ~ classTemplateOpt

  /** ClassTemplateOpt ::= extends ClassTemplate | [[extends] TemplateBody] */
  lazy val classTemplateOpt = `extends` ~ classTemplate | ((`extends`?) ~ templateBody ?)

  /** TraitTemplateOpt ::= extends TraitTemplate | [[extends] TemplateBody] */
  lazy val traitTemplateOpt = `extends` ~ traitTemplate | ((`extends`?) ~ templateBody ?)

  /** ClassTemplate ::= [EarlyDefs] ClassParents [TemplateBody] */
  lazy val classTemplate = (earlyDefs?) ~ classParents ~ (templateBody?)

  /** TraitTemplate ::= [EarlyDefs] TraitParents [TemplateBody] */
  lazy val traitTemplate = (earlyDefs?) ~ traitParents ~ (templateBody?)

  /** ClassParents ::= Constr {with AnnotType} */
  lazy val classParents = constr ~ (`with` ~ annotType *)

  /** TraitParents ::= AnnotType {with AnnotType} */
  lazy val traitParents = annotType ~ (`with` ~ annotType *)

  /** Constr ::= AnnotType {ArgumentExprs} */
  lazy val constr = annotType ~ (argumentExprs)

  /** EarlyDefs ::= ‘{’ [EarlyDef {semi EarlyDef}] ‘}’ with */
  lazy val earlyDefs = curly(earlyDef ~+~ semi ?)

  /** EarlyDef ::= Annotations Modifiers PatDef */
    // NB 'Annotations' and 'Modifiers' not defined in SLS 2.6.0 Syntax Summary
  lazy val earlyDef = (annotation*) ~ (modifier*) ~ patDef

  /** ConstrExpr ::= SelfInvocation
| ConstrBlock */
  lazy val constrExpr = selfInvocation | constrBlock

  /** ConstrBlock ::= ‘{’ SelfInvocation {semi BlockStat} ‘}’ */
  lazy val constrBlock = curly(selfInvocation ~ (semi ~ blockStat *))

  /** SelfInvocation ::= this ArgumentExprs {ArgumentExprs} */
  lazy val selfInvocation = `this` ~ (argumentExprs+)

  /** TopStatSeq ::= TopStat {semi TopStat} */
  lazy val topStatSeq = topStat ~+~ semi

  /** TopStat ::= {Annotation} {Modifier} TmplDef
| Import
| Packaging
| */
  lazy val topStat : Rule[Any] = (
      (annotation*) ~ (modifier*) ~ tmplDef
      | importStat
      | packaging)

  /** Packaging ::= package QualId [nl] ‘{’ TopStatSeq ‘}’ */
  lazy val packaging = `package` ~ qualId ~ (nl?) ~ curly(topStatSeq)

  /** CompilationUnit ::= [package QualId semi] TopStatSeq */
  def compilationUnit = (`package` ~ qualId ~ semi ?) ~ topStatSeq

}
