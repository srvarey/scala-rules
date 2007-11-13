package net.foggin.rules;

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
  
  /** Note left-recursive definition.
  *
  * Path ::= StableId 
  *    | [id ‘.’] this 
  *
  * StableId ::= id
  *    | Path ‘.’ id
  *    | [id ’.’] super [ClassQualifier] ‘.’ id
  *
  * ClassQualifier ::= ‘[’ id ‘]’
  */
  val pathElement : Rule[PathElement] = (id ^^ Name
      | `super` -~ (square(id) ?) ^^ Super
      | `this` ^^^ This)
    
  val path = pathElement ~+~ dot
    
  /** StableId is a Path ending in an id */
  val stableId = path filter (_ last match {
    case Name(_) => true
    case _ => false
  })
  
  /** Type ::= InfixType ‘=>’ Type
   *    | ‘(’ [‘=>’ Type] ‘)’ ‘=>’ Type     // don't think this is right - should be multiple parameter types with optional lazy?
   *    | InfixType [ExistentialClause] */
  lazy val typeSpec : Rule[Type] = (
      infixType ~- `=>` ~ typeSpec ^^ { 
        case a ~ b => FunctionType(List(a -> false), b) }
      | round(`=>` -~ typeSpec ?) ~- `=>` ~ typeSpec ^^ { 
        case Some(a) ~ b => FunctionType(List(a ->true), b)
        case None ~ b => FunctionType(List(), b) }
      | (compoundType >> infixType) ~ (existentialClause?) ^^ { 
        case a ~ b => a })
    
  /** ExistentialClause ::= forSome ‘{’ ExistentialDcl {semi ExistentialDcl}} ‘}’ */
    // note typo above (double }})
  lazy val existentialClause : Rule[Any] = failure //`forSome` ~  curly(existentialDcl ~+~ semi)
      
  /** ExistentialDcl ::= type TypeDcl | val ValDcl */
  lazy val existentialDcl = (
      `type` ~ typeDcl 
      |`val` ~ valDcl)
        
  /** InfixType ::= CompoundType {id [nl] CompoundType} */
  lazy val infixType : Rule[Type] = compoundType >> infixType
  def infixType(left : Type) : Rule[Type] = (
      (id ~- (nl?) ~ compoundType ^^ { case id ~ right => InfixType(id, left, right) }) >> infixType
      | success(left))
      
  /** CompoundType ::= AnnotType {with AnnotType} [Refinement]
   *    | Refinement */
  lazy val compoundType : Rule[Type] = (
      annotType ~++ (`with` -~ annotType *) ~ refinement ^^ { case as ~ r => CompoundType(as, Some(r)) }
      | annotType ~++ (`with` -~ annotType +) ~ (refinement ?) ^^ { case as ~ optR => CompoundType(as, optR) }
      | refinement ^^ { r => CompoundType(Nil, Some(r)) }
      | annotType)
      
    
  /** AnnotType ::= {Annotation} SimpleType */
  lazy val annotType = ((annotation +) ~ simpleType ^~^ AnnotatedType
      | simpleType)

  /** SimpleType ::= SimpleType TypeArgs
   *    | SimpleType ‘#’ id
   *    | StableId
   *    | Path ‘.’ type
   *    | ‘(’ Types [‘,’] ’)’ 
   *
   * Note left-recursive definition above.
   */
  val simpleType = (
      path ~- dot ~- `type` ^^ SingletonType
      | stableId ^^ { list => { val Name(id) :: rest = list.reverse; TypeDesignator(rest.reverse, id) }}
      | round(types ~- (comma?)) ^^ TupleType) >> typeArgsOrProjection

  def typeArgsOrProjection(simpleType : SimpleType) : Rule[SimpleType] = (
      parameterizedType(simpleType) >> typeArgsOrProjection
      | projection(simpleType) >> typeArgsOrProjection
      | success(simpleType))
      
  def parameterizedType(simpleType : SimpleType) = square(types) ^^ (ParameterizedType(simpleType, _))
  def projection(simpleType : SimpleType) = `#` -~ id ^^ (TypeProjection(simpleType, _))
      
  /** TypeArgs ::= ‘[’ Types ‘]’ */
  lazy val typeArgs = square(types)

  /** Types ::= Type {‘,’ Type} */
  lazy val types : Rule[List[Type]] = typeSpec ~+~ comma

  /** Refinement ::= [nl] ‘{’ RefineStat {semi RefineStat} ‘}’ */
  lazy val refinement : Rule[Refinement] = failure //(nl?) -~ curly(refineStat ~+~ semi) 

  /** RefineStat ::= Dcl
| type TypeDef
| */
  lazy val refineStat = dcl | `type` ~ typeDef

  /** TypePat ::= Type */
  val typePat = typeSpec
  
  /** Expr ::= (Bindings | id) ‘=>’ Expr
   *     | Expr1 */
  lazy val expr : Rule[Expression] = (
      (bindings | id ^^ { id => List(Binding(id, None)) }) ~- `=>` ~ expr ^~^ FunctionExpression
      | expr1)

  /** Expr1 ::= if ‘(’ Expr ‘)’ {nl} Expr [[semi] else Expr]
   *     | while ‘(’ Expr ‘)’ {nl} Expr
   *     | try ‘{’ Block ‘}’ [catch ‘{’ CaseClauses ‘}’] [finally Expr]
   *     | do Expr [semi] while ‘(’ Expr ’)’
   *     | for (‘(’ Enumerators ‘)’ | ‘{’ Enumerators ‘}’) {nl} [yield] Expr
   *     | throw Expr
   *     | return [Expr]
   *     | [SimpleExpr ‘.’] id ‘=’ Expr
   *     | SimpleExpr1 ArgumentExprs ‘=’ Expr
   *     | PostfixExpr
   *     | PostfixExpr Ascription
   *     | PostfixExpr match ‘{’ CaseClauses ‘}’ 
   *
   *  Ascription ::= ‘:’ CompoundType
   *     | ‘:’ Annotation {Annotation}
   *     | ‘:’ ‘_’ ‘*’ 
   */
  lazy val expr1 : Rule[Expression] = (
      `if` -~ round(expr) ~- (nl*) ~ expr ~ ((semi?) -~ `else` -~ expr ?) ^~~^ IfExpression
      | `while` -~ round(expr)  ~- (nl*) ~ expr ^~^ WhileExpression
      | `try` -~ curly(block) ~ (`catch` -~ curly(caseClauses) ?) ~ (`finally` -~ expr ?) ^~~^ TryCatchFinally
      | `do` -~ expr ~- (semi?) ~- `while` ~ round(expr) ^~^ DoExpression
      | `for` -~ (round(enumerators) | curly(enumerators))  ~- (nl*) ~ ((`yield`?) ^^ (_.isDefined)) ~ expr ^~~^ ForComprehension
      | `throw` -~ expr ^^ Throw
      | `return` -~ (expr?) ^^ Return
      | assignment
      | postfixExpr ~- `:` ~ compoundType ^~^ TypedExpression
      | postfixExpr ~- `:` ~ (annotation*) ^~^ AnnotatedExpression
      | postfixExpr ~- `:` ~- `_` ~- `*` ^^ VarArgExpression
      | postfixExpr ~- `match`~ curly(caseClauses) ^~^ MatchExpression
      | postfixExpr)
      
  lazy val assignment = simpleExpr ~- `=` ~ expr >> {
    case Name(id) ~ value => success(SimpleAssignment(id, value))
    case DotExpression(expr, Name(id)) ~ value => success(DotAssignment(expr, id, value))
    case ApplyExpression(expr, args) ~ value => success(Update(expr, args, value))
    case _ => failure
  }
    
  /** PostfixExpr ::= InfixExpr [id [nl]] */
  lazy val postfixExpr = (
      infixExpr ~ id ~- (nl?) ^~^ PostfixExpression
      | infixExpr)
      
  def infixId(start : Rule[Char]) = token((start&) -~ id)
      
  def infix(operators : List[Rule[String]]) : Rule[Expression] = {
    val head :: tail = operators
    val next = if (tail == Nil) prefixExpr else infix(tail)
    val op : Rule[(Expression, Expression) => Expression] = head ^^ { id => InfixExpression(id, _, _) }
    next ~*~ op
  }
  
  /** Infix operators listed from lowest to hightest precedence */
  val operators : List[Rule[String]] = List(
      infixId(letter),
      infixId('|'),
      infixId('^'),
      infixId('&'),
      infixId(choice("<>")),
      infixId(choice("=!")),
      infixId(':'),
      infixId(choice("+-")),
      infixId(choice("*/%")),
      infixId(opChar - choice("|^&<>=!:+-*/%")))

  /** InfixExpr ::= PrefixExpr | InfixExpr id [nl] InfixExpr */
  val infixExpr = infix(operators)

  /** PrefixExpr ::= [‘-’ | ‘+’ | ‘~’ | ‘!’] SimpleExpr */
  lazy val prefixExpr = ((plus | minus | bang | tilde) ~ simpleExpr ^~^ PrefixExpression
      | simpleExpr)

  /** SimpleExpr ::= new (ClassTemplate | TemplateBody)
   *     | BlockExpr
   *     | SimpleExpr1 [‘_’] */
  lazy val simpleExpr = (
      //`new` ~ (classTemplate | templateBody)
      blockExpr
      | simpleExpr1 ~- `_` ^^ Unapplied
      | simpleExpr1)

  /** SimpleExpr1 ::= Literal
   *     | Path
   *     | ‘_’
   *     | ‘(’ [Exprs [‘,’]] ‘)’
   *     | SimpleExpr ‘.’ id
   *     | SimpleExpr TypeArgs
   *     | SimpleExpr1 ArgumentExprs
   *     | XmlExpr 
   *
   * Note left-recursive definition above.  
   */
  lazy val simpleExpr1 : Rule[Expression] = (
      `_` ^^^ Underscore
      | literal 
      | pathElement
      | tupleExpr
      | xmlExpr) >> simpleExpr1Rest
      
  def simpleExpr1Rest(expr : Expression) : Rule[Expression] = (
      dot -~ (pathElement ^^ (DotExpression(expr, _))) >> simpleExpr1Rest
      | (typeArgs ^^ (ExpressionTypeArgs(expr, _))) >> simpleExpr1Rest
      | (argumentExprs ^^ (ApplyExpression(expr, _))) >> simpleExpr1Rest
      | success(expr))
      
  // NB XmlExpr is not in the syntax summary of SLS 2.6.0
  val xmlExpr = failure

  lazy val tupleExpr = round(exprs ~- (comma?) | success(Nil)) ^^ TupleExpression
  
  /** Exprs ::= Expr {‘,’ Expr} */
  lazy val exprs = expr ~+~ comma

  /** ArgumentExprs ::= ‘(’ [Exprs [‘,’]] ’)’ | [nl] BlockExpr */
  lazy val argumentExprs = round(exprs ~- (comma?) | success(Nil)) | (nl?) -~ blockExpr ^^ (List(_))

  /** BlockExpr ::= ‘{’ CaseClauses ‘}’ | ‘{’ Block ‘}’ */
  lazy val blockExpr : Rule[Expression] = curly(caseClauses | block)

  /** Block ::= {BlockStat semi} [ResultExpr] */
  lazy val block : Rule[Block] = (blockStat ~- semi *) ~ (resultExpr?) ^~^ Block
 
  /** BlockStat ::= Import
   *     | [implicit] Def
   *     | {LocalModifier} TmplDef
   *     | Expr1
   */
  lazy val blockStat  : Rule[Statement] = (
      importStat
      //| (`implicit`?) ~ definition
      //| (localModifier *) ~ tmplDef
      | expr1)

  /** ResultExpr ::= Expr1 | (Bindings | id ‘:’ CompoundType) ‘=>’ Block */
  lazy val resultExpr = ((bindings | singleIdBinding ) ~- `=>` ~ block ^~^ FunctionExpression
      | expr1)

  lazy val singleIdBinding = id ~- `:` ~ compoundType ^^ { case id ~ typeSpec => List(Binding(id, Some(typeSpec))) }
  
  /** Enumerators ::= Generator {semi Enumerator} */
  lazy val enumerators = generator ~++ (semi -~ enumerator *)

  /** Enumerator ::= Generator
   *     | Guard
   *     | val Pattern1 ‘=’ Expr */
  lazy val enumerator : Rule[Enumerator] = (
      generator 
      | guard ^^ Guard 
      | (`val` -~ pattern1 ~- `=`) ~ expr ^~^ ValEnumerator)

  /** Generator ::= Pattern1 ‘<-’ Expr [Guard] */
  lazy val generator = pattern1 ~- `<-` ~ expr ~ (guard?) ^~~^ Generator

  /** CaseClauses ::= CaseClause { CaseClause } */
  lazy val caseClauses = (caseClause+) ^^ CaseClauses

  /** CaseClause ::= case Pattern [Guard] ‘=>’ Block */
  lazy val caseClause = `case` -~ pattern ~ (guard?) ~- `=>` ~ block ^~~^ CaseClause

  /** Guard ::= ‘if’ PostfixExpr */
  lazy val guard = `if` -~ postfixExpr

  /** Pattern ::= Pattern1 { ‘|’ Pattern1 } */
  lazy val pattern = (pattern1 ~++ (`|` -~ pattern1 +)  ^^ OrPattern
      | pattern1)

  /** Pattern1 ::= varid ‘:’ TypePat
   *     | ‘_’ ‘:’ TypePat
   *     | Pattern2 */
  lazy val pattern1 = (
    varid ~- `:` ~ typePat ^~^ TypedVariablePattern
    | `_` -~ `:`-~ typePat ^^ TypePattern
    | pattern2)

  /** Pattern2 ::= varid [‘@’ Pattern3]
   *     | Pattern3 */
  lazy val pattern2 = (
      (varid ~- `@` ~ pattern3) ^~^ AtPattern
      | pattern3)

  /** Pattern3 ::= SimplePattern | SimplePattern { id [nl] SimplePattern } */
  lazy val pattern3 : Rule[Expression] = (
      simplePattern ~ (((id -`|`) ~- (nl?) ~ simplePattern ^^ { case a ~ b => (a, b) })+) ^~^ InfixPattern
      | simplePattern)

  /** SimplePattern ::= ‘_’
   *     | varid
   *     | Literal
   *     | StableId
   *     | StableId ‘(’ [Patterns [‘,’]] ‘)’
   *     | StableId ‘(’ [Patterns ‘,’] ‘_’ ‘*’ ‘)’
   *     | ‘(’ [Patterns [‘,’]] ‘)’
   *     | XmlPattern
   */
  lazy val simplePattern : Rule[Expression] = (
      `_` ^^^ Underscore
      | varid ~- !dot ^^ VariablePattern
      | literal
      | stableId ~ round(patterns ~- (comma?)) ^^ { case a ~ b => StableIdPattern(a, Some(b), false) }
      | stableId ~ round((pattern ~- comma *) ~- `_` ~- `*`) ^^ { case a ~ b => StableIdPattern(a, Some(b), true) }
      | stableId  ^^ (StableIdPattern(_, None, false))
      | round(patterns ~- (comma?)) ^^ TupleExpression
      | xmlPattern)

  // NB XmlPattern is not in the syntax summary of SLS 2.6.0
  val xmlPattern = failure

  /** Patterns ::= Pattern [‘,’ Patterns]
   *     | ‘_’ * */
  lazy val patterns = pattern ~+~ comma | success(Nil)

  /** TypeParamClause ::= ‘[’ VariantTypeParam {‘,’ VariantTypeParam} ‘]’ */
  lazy val typeParamClause = square(variantTypeParam ~+~ comma)

  /** FunTypeParamClause::= ‘[’ TypeParam {‘,’ TypeParam} ‘]’ */
  lazy val funTypeParamClause = square(typeParam ~+~ comma)

  val variance = (plus ^^^ Covariant
      | minus ^^^ Contravariant
      | success(Invariant))
  
  /** VariantTypeParam ::= [‘+’ | ‘-’] TypeParam */
  lazy val variantTypeParam = variance ~ typeParam ^^ { 
      case v ~ TypeParameter(id, lower, upper, view) => VariantTypeParameter(id, lower, upper, view, v) }
      

  /** TypeParam ::= id [>: Type] [<: Type] [<% Type] */
  lazy val typeParam = id ~ (`>:` -~ typeSpec ?) ~ (`<:` -~ typeSpec ?) ~ (`<%` -~ typeSpec ?) ^~~~^ TypeParameter

  /** ParamClause ::= [nl] ‘(’ [Params] ’)’} */
    // typo above - extra }
  lazy val paramClause = (nl?) -~ round(optParams)

  lazy val optParams = params | success(Nil)
  
  /** Params ::= Param {‘,’ Param} */
  lazy val params = param ~+~ comma

  /** Param ::= {Annotation} id [‘:’ ParamType] */
  lazy val param = (annotation*) ~ id ~ (`:` -~ paramType ?) ^^ { 
    case annotations ~ id ~ None => Parameter(id, false, None, false, annotations)
    case annotations ~ id ~ Some((byName, typeSpec, varArgs)) => Parameter(id, byName, Some(typeSpec), varArgs, annotations)
  }

  /** ParamType ::= Type | ‘=>’ Type | Type ‘*’ */
  lazy val paramType : Rule[(Boolean, Type, Boolean)] = (
      `=>` -~ typeSpec ^^ { t => (true, t, false) }
      | typeSpec ~- `*` ^^ { t => (false, t, true) }
      | typeSpec ^^ { t => (false, t, false) })

  /** ClassParamClauses ::= {ClassParamClause} [[nl] ‘(’ implicit ClassParams ‘)’] */
  lazy val classParamClauses = (classParamClause*) ~ ((nl?) ~ round(`implicit` ~ classParams) ?)

  /** ClassParamClause ::= [nl] ‘(’ [ClassParams] ’)’ */
  lazy val classParamClause = (nl?) ~ round(classParams?)

  /** ClassParams ::= ClassParam {‘’ ClassParam} */
  lazy val classParams = classParam ~+~ comma

  /** ClassParam ::= {Annotation} [{Modifier} (‘val’ | ‘var’)] id [‘:’ ParamType] */
  lazy val classParam = (annotation*) ~ ((modifier*) ~ (`val` | `var`) ?) ~ id ~ (`:` ~ paramType ?)

  /** Bindings ::= ‘(’ Binding {‘,’ Binding ‘)’ */
  lazy val bindings = round(binding ~+~ comma)

  /** Binding ::= id [‘:’ Type] */
  lazy val binding = id ~ (`:` -~ typeSpec ?) ^~^ Binding

  /** Modifier ::= LocalModifier
   *     | AccessModifier
   *     | override 
   */
  lazy val modifier : Rule[Modifier] = localModifier | accessModifier | `override`^^^ Override

  /** LocalModifier ::= abstract
   *     | final
   *     | sealed
   *     | implicit
   *     | lazy 
   */
  lazy val localModifier : Rule[Modifier]  = (`abstract` ^^^ Abstract
      | `final` ^^^ Final
      | `sealed` ^^^ Sealed
      | `implicit` ^^^ Implicit
      | `lazy` ^^^ Lazy)

  /** AccessModifier ::= (private | protected) [AccessQualifier] */
  lazy val accessModifier : Rule[Modifier] = (`private` -~ (accessQualifier?) ^^ Private
      | `protected`-~ (accessQualifier?) ^^ Protected) 

  /** AccessQualifier ::= ‘[’ (id | this) ‘]’ */
  lazy val accessQualifier = square(id ^^ Name | `this` ^^^ This)

  /** Annotation ::= ‘@’ AnnotationExpr [nl] */
  lazy val annotation : Rule[Annotation] = failure //`@` ~ annotationExpr ~ (nl?)

  /** AnnotationExpr ::= Constr [[nl] ‘{’ {NameValuePair} ‘}’] */
  lazy val annotationExpr = constr ~ ((nl?) ~ curly(nameValuePair*) ?)

  /** NameValuePair ::= val id ‘=’ PrefixExpr */
  lazy val nameValuePair = `val` ~ id ~ prefixExpr

  /** TemplateBody ::= [nl] ‘{’ [id [‘:’ Type] ‘=>’] TemplateStat {semi TemplateStat} ‘}’ */
  lazy val templateBody = (nl?) -~ curly(selfType ~ (templateStat ~+~ semi)) ^~~^ TemplateBody

  lazy val selfType = ((id ^^ Some[String]) ~ (`:` -~ typeSpec ?)  ~- `=>`
      | (`this` ^^^ None) ~ (`:` -~ typeSpec ^^ Some[Type]) ~- `=>` 
      | success(None) ~ success(None))
  
  /** TemplateStat ::= Import
   *     | {Annotation} {Modifier} Def
   *     | {Annotation} {Modifier} Dcl
   *     | Expr
   */
  lazy val templateStat = (importStat
      | (annotation*) ~ (modifier*) ~ definition ^~~^ AnnotatedDefinition
      | (annotation*) ~ (modifier*) ~ dcl ^~~^ AnnotatedDeclaration
      | expr)

  /** Import ::= import ImportExpr {‘,’ ImportExpr} */
  lazy val importStat : Rule[Statement] = `import` -~ (importExpr ~+~ comma) ^^ ImportStatement

  /** ImportExpr ::= StableId ‘.’ (id | ‘_’ | ImportSelectors) */
  lazy val importExpr : Rule[Import] = (
      stableId ~- dot ~ importSelectors ^^ { case path ~ selectors => Import(path, selectors) }
      | stableId ~- dot ~ wildcardImportSelector ^^ { case path ~ selector => Import(path, List(selector)) }
      | path >> importId)

  def importId(path : List[PathElement]) = path.reverse match {
    case Name(id) :: (rest @ Name(_) :: _) => success(Import(rest.reverse, List(ImportSelector(id, None))))
    case _ => failure
  }
         
  /** ImportSelectors ::= ‘{’ {ImportSelector ‘,’} (ImportSelector | ‘_’) ‘}’ */
  lazy val importSelectors : Rule[List[ImportSelector]] = curly((importSelector ~- comma *) ~ (importSelector | wildcardImportSelector)) ^^ { case ss ~ s => (s :: ss.reverse).reverse }

  /** ImportSelector ::= id [‘=>’ id | ‘=>’ ‘_’] */
  lazy val importSelector = id ~ (`=>` -~ (id | `_`) ?) ^~^ ImportSelector
  lazy val wildcardImportSelector = `_` ^^^ ImportSelector("_", None)

  /** Dcl ::= val ValDcl
   *     | var VarDcl
   *     | def FunDcl
   *     | type {nl} TypeDcl
   */
  lazy val dcl = (`val` -~ valDcl
      | `var` -~ varDcl
      | `def` -~ funDcl
      | `type` -~ (nl*) -~ typeDcl)

  /** ValDcl ::= ids ‘:’ Type */
  lazy val valDcl = ids ~- `:` ~ typeSpec ^~^ ValDeclaration

  /** VarDcl ::= ids ‘:’ Type */
  lazy val varDcl = ids ~- `:` ~ typeSpec ^~^ VarDeclaration

  /** FunDcl ::= FunSig [‘:’ Type] */
  lazy val funDcl = funSig ~ (`:` -~ typeSpec ?)  ^~~~~^ FunctionDeclaration

  /** FunSig ::= id [FunTypeParamClause] ParamClauses 
   *
   *  ParamClauses ::= {ParamClause} [[nl] ‘(’ implicit Params ‘)’] 
   */
  lazy val funSig = id ~ (funTypeParamClause?) ~ (paramClause*) ~ (implicitParamClause?)

  lazy val implicitParamClause = (nl?) -~ round(`implicit` -~ params)
  
  /** TypeDcl ::= id [TypeParamClause] [‘>:’ Type] [‘<:’ Type] */
  lazy val typeDcl = id ~ (typeParamClause?) ~ (`>:` -~ typeSpec ?) ~ (`<:` -~ typeSpec ?) ^~~~^ TypeDeclaration


  /** Def ::= val PatDef
   *     | var VarDef
   *     | def FunDef
   *     | type {nl} TypeDef
   *     | TmplDef 
   *
   *  VarDef ::= PatDef | ids ‘:’ Type ‘=’ ‘_’ 
   *
   *   FunDef ::= FunSig ‘:’ Type ‘=’ Expr   // NB think :Type should be optional here
   *     | FunSig [nl] ‘{’ Block ‘}’
   *     | this ParamClause ParamClauses (‘=’ ConstrExpr | [nl] ConstrBlock) 
   *
   *  ParamClauses ::= {ParamClause} [[nl] ‘(’ implicit Params ‘)’] 
   */
  lazy val definition : Rule[Definition] = (
      `val` -~ patDef ^~~^ ValPatternDefinition
      | `var` -~ patDef ^~~^ VarPatternDefinition
      | `var` -~ ids ~ (`:` -~ typeSpec ~- `=` ~- `_`) ^~^ VarDefaultDefinition
      | `def` -~ funSig ~ (`:` -~ typeSpec ?) ~ (`=` -~ expr) ^~~~~~^ FunctionDefinition
      | `def` -~ funSig ~ ((nl?) -~ curly(block)) ^~~~~^ ProcedureDefinition
      | `def` -~ `this` -~ (paramClause+) ~ (implicitParamClause?) ~ (`=` -~ constrExpr | (nl?) -~ constrBlock) ^~~^ ConstructorDefinition
      | `type` -~ (nl*) -~ typeDef ^~~^ TypeDefinition
      | tmplDef)

  /** ConstrExpr ::= SelfInvocation | ConstrBlock */
  lazy val constrExpr : Rule[ConstructorExpression] = (selfInvocation ^^ (ConstructorExpression(_, Nil)) 
      | constrBlock)

  /** ConstrBlock ::= ‘{’ SelfInvocation {semi BlockStat} ‘}’ */
  lazy val constrBlock = curly(selfInvocation ~ (semi -~ blockStat *)) ^~^ ConstructorExpression

  /** SelfInvocation ::= this ArgumentExprs {ArgumentExprs} */
  lazy val selfInvocation = `this` -~ (argumentExprs+)


  /** PatDef ::= Pattern2 {‘,’ Pattern2} [‘:’ Type] ‘=’ Expr */
  lazy val patDef = (pattern2 ~+~ comma) ~ (`:` -~ typeSpec ?) ~ (`=` -~ expr)

  /** TypeDef ::= id [TypeParamClause] ‘=’ Type */
  lazy val typeDef = id ~ (typeParamClause?) ~ (`=` -~ typeSpec)

  /** TmplDef ::= [case] class ClassDef
   *     | [case] object ObjectDef
   *     | trait TraitDef 
   */
  lazy val tmplDef = (
      //(`case`?) ~ `class` ~ classDef
      //| (`case`?) ~ `object` ~ objectDef
      //| 
        `trait` -~ traitDef)

  /** ClassDef ::= id [TypeParamClause] {Annotation} [AccessModifier] ClassParamClauses [requires AnnotType] ClassTemplateOpt */
  lazy val classDef = (id 
      ~ (typeParamClause?) 
      ~ (annotation*) 
      ~ (accessModifier?)
      ~ classParamClauses
      //~ (`requires` ~ annotType ?)
      ~ classTemplateOpt)

  /** TraitDef ::= id [TypeParamClause] [requires AnnotType] TraitTemplateOpt */
  lazy val traitDef = (id 
      ~ (typeParamClause?) 
      //~ (`requires` ~ annotType ?)
      ~ traitTemplateOpt) ^~~^ TraitDefinition

  /** ObjectDef ::= id ClassTemplateOpt */
  lazy val objectDef = id ~ classTemplateOpt

  /** ClassTemplateOpt ::= extends ClassTemplate | [[extends] TemplateBody] */
  lazy val classTemplateOpt = `extends` ~ classTemplate | ((`extends`?) ~ templateBody ?)

  /** TraitTemplateOpt ::= extends TraitTemplate | [[extends] TemplateBody] */
  lazy val traitTemplateOpt = (`extends` -~ traitTemplate
      | ((`extends`?) -~ templateBody ?) ^^ (TraitTemplate(None, Nil, _)))

  /** ClassTemplate ::= [EarlyDefs] ClassParents [TemplateBody] */
  lazy val classTemplate = (earlyDefs?) ~ annotType ~ (argumentExprs*) ~ (`with` -~ annotType *) ~ (templateBody?)

  /** TraitTemplate ::= [EarlyDefs] TraitParents [TemplateBody] */
  lazy val traitTemplate = (earlyDefs?) ~ traitParents ~ (templateBody?)  ^~~^ TraitTemplate

  /** ClassParents ::= Constr {with AnnotType} */
  //lazy val classParents = constr ~ (`with` -~ annotType *)

  /** TraitParents ::= AnnotType {with AnnotType} */
  lazy val traitParents = annotType ~++ (`with` -~ annotType *)

  /** Constr ::= AnnotType {ArgumentExprs} */
  lazy val constr = annotType ~ (argumentExprs*)

  /** EarlyDefs ::= ‘{’ [EarlyDef {semi EarlyDef}] ‘}’ with */
  lazy val earlyDefs = curly(earlyDef ~+~ semi | success(Nil)) ~- `with`

  /** EarlyDef ::= Annotations Modifiers PatDef */
    // NB 'Annotations' and 'Modifiers' not defined in SLS 2.6.0 Syntax Summary
  lazy val earlyDef = (annotation*) ~ (modifier*) ~ (`val` -~ patDef ^~~^ ValPatternDefinition) ^~~^ AnnotatedDefinition

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
  lazy val compilationUnit = (`package` ~ qualId ~ semi ?) ~ topStatSeq

}
