package net.foggin.rules.scala

import Character._

object ScalaParser {
  
  val reserved = _root_.scala.collection.immutable.Set(
      "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final",
      "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null", "object",
      "override", "package", "private", "protected", "requires", "return", "sealed", "super", "this", 
      "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield",
      "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2")

  /** Reserved ids that can terminate a statement */
  val endStatements = _root_.scala.collection.immutable.Set(
      "this", "null", "true", "false", "return", "type", "_")
    
  /** Reserved ids that cannot start a statement 
   *
   * Note: "case" cannot start statement unless followed by "class" or "object" 
   */
  val cannotStartStatements = _root_.scala.collection.immutable.Set(
      "case", "catch", "else", "extends", "finally", "forSome", "match", "requires", "with", "yield",
      "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "\u21D2")

  def isReserved(id : String) = reserved.contains(id)
  def isNotReserved(id : String) = !reserved.contains(id)
  def canStartStatement(id : String) = !cannotStartStatements.contains(id)
  def canEndStatement(id : String) = endStatements.contains(id)
}

import ScalaParser._


/** A parser for Scala source.
  *
  * @author Andrew Foggin, based on Scala Language Specification
 */
abstract class ScalaParser[T <: Input[Char, T] with Memoisable[T]] extends Scanner with MemoisableRules {
  type Context = ScalaInput[T]
  
  /** Treat a symbol as a rule that matches the corresponding keyword */
  implicit def symbolToKeyword(symbol : Symbol) : Rule[String] = keyword filter (_ == symbol.name)
   
  /** rule that sets multiple statements status and returns the previous value */
  def multiple(allow : Boolean) = read(_.multipleStatementsAllowed) ~- update(_.multipleStatementsAllowed = allow)
          
  def singleStatement[T](rule : Rule[T]) = for (s <- multiple(false); t <- rule; _ <- multiple(s)) yield t
  def multipleStatements[T](rule : Rule[T]) = for (s <- multiple(true); t <- rule; _ <- multiple(s)) yield t
          
  val multipleStatementsAllowed = predicate(_.multipleStatementsAllowed)
  val lastTokenCanEndStatement = predicate(_.lastTokenCanEndStatement)
  
  def token[T](key : String, rule : Rule[T], f : T => Boolean) : Rule[T] = memo(key, !nl -~ (newline | space | comment *) -~ rule >> tokenCanEndStatement(f))
  def tokenCanEndStatement[T](f : T => Boolean)(t : T) = update(_.lastTokenCanEndStatement = f(t)) -~ success(t)
  def endToken[T](key : String, rule : Rule[T]) : Rule[T] = token(key, rule, { t : T => true })
  
  val space = choice(" \t")
  
  lazy val startStatement = (space | comment | newline *) ~- (
      choice("({") 
      | literal 
      | id 
      | reservedId.filter(canStartStatement) 
      | 'case ~ ('class | 'object))
  
  lazy val nl = memo("nl", multipleStatementsAllowed 
      -~ lastTokenCanEndStatement 
      -~ (space | comment *) 
      -~ newline 
      ~- (startStatement &))

  val delimiter = token("delimiter", choice(";.,()[]{}"), delimCanEndStatement)
  def delimCanEndStatement(ch : Char) = ")]}" contains ch
  def delim (char : Char) : Rule[Char] = delimiter.filter(_ == char)
    
  lazy val semi = delim(';') | (nl+)
  lazy val dot = delim('.')
  lazy val comma = delim(',')
  
  def round[T](rule : Rule[T]) = delim('(') -~ singleStatement(rule) ~- delim(')')
  def square[T](rule : Rule[T]) = delim('[') -~ singleStatement(rule) ~- delim(']')
  def curly[T](rule : Rule[T]) = delim('{') -~ multipleStatements(rule) ~- delim('}')
  
  def idToken(string : String) : Rule[String] = (plainId | reservedId) filter (_ == string)
  
  lazy val `_` = idToken("_")
  lazy val `:` = idToken(":")
  lazy val `=` = idToken("=")
  lazy val `=>` = idToken("=>") | idToken("\u21D2")
  lazy val `<-` = idToken("<-")
  lazy val `<:` = idToken("<:")
  lazy val `<%` = idToken("<%")
  lazy val `>:` = idToken(">:")
  lazy val `#` = idToken("#")
  lazy val `@` = idToken("@")
  
  lazy val `|` = idToken("|")
  lazy val `*` = idToken("*")
  
  lazy val plus = idToken("+")
  lazy val minus = idToken("-")
  lazy val bang = idToken("!")
  lazy val tilde = idToken("~")
  
  val decimalDigit = ('0' to '9') ^^ (_ - 48L)
  def decimal(n : Long) = decimalDigit ^^ (n * 10 + _)
  def decimalN(n : Long) : Rule[Long] = decimal(n) >> decimalN | success(n)
  
  val octalDigit = decimalDigit.filter(_ < 8)
  def octal(n : Long) = octalDigit ^^ (n * 8 + _)
  def octalN(n : Long) : Rule[Long] = octal(n) >> octalN | success(n)
  
  val hexDigit = decimalDigit | ('A' to 'F') ^^ (_ - 55L) | ('a' to 'f') ^^ (_ - 87L)
  def hex(n : Long) = hexDigit ^^ (n * 16 + _)
  def hexN(n : Long) : Rule[Long] = hex(n) >> hexN | success(n)

  val unicodeEscape = "\\u" -~ hexDigit >> hex >> hex >> hex ^^ { _.asInstanceOf[Char] }
  val octalEscape = '\\' -~ octalDigit >> octal >> octal ^^ { _.asInstanceOf[Char] }
 
  val charEscapeSeq = '\\' -~ ( choice("\"\'\\")
      | 'b' -^ '\b' | 't' -^ '\t' | 'n' -^ '\n' | 'f' -^ '\f' | 'r' -^ '\r') 

  val anyChar = unicodeEscape | octalEscape | item
  val printableChar = !choice("\b\t\n\f\r") -~ anyChar
  
  def unicode(category : Int) = anyChar filter (getType(_) == category)
  
  val letter = choice("$_") | (anyChar filter isLetter)
  val digit = anyChar filter isDigit
  val lower = anyChar filter isLowerCase
  val idChar = letter | digit
  val opChar = unicode(MATH_SYMBOL) | unicode(OTHER_SYMBOL) | choice("!#%&*+-/:<=>?@\\^|~")
  lazy val idRest : Rule[List[Char]] = ('_' ~++ (opChar+)) ~- !idChar | !idChar -^ Nil | idChar ~++ idRest
  
  val quoteId = endToken("quoteId", '`' -~ (printableChar +~- '`') ^^ toString)
  val plainId = endToken("plainId", notReserved((letter ~++ idRest | (opChar+)) ^^ toString))
  val id = quoteId | plainId
  val varId = plainId filter { id => id.charAt(0) isLowerCase }
  val rightOp = plainId filter { id => id.endsWith(":") }
  
  val keyword = token[String]("keyword", reserved(letter ~++ idRest ^^ toString), canEndStatement)
  val reservedOp = token[String]("reservedOp", reserved((opChar+) ^^ toString), canEndStatement)
  val reservedId = keyword | reservedOp
  
  def reserved(rule : Rule[String]) = rule filter isReserved
  def notReserved(rule : Rule[String]) = rule filter isNotReserved
  
  val nonZero = decimalDigit filter (_ > 0)
  val hexNumeral = "0x" -~ hexDigit >> hexN
  val octalNumeral = '0' -~ octalDigit >> octalN
  val decimalNumeral = nonZero >> decimalN | '0' -^ 0L
  
  val integerLiteral = (hexNumeral | octalNumeral | decimalNumeral) ~ (choice("Ll")-?) ~- !idChar >> {
    case value ~ false => success(IntegerLiteral(value.asInstanceOf[Int]))
    case value ~ true => success(LongLiteral(value))
  }
  
  val intPart = decimalNumeral ^^ (_ toString) | ""
  val floatPart = ('.' ~++ (('0' to '9')*) ^^ toString) | ""
  val exponentPart = choice("Ee") ~ ("+" | "-" | "") ~ intPart ^^ { case e ~ s ~ n => e + s + n } | ""

  val floatLiteral = intPart ~ floatPart ~ exponentPart ~ (choice("FfDd")?) >> {
    case "" ~ "" ~ _ ~ _ => failure
    case "" ~ "." ~ _ ~ _ => failure
    case _ ~ "" ~ "" ~ None => failure
    case i ~ f ~ e ~ Some('F' | 'f') => success(FloatLiteral((i + f + e).toFloat))
    case i ~ f ~ e ~ _ => success(DoubleLiteral((i + f + e).toDouble))
  }

  val charElement = charEscapeSeq | printableChar
  val characterLiteral = '\'' -~ (charElement - '\'') ~- '\'' ^^ CharacterLiteral
  val stringLiteral = ('\"' -~ charElement *~- '\"' | "\"\"\"" -~ anyChar *~- "\"\"\"") ^^ toString ^^ StringLiteral
  val symbolLiteral = '\'' -~ plainId ^^ Symbol ^^ SymbolLiteral
  
  // note multi-line comments can nest
  lazy val multiLineComment : Rule[String] = ("/*" -~ (multiLineComment | anyChar) *~- "*/") ^^ toString
  val singleLineComment : Rule[String] = "//" -~ (item - newline *) ^^ toString
  lazy val comment = memo("comment", singleLineComment | multiLineComment)
  
  lazy val literal : Rule[Literal] = endToken("literal", 
      'null -~ !idChar -^ Null
      | 'true -~ !idChar -^ True
      | 'false -~ !idChar -^ False
      | integerLiteral 
      | characterLiteral 
      | stringLiteral 
      | symbolLiteral
      | floatLiteral)
    
  val xmlNameStart = (elem('_')
      | unicode(LOWERCASE_LETTER) // Ll
      | unicode(UPPERCASE_LETTER) // Lu
      | unicode(OTHER_LETTER ) // Lo
      | unicode(TITLECASE_LETTER) //Lt
      | unicode(LETTER_NUMBER)) // Nl
      
  val xmlNameChar = (xmlNameStart | choice(":.-")
      | unicode(COMBINING_SPACING_MARK) // Mc
      | unicode(ENCLOSING_MARK) // Me
      | unicode(NON_SPACING_MARK) // Mn
      | unicode(MODIFIER_LETTER) // Lm
      | unicode(DECIMAL_DIGIT_NUMBER )) // Nd
      
  val xmlName = xmlNameStart ~++ (xmlNameChar*) ^^ toString
  val xmlS = choice(" \t\r\n")+
  val xmlComment = "<!--" -~ anyChar *~- "-->" ^^ toString ^^ XMLComment
  val reference = "&amp;" -^ '&' | "&lt;" -^ '<' | "&gt;" -^ '>' | "&apos;" -^ '\'' | "&quot;" -^ '"'
    
  def statements[T](rule : Rule[T]) = curly(rule +/semi)
 
  val qualId = id+/dot
  val ids = id+/comma
    
  lazy val path = pathElement+/dot
  lazy val pathElement : Rule[PathElement] = (id ^^ Name
      | 'super -~ (square(id) ?) ^^ Super
      | 'this -^ This)
    
  /** StableId is a Path ending in an id */
  lazy val stableId = path filter (_ last match {
    case Name(_) => true
    case _ => false
  })
  
  lazy val typeSpec : Rule[Type] = functionType | existentialType | infixType
  lazy val existentialType = infixType ~- 'forSome ~ statements(typeDcl | valDcl) ^~^ ExistentialType
  lazy val functionType = (functionParameters | simpleFunctionParameter) ~- `=>` ~ typeSpec ^~^ FunctionType
  lazy val functionParameters = round(parameterType*/comma).filter(checkParamTypes)
  lazy val simpleFunctionParameter = infixType ^^ { t => List(ParameterType(false, t, false)) }
  lazy val parameterType = (`=>` -?) ~ typeSpec ~ (`*` -?) ^~~^ ParameterType
  
  /** Checks that only the last parameter in a list is repeated (*) */
  private def checkParamTypes(list : List[ParameterType]) : Boolean = list match {
    case ParameterType(_, _, true) :: rest :: Nil => false
    case first :: rest => checkParamTypes(rest)
    case Nil => true
  }
  
  lazy val infixType : Rule[Type] = rightAssociativeInfixType | compoundType >> optInfixType
  lazy val rightAssociativeInfixType : Rule[Type] = compoundType ~ rightOp ~- (nl?) ~ (rightAssociativeInfixType | compoundType) ^~~^ InfixType
  def optInfixType(left : Type) : Rule[Type] = infixType(left) >> optInfixType | success(left)
  def infixType(left : Type) = id ~- (nl?) ~ compoundType ^^ { case id ~ right => InfixType(left, id, right) }
      
  lazy val compoundType : Rule[Type] = (refinement 
      | annotType ~- !('with | refinement) 
      | annotType ~ ('with -~ annotType *) ~ (refinement?) ^~~^ CompoundType)
  lazy val refinement : Rule[Refinement] = (nl?) -~ statements(dcl | typeDef) ^^ Refinement
  
  // TODO: report issue with AnnotType definition in syntax summary
  lazy val annotType = simpleType ~ (annotation+) ^~^ AnnotatedType | simpleType

  lazy val simpleType = (path ~- dot ~- 'type ^^ SingletonType
      | stableId ^^ { list => { val Name(id) :: rest = list.reverse; TypeDesignator(rest.reverse, id) }}
      | round(types ~- (comma?)) ^^ TupleType) >> typeArgsOrProjection
      
  def typeArgsOrProjection(simpleType : Type) : Rule[Type] = (
      (typeArgs ^^ ParameterizedType(simpleType)) >> typeArgsOrProjection
      | `#` -~ (id ^^ TypeProjection(simpleType)) >> typeArgsOrProjection
      | success(simpleType))
      
  lazy val typeArgs = square(types)
  lazy val types = typeSpec+/comma

  lazy val expr : Rule[Expression] = (bindings | untypedIdBinding) ~- `=>` ~ expr ^~^ FunctionExpression | expr1

  lazy val expr1 : Rule[Expression] = (
      'if -~ round(expr) ~- (nl*) ~ expr ~ ((semi?) -~ 'else -~ expr ?) ^~~^ IfExpression
      | 'while -~ round(expr)  ~- (nl*) ~ expr ^~^ WhileExpression
      | 'try -~ curly(block) ~ ('catch -~ curly(caseClauses) ?) ~ ('finally -~ expr ?) ^~~^ TryCatchFinally
      | 'do -~ expr ~- (semi?) ~- 'while ~ round(expr) ^~^ DoExpression
      | 'for -~ (round(enumerators) | curly(enumerators))  ~- (nl*) ~ (('yield?) ^^ (_.isDefined)) ~ expr ^~~^ ForComprehension
      | 'throw -~ expr ^^ Throw
      | 'return -~ (expr?) ^^ Return
      | assignment
      | postfixExpr ~- `:` ~ compoundType ^~^ TypedExpression
      | postfixExpr ~- `:` ~ (annotation+) ^~^ AnnotatedExpression
      | postfixExpr ~- `:` ~- `_` ~- `*` ^^ VarArgExpression
      | postfixExpr ~- 'match ~ curly(caseClauses) ^~^ MatchExpression
      | postfixExpr)
      
  lazy val assignment = simpleExpr ~- `=` ~ expr >>? {
    case Name(id) ~ value => success(SimpleAssignment(id, value))
    case DotExpression(expr, Name(id)) ~ value => success(DotAssignment(expr, id, value))
    case ApplyExpression(expr, args) ~ value => success(Update(expr, args, value))
  }
    
  lazy val postfixExpr = infixExpr ~ id ~- (nl?) ^~^ PostfixExpression | infixExpr
      
  /** InfixExpr ::= PrefixExpr | InfixExpr id [nl] InfixExpr */
  lazy val infixExpr = infix(operators)
  
  def infix(operators : List[Rule[(Expression, Expression) => Expression]]) : Rule[Expression] = {
    val op :: tail = operators
    val next = if (tail == Nil) prefixExpr else infix(tail)
    next ~*~ op
  }
  
  def infixId(choices : String) : Rule[String] = id filter { string => choices contains (string.charAt(0)) }
  def infixOp(rule : Rule[String]) : Rule[(Expression, Expression) => Expression] = rule ~- (nl?) ^^ { id => InfixExpression(id, _, _) }
      
  /** Infix operators in list from lowest to highest precedence */
  lazy val operators : List[Rule[(Expression, Expression) => Expression]] = List[Rule[String]](
      infixId("_$") | id filter(_.charAt(0).isLetter),
      infixId("|"),
      infixId("^"),
      infixId("&"),
      infixId("<>"),
      infixId("=!"),
      infixId(":"),
      infixId("+-"),
      infixId("*%/"),
      otherOp) map infixOp
      
  lazy val otherOp = id filter { string => 
        val first = string.charAt(0)
        !first.isLetter && !"_$|^&<>=!:+-*%/".contains(first)
  }

  lazy val prefixExpr = (plus | minus | bang | tilde) ~ simpleExpr ^~^ PrefixExpression | simpleExpr

  lazy val simpleExpr = ('new -~ (classTemplate | templateBody) ^^ InstanceCreation
      | blockExpr
      | simpleExpr1 ~- `_` ^^ Unapplied
      | simpleExpr1)

  lazy val simpleExpr1 : Rule[Expression] = (`_` -^ Underscore
      | literal
      | xmlExpr
      | pathElement
      | tupleExpr) >> simpleExpr1Rest
      
  def simpleExpr1Rest(expr : Expression) : Rule[Expression] = (
      dot -~ (pathElement ^^ (DotExpression(expr, _))) >> simpleExpr1Rest
      | (typeArgs ^^ (ExpressionTypeArgs(expr, _))) >> simpleExpr1Rest
      | (argumentExprs ^^ (ApplyExpression(expr, _))) >> simpleExpr1Rest
      | success(expr))
      
  lazy val xmlExpr = token("xmlExpr", (xmlElement+) ^^ NodeList, { t : Any => true })
  lazy val xmlElement = '<' -~ xmlName ~ (attribute*) ~- (xmlS?) >~> xmlElementRest
  def xmlElementRest(name : String, attributes : List[Attribute]) : Rule[XMLElement] = ("/>" -^ None
      | '>' -~ (xmlContent  ^^ Some[Expression]) ~- endElement(name)) ^^ XMLElement(name, attributes)
  def endElement(name : String) = ("</" -~ xmlName ~- (xmlS?) ~- '>') filter (_ == name)
  lazy val xmlContent : Rule[Expression] = (xmlElement | xmlComment | charData | scalaExpr *) ^^ NodeList // | cdataSect | pi 

  val attributeName = xmlS -~ xmlName ~- '='
  val attributeValue : Rule[Expression] = quoted('"') | quoted('\'') | scalaExpr
  def quoted(ch : Char) = ch -~ (reference | anyChar - choice("<&")) *~- ch ^^ toString ^^ StringLiteral
  val scalaExpr = '{' -~ singleStatement(expr) ~- delim('}')
  val attribute = attributeName ~ attributeValue ^~^ Attribute
  val charData = ("{{" -^ '{' | reference | anyChar - ("]]>" | '{' | '<' | '&') +) ^^ toString ^^ TextNode
  
  lazy val tupleExpr = round(exprs ~- (comma?) | nil) ^^ TupleExpression
  lazy val exprs = expr +/comma
  lazy val argumentExprs = round(exprs ~- (comma?) | nil) | (nl?) -~ blockExpr ^^ (List(_))

  lazy val blockExpr : Rule[Expression] = curly(caseClauses | block)
  lazy val block : Rule[Block] = ((blockStat ~- semi *) ~ resultExpr ^^ { case s ~ r => s ::: r :: Nil } | nil) ^^ Block
  lazy val blockStat  : Rule[Statement] = (importStat
      | 'implicit -~ definition ^^ ImplicitDefinition
      | definition
      | nil ~ (localModifier*) ~ tmplDef ^~~^ AnnotatedDefinition // TODO: check - shouldn't annotations be allowed?
      | expr1)

  lazy val resultExpr = (bindings | singleIdBinding ) ~- `=>` ~ block ^~^ FunctionExpression | expr1
  lazy val bindings = round(binding +/comma)
  lazy val binding = id ~ (`:` -~ typeSpec ?) ^~^ Binding
  lazy val singleIdBinding = id ~ (`:` -~ compoundType ?) ^~^ Binding ^^ { List(_) }
  lazy val untypedIdBinding = id ~ none ^~^ Binding ^^ { List(_) }
  
  lazy val enumerators = generator ~++ (semi -~ enumerator *)
  lazy val generator = pattern1 ~- `<-` ~ expr ~ (guard?) ^~~^ Generator
  lazy val enumerator : Rule[Enumerator] = (generator 
      | guard ^^ Guard 
      | ('val -~ pattern1 ~- `=`) ~ expr ^~^ ValEnumerator)
  lazy val guard = 'if -~ postfixExpr

  lazy val caseClauses = (caseClause+) ^^ CaseClauses
  lazy val caseClause = 'case -~ singleStatement(pattern ~ (guard?)) ~- `=>` ~ block ^~~^ CaseClause

  lazy val pattern = pattern1 ~*~ (`|` -^ OrPattern _)
  lazy val pattern1 = (varId ~- `:` ~ typeSpec ^~^ TypedVariablePattern
      | `_` -~ `:` -~ typeSpec ^^ TypePattern
      | pattern2)
  lazy val pattern2 = ((varId ~- `@` ~ pattern3) ^~^ AtPattern
      | pattern3)

  lazy val pattern3 : Rule[Expression] = infixPattern(operators)
      
  def infixPattern(operators : List[Rule[(Expression, Expression) => Expression]]) : Rule[Expression] = {
    val op :: tail = operators
    val next = if (tail == Nil) simplePattern else infixPattern(tail)
    next ~*~ (op - `|`)
  }
 
  lazy val simplePattern : Rule[Expression] = (`_` -^ Underscore
      | varId ~- !dot ^^ VariablePattern
      | literal
      | xmlPattern
      | stableId ~ round(patterns ~- (comma?)) ^^ { case a ~ b => StableIdPattern(a, Some(b), false) }
      | stableId ~ round((pattern ~- comma *) ~- `_` ~- `*`) ^^ { case a ~ b => StableIdPattern(a, Some(b), true) }
      | stableId  ^^ (StableIdPattern(_, None, false))
      | round(patterns ~- (comma?)) ^^ TupleExpression)

  lazy val xmlPattern = token("xmlPattern", '<' -~ xmlName ~- (xmlS?) >> xmlPatternRest, { t : Any => true })
  
  def xmlPatternRest(name : String) : Rule[XMLPattern] = ("/>" -^ None
      | '>' -~ xmlPatternContent ~- endElement(name)) ^^ XMLPattern(name)
      
  lazy val xmlPatternContent = (xmlPattern | xmlComment | charData | scalaPattern *) ^^ NodeList ^^ Some[Expression]  // | cdataSect | pi 

  lazy val scalaPattern = '{' -~ singleStatement(patterns ^^ TupleExpression) ~- delim('}')

  /** Patterns ::= Pattern [‘,’ Patterns] | ‘_’ ‘*’  */
  lazy val patterns = (pattern +/comma | success(Nil))

  /** TypeParamClause ::= ‘[’ VariantTypeParam {‘,’ VariantTypeParam} ‘]’ */
  lazy val typeParamClause = square(variantTypeParam +/comma)

  /** FunTypeParamClause::= ‘[’ TypeParam {‘,’ TypeParam} ‘]’ */
  lazy val funTypeParamClause = square(typeParam +/comma)

  lazy val variance = (plus -^ Covariant
      | minus -^ Contravariant
      | success(Invariant))
  
  /** VariantTypeParam ::= [‘+’ | ‘-’] TypeParam */
  lazy val variantTypeParam = variance ~ typeParam ^~^ VariantTypeParameter

  /** TypeParam ::= id [>: Type] [<: Type] [<% Type] */
  lazy val typeParam = id ~ (`>:` -~ typeSpec ?) ~ (`<:` -~ typeSpec ?) ~ (`<%` -~ typeSpec ?) ^~~~^ TypeParameter

  /** ParamClause ::= [nl] ‘(’ [Params] ’)’ */
  lazy val paramClause = (nl?) -~ round(optParams)

  lazy val optParams = params | success(Nil)
  
  /** Params ::= Param {‘,’ Param} */
  lazy val params = param +/comma

  /** Param ::= {Annotation} id [‘:’ ParamType] */
  lazy val param = (annotation*) ~ id ~ (paramType?) ^~~^ Parameter

  /** ParamType ::= Type | ‘=>’ Type | Type ‘*’ */
  lazy val paramType = `:` -~ (`=>`-?) ~ typeSpec ~ (`*`-?) ^~~^ ParameterType

  /** ClassParamClauses ::= {ClassParamClause} [[nl] ‘(’ implicit ClassParams ‘)’] */
  lazy val classParamClauses = (classParamClause*) ~ ((nl?) -~ round('implicit -~ classParams) ?) ^~^ ClassParamClauses

  /** ClassParamClause ::= [nl] ‘(’ [ClassParams] ’)’ */
  lazy val classParamClause = (nl?) -~ round(classParams | success(Nil))

  /** ClassParams ::= ClassParam {‘’ ClassParam} */
  lazy val classParams = classParam +/comma

  /** ClassParam ::= {Annotation} [{Modifier} (‘val’ | ‘var’)] id [‘:’ ParamType] */
  lazy val classParam = (annotation*) ~ (classParamModifiers?) ~ id ~ (paramType ?) ^~~~^ ClassParameter

  lazy val classParamModifiers = ((modifier*) ~- 'val ^^ ValParameterModifiers 
      | (modifier*) ~- 'var ^^ VarParameterModifiers)

  /** Modifier ::= LocalModifier
   *     | AccessModifier
   *     | override 
   */
  lazy val modifier : Rule[Modifier] = localModifier | accessModifier | 'override -^ Override

  /** LocalModifier ::= abstract
   *     | final
   *     | sealed
   *     | implicit
   *     | lazy 
   */
  lazy val localModifier : Rule[Modifier]  = ('abstract -^ Abstract
      | 'final -^ Final
      | 'sealed -^ Sealed
      | 'implicit -^ Implicit
      | 'lazy -^ Lazy)

  /** AccessModifier ::= (private | protected) [AccessQualifier] */
  lazy val accessModifier : Rule[Modifier] = ('private -~ (accessQualifier?) ^^ Private
      | 'protected-~ (accessQualifier?) ^^ Protected) 

  /** AccessQualifier ::= ‘[’ (id | this) ‘]’ */
  lazy val accessQualifier = square(id ^^ Name | 'this -^ This)

  lazy val annotation : Rule[Annotation] = `@` -~ annotationExpr ~- (nl?)
  lazy val annotationExpr = constr ~ ((nl?) -~ curly(nameValuePair*/semi) | nil) ^~~^ Annotation
  lazy val nameValuePair = 'val -~ id ~- `=` ~ prefixExpr ^~^ Pair[String, Expression]

  /** TemplateBody ::= [nl] ‘{’ [id [‘:’ Type] ‘=>’] TemplateStat {semi TemplateStat} ‘}’ */
  lazy val templateBody = (nl?) -~ curly(selfType ~- (nl*) ~ (templateStat +/semi)) ^~~^ TemplateBody

  lazy val selfType = ((id ^^ Some[String]) ~ (`:` -~ typeSpec ?)  ~- `=>`
      | ('this -^ None) ~ (`:` -~ typeSpec ^^ Some[Type]) ~- `=>` 
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
  lazy val importStat : Rule[Statement] = 'import -~ (importExpr +/comma) ^^ ImportStatement

  /** ImportExpr ::= StableId ‘.’ (id | ‘_’ | ImportSelectors) */
  def importExpr : Rule[Import] = (
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
  lazy val wildcardImportSelector = `_` -^ ImportSelector("_", None)

  /** Dcl ::= val ValDcl
   *     | var VarDcl
   *     | def FunDcl
   *     | type {nl} TypeDcl
   * ValDcl ::= ids ‘:’ Type
   * VarDcl ::= ids ‘:’ Type
   * FunDcl ::= FunSig [‘:’ Type]
   * TypeDcl ::= id [TypeParamClause] [‘>:’ Type] [‘<:’ Type] 
   */
  lazy val dcl = (valDcl | varDcl | funDcl | typeDcl)
  lazy val valDcl = 'val -~ ids ~- `:` ~ typeSpec ^~^ ValDeclaration
  lazy val varDcl = 'var -~ ids ~- `:` ~ typeSpec ^~^ VarDeclaration
  lazy val funDcl = 'def -~ funSig ~ (`:` -~ typeSpec ?)  ^~~~~^ FunctionDeclaration
  lazy val typeDcl = 'type -~ (nl*) -~ id ~ (typeParamClause?) ~ (`>:` -~ typeSpec ?) ~ (`<:` -~ typeSpec ?) ^~~~^ TypeDeclaration

  /** FunSig ::= id [FunTypeParamClause] ParamClauses 
   *
   *  ParamClauses ::= {ParamClause} [[nl] ‘(’ implicit Params ‘)’] 
   */
  lazy val funSig = id ~ (funTypeParamClause?) ~ (paramClause*) ~ (implicitParamClause?)

  lazy val implicitParamClause = (nl?) -~ round('implicit -~ params)
  
  /** Def ::= val PatDef
   *     | var VarDef
   *     | def FunDef
   *     | type {nl} TypeDef
   *     | TmplDef 
   *
   *  VarDef ::= PatDef | ids ‘:’ Type ‘=’ ‘_’ 
   *
   *   FunDef ::= FunSig [‘:’ Type] ‘=’ Expr 
   *     | FunSig [nl] ‘{’ Block ‘}’
   *     | this ParamClause ParamClauses (‘=’ ConstrExpr | [nl] ConstrBlock) 
   *
   *  ParamClauses ::= {ParamClause} [[nl] ‘(’ implicit Params ‘)’] 
   */
  lazy val definition : Rule[Definition] = (
      'val -~ patDef ^~~^ ValPatternDefinition
      | 'var -~ patDef ^~~^ VarPatternDefinition
      | 'var -~ ids ~ (`:` -~ typeSpec ~- `=` ~- `_`) ^~^ VarDefaultDefinition
      | 'def -~ funSig ~ (`:` -~ typeSpec ?) ~ (`=` -~ expr) ^~~~~~^ FunctionDefinition
      | 'def -~ funSig ~ ((nl?) -~ curly(block)) ^~~~~^ ProcedureDefinition
      | 'def -~ 'this -~ (paramClause+) ~ (implicitParamClause?) ~ (`=` -~ constrExpr | (nl?) -~ constrBlock) ^~~^ ConstructorDefinition
      | typeDef
      | tmplDef)

  /** ConstrExpr ::= SelfInvocation | ConstrBlock */
  lazy val constrExpr : Rule[ConstructorExpression] = (selfInvocation ^^ (ConstructorExpression(_, Nil)) 
      | constrBlock)

  /** ConstrBlock ::= ‘{’ SelfInvocation {semi BlockStat} ‘}’ */
  lazy val constrBlock = curly(selfInvocation ~ (semi -~ blockStat *)) ^~^ ConstructorExpression

  /** SelfInvocation ::= this ArgumentExprs {ArgumentExprs} */
  lazy val selfInvocation = 'this -~ (argumentExprs+)


  /** PatDef ::= Pattern2 {‘,’ Pattern2} [‘:’ Type] ‘=’ Expr */
  lazy val patDef = (pattern2 +/comma) ~ (`:` -~ typeSpec ?) ~ (`=` -~ expr)

  /** TypeDef ::= id [TypeParamClause] ‘=’ Type */
  lazy val typeDef = 'type -~ (nl*) -~ id ~ (typeParamClause?) ~ (`=` -~ typeSpec) ^~~^ TypeDefinition

  /** TmplDef ::= [case] class ClassDef
   *     | [case] object ObjectDef
   *     | trait TraitDef 
   */
  lazy val tmplDef = ('class -~ classDef
      | 'case -~ 'class -~ classDef ^^ CaseClassDefinition
      | 'object -~ objectDef
      | 'case-~ 'object -~ objectDef ^^ CaseObjectDefinition
      | 'trait -~ traitDef)

  /** ClassDef ::= id [TypeParamClause] {Annotation} [AccessModifier] ClassParamClauses ClassTemplateOpt */
  lazy val classDef = (id 
      ~ (typeParamClause?) 
      ~ (annotation*) 
      ~ (accessModifier?)
      ~ classParamClauses
      ~ classTemplateOpt) ^~~~~~^ ClassDefinition

  /** TraitDef ::= id [TypeParamClause] TraitTemplateOpt */
  lazy val traitDef = (id 
      ~ (typeParamClause?) 
      ~ traitTemplateOpt) ^~~^ TraitDefinition

  /** ObjectDef ::= id ClassTemplateOpt */
  lazy val objectDef = id ~ classTemplateOpt ^~^ ObjectDefinition

  /** ClassTemplateOpt ::= extends ClassTemplate | [[extends] TemplateBody] */
  lazy val classTemplateOpt = ('extends -~ classTemplate 
      | (('extends ?) -~ templateBody ?) ^^ (ClassTemplate(None, None, Nil, Nil, _)))

  /** TraitTemplateOpt ::= extends TraitTemplate | [[extends] TemplateBody] */
  lazy val traitTemplateOpt = ('extends -~ traitTemplate
      | (('extends?) -~ templateBody ?) ^^ (TraitTemplate(None, Nil, _)))

  /** ClassTemplate ::= [EarlyDefs] ClassParents [TemplateBody] */
  lazy val classTemplate = ((earlyDefs?) 
      ~ (annotType ^^ Some[Type]) 
      ~ (argumentExprs*) 
      ~ ('with -~ annotType *) 
      ~ (templateBody?)) ^~~~~^ ClassTemplate

  /** TraitTemplate ::= [EarlyDefs] TraitParents [TemplateBody] */
  lazy val traitTemplate = (earlyDefs?) ~ traitParents ~ (templateBody?)  ^~~^ TraitTemplate

  /** ClassParents ::= Constr {with AnnotType} */
  //def classParents = constr ~ ("with" -~ annotType *)

  /** TraitParents ::= AnnotType {with AnnotType} */
  lazy val traitParents = annotType ~++ ('with -~ annotType *)

  /** Constr ::= AnnotType {ArgumentExprs} */
  lazy val constr = annotType ~ (argumentExprs*)

  /** EarlyDefs ::= ‘{’ [EarlyDef {semi EarlyDef}] ‘}’ with */
  lazy val earlyDefs = curly(earlyDef */semi) ~- 'with

  /** EarlyDef ::= Annotations Modifiers PatDef */
  lazy val earlyDef = (annotation*) ~ (modifier*) ~ ('val -~ patDef ^~~^ ValPatternDefinition) ^~~^ AnnotatedDefinition

  /** TopStatSeq ::= TopStat {semi TopStat} */
  lazy val topStatSeq = topStat */semi

  /** TopStat ::= {Annotation} {Modifier} TmplDef
| Import
| Packaging
| */
  lazy val topStat : Rule[Statement] = (
      (annotation*) ~ (modifier*) ~ tmplDef ^~~^ AnnotatedDefinition
      | importStat
      | packaging)

  /** Packaging ::= package QualId [nl] ‘{’ TopStatSeq ‘}’ */
  lazy val packaging = ('package -~ qualId) ~ ((nl?) -~ curly(topStatSeq)) ^~^ Packaging

  /** CompilationUnit ::= [package QualId semi] TopStatSeq */
  lazy val compilationUnit = ('package -~ qualId ~- semi ?) ~ topStatSeq ^~^ CompilationUnit
  
}
