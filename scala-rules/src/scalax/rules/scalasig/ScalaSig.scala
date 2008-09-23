package scalax.rules.scalasig

object ScalaSigParser {
  
  def getScalaSig(clazz : Class[_]) : Option[ByteCode] = {
    val byteCode = ByteCode.forClass(clazz)
    val classFile = ClassFileParser.parse(byteCode)
    
    /*
    println("ClassFile version: " + classFile.majorVersion + "." + classFile.minorVersion)
    println("Class: " + classFile.className)
    println("Superclass: " + classFile.superClass)
    println("Interfaces: " + classFile.interfaces.mkString(", "))
    println("Constant pool:")
    val constantPool = classFile.header.constants
    for (i <- 1 to constantPool.size) println(i + "\t" + constantPool(i))
    */
    
    classFile.attribute("ScalaSig").map(_.byteCode)
  }
  
  def parse(clazz : Class[_]) : Option[ScalaSig] = {
    getScalaSig(clazz).map(ScalaSigAttributeParsers.parse)
  }
}

object ScalaSigAttributeParsers extends ByteCodeReader  {
  def parse(byteCode : ByteCode) = expect(scalaSig)(byteCode)
  
  val nat = apply { 
    def natN(in : ByteCode, x : Int) : Result[ByteCode, Int, Nothing] = in.nextByte match {
      case Success(out, b) => {
        val y = (x << 7) + (b & 0x7f)
        if ((b & 0x80) == 0) Success(out, y) else natN(out, y)
      }
      case _ => Failure
    }
    in => natN(in, 0)
  }
  
  val rawBytes = nat >> bytes
  val entry = nat ~ rawBytes
  val symtab = nat >> entry.times
  val scalaSig = nat ~ nat ~ symtab ^~~^ ScalaSig

  val utf8 = read(_ toUTF8String)
  val longValue = read(_ toLong)
}

case class ScalaSig(majorVersion : Int, minorVersion : Int, table : Seq[Int ~ ByteCode]) extends DefaultMemoisable {
  
  case class Entry(index : Int, entryType : Int, byteCode : ByteCode) extends DefaultMemoisable {
    def scalaSig = ScalaSig.this
    
    def setByteCode(byteCode : ByteCode) = Entry(index, entryType, byteCode)
  }
  
  def hasEntry(index : Int) = table isDefinedAt index
  
  def getEntry(index : Int) = {
    val entryType ~ byteCode = table(index)
    Entry(index, entryType, byteCode)
  }
  
  def parseEntry(index : Int) = applyRule(ScalaSigParsers.parseEntry(ScalaSigEntryParsers.entry)(index))
  
  implicit def applyRule[A](parser : ScalaSigParsers.Parser[A]) = ScalaSigParsers.expect(parser)(this)
  
  override def toString = "ScalaSig version " + majorVersion + "." + minorVersion + {
    for (i <- 0 until table.size) yield i + ":\t" + parseEntry(i) // + "\n\t" + getEntry(i)
  }.mkString("\n", "\n", "")
  
  lazy val symbols : Seq[Symbol] = ScalaSigParsers.symbols
  
  lazy val topLevelClass : Option[ClassSymbol] = ScalaSigParsers.topLevelClass
  lazy val topLevelObject : Option[ObjectSymbol] = ScalaSigParsers.topLevelObject
}

object ScalaSigParsers extends RulesWithState with MemoisableRules {
  type S = ScalaSig
  type Parser[A] = Rule[A, String]

  val symTab = read(_.table)
  val size = symTab ^^ (_.size)
  
  def entry(index : Int) = memo(("entry", index)) {
    cond(_ hasEntry index) -~ read(_ getEntry index) >-> { entry => Success(entry, entry.entryType) }
  }
  
  def parseEntry[A](parser : ScalaSigEntryParsers.EntryParser[A])(index : Int) : Parser[A] = 
    entry(index) -~ parser >> { a => entry => Success(entry.scalaSig, a) }

  def allEntries[A](f : ScalaSigEntryParsers.EntryParser[A]) = size >> { n => anyOf((0 until n) map parseEntry(f)) }

  lazy val entries = allEntries(ScalaSigEntryParsers.entry) as "entries"
  lazy val symbols = allEntries(ScalaSigEntryParsers.symbol) as "symbols"
  lazy val methods = allEntries(ScalaSigEntryParsers.methodSymbol) as "methods"
  lazy val attributes = allEntries(ScalaSigEntryParsers.attributeInfo) as "attributes"
  
  lazy val topLevelClass = allEntries(ScalaSigEntryParsers.topLevelClass) ^^ (_ firstOption)
  lazy val topLevelObject = allEntries(ScalaSigEntryParsers.topLevelObject) ^^ (_ firstOption)
}

object ScalaSigEntryParsers extends RulesWithState with MemoisableRules {
  import ScalaSigAttributeParsers.{nat, utf8, longValue}
  
  type S = ScalaSig#Entry
  type EntryParser[A] = Rule[A, String]
    
  implicit def byteCodeEntryParser[A](rule : ScalaSigAttributeParsers.Parser[A]) : EntryParser[A] = apply { entry =>
    rule(entry.byteCode) mapOut (entry setByteCode _)
  }
    
  def toEntry[A](index : Int) = apply { sigEntry => ScalaSigParsers.entry(index)(sigEntry.scalaSig) }
  
  def parseEntry[A](parser : EntryParser[A])(index : Int) = (toEntry(index) -~ parser)
  
  implicit def entryType(code : Int) = key filter (_ == code)
  
  val index = read(_.index)
  val key = read(_.entryType)
  
  lazy val entry : EntryParser[Any] = symbol | typeEntry | literal | name | attributeInfo | annotInfo | children | get
  
  val ref = byteCodeEntryParser(nat)
  
  val termName = 1 -~ utf8
  val typeName = 2 -~ utf8
  
  val name = termName | typeName as "name"
  
  def refTo[A](rule : EntryParser[A]) : EntryParser[A] = ref >>& parseEntry(rule)

  lazy val nameRef = refTo(name)
  lazy val symbolRef = refTo(symbol)
  lazy val typeRef = refTo(typeEntry)
  lazy val constantRef = refTo(literal)

  val symbolInfo = nameRef ~ symbolRef ~ nat ~ (symbolRef?) ~ ref ~ get ^~~~~~^ SymbolInfo

  def symbolEntry(key : Int) = (key -~ none | (key + 64) -~ nat) -~ symbolInfo
  
  val noSymbol = 3 -^ NoSymbol
  val typeSymbol = symbolEntry(4) ^^ TypeSymbol as "typeSymbol"
  val aliasSymbol = symbolEntry(5) ^^ AliasSymbol as "alias"
  val classSymbol = symbolEntry(6) ~ (ref?) ^~^ ClassSymbol as "class"
  val objectSymbol = symbolEntry(7) ^^ ObjectSymbol as "object"
  val methodSymbol = symbolEntry(8) ~ (ref?) ^~^ MethodSymbol as "method"
  val extRef = 9 -~ nameRef ~ (symbolRef?) ~ get ^~~^ ExternalSymbol as "extRef"
  val extModClassRef = 10 -~ nameRef ~ (symbolRef?) ~ get ^~~^ ExternalSymbol as "extModClassRef"
  
  lazy val symbol : EntryParser[Symbol] =
      noSymbol |
      typeSymbol | 
      aliasSymbol | 
      classSymbol | 
      objectSymbol | 
      methodSymbol | 
      extRef |
      extModClassRef as "symbol"
  
  val classSymRef = ref
  val attribTreeRef = ref
  val typeLevel = nat
  val typeIndex = nat
  
  lazy val typeEntry : EntryParser[Type] =
      11 -^ NoType |
      12 -^ NoPrefixType |
      13 -~ symbolRef ^^ ThisType |
      14 -~ typeRef ~ symbolRef ^~^ SingleType |
      15 -~ typeRef ~ constantRef ^~^ ConstantType |
      16 -~ typeRef ~ symbolRef ~ (typeRef*) ^~~^ TypeRefType |
      17 -~ typeRef ~ typeRef ^~^ TypeBoundsType |
      18 -~ classSymRef ~ (typeRef*) ^~^ RefinedType |
      19 -~ symbolRef ~ (typeRef*) ^~^ ClassInfoType |
      20 -~ typeRef ~ (typeRef*) ^~^ MethodType |
      21 -~ typeRef ~ (refTo(typeSymbol)*) ^~^ PolyType |
      22 -~ typeRef ~ (typeRef*) ^~^ ImplicitMethodType
      42 -~ typeRef ~ (attribTreeRef*) ^~^ AnnotatedType |
      51 -~ typeRef ~ symbolRef ~ (attribTreeRef*) ^~~^ AnnotatedWithSelfType |
      47 -~ typeLevel ~ typeIndex ^~^ DeBruijnIndexType |
      48 -~ typeRef ~ (symbolRef*) ^~^ ExistentialType as "type"
  
  lazy val literal =
      24 -^ () |
      25 -~ longValue ^^ (_ != 0) |
      26 -~ longValue ^^ (_.asInstanceOf[Byte]) |
      27 -~ longValue ^^ (_.asInstanceOf[Short]) |
      28 -~ longValue ^^ (_.asInstanceOf[Char]) |
      29 -~ longValue ^^ (_.asInstanceOf[Int]) |
      30 -~ longValue ^^ (_.asInstanceOf[Long]) |
      31 -~ longValue ^^ (l => java.lang.Float.intBitsToFloat(l.asInstanceOf[Int])) |
      32 -~ longValue ^^ (java.lang.Double.longBitsToDouble) |
      33 -~ nameRef |
      34 -^ null |
      35 -~ typeRef
    
  lazy val attributeInfo = 40 -~ symbolRef ~ typeRef ~ (constantRef?) ~ (nameRef ~ constantRef *) ^~~~^ AttributeInfo // sym_Ref info_Ref {constant_Ref} {nameRef constantRef}
  lazy val children = 41 -~ (nat*) ^^ Children //sym_Ref {sym_Ref} 
  lazy val annotInfo = 43 -~ (nat*) ^^ AnnotInfo // attarg_Ref {constant_Ref attarg_Ref}

  lazy val topLevelClass = classSymbol filter isTopLevel
  lazy val topLevelObject = objectSymbol filter isTopLevel
  
  def isTopLevel(symbol : Symbol) = symbol.parent match {
    case Some(ext : ExternalSymbol) => true
    case _ => false
  }
  
  //
}

  case class AttributeInfo(symbol : Symbol, typeRef : Type, value : Option[Any], values : Seq[String ~ Any]) // sym_Ref info_Ref {constant_Ref} {nameRef constantRef}
  case class Children(symbolRefs : Seq[Int]) //sym_Ref {sym_Ref} 

  case class AnnotInfo(refs : Seq[Int]) // attarg_Ref {constant_Ref attarg_Ref}

  /*                  | 49 TREE len_Nat 1 EMPTYtree
 *                  | 49 TREE len_Nat 2 PACKAGEtree type_Ref sym_Ref mods_Ref name_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 3 CLASStree type_Ref sym_Ref mods_Ref name_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 4 MODULEtree type_Ref sym_Ref mods_Ref name_Ref tree_Ref
 *                  | 49 TREE len_Nat 5 VALDEFtree type_Ref sym_Ref mods_Ref name_Ref tree_Ref tree_Ref
 *                  | 49 TREE len_Nat 6 DEFDEFtree type_Ref sym_Ref mods_Ref name_Ref numtparams_Nat {tree_Ref} numparamss_Nat {numparams_Nat {tree_Ref}} tree_Ref tree_Ref
 *                  | 49 TREE len_Nat 7 TYPEDEFtree type_Ref sym_Ref mods_Ref name_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 8 LABELtree type_Ref sym_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 9 IMPORTtree type_Ref sym_Ref tree_Ref {name_Ref name_Ref}
 *                  | 49 TREE len_Nat 10 ANNOTATIONtree type_Ref sym_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 11 DOCDEFtree type_Ref sym_Ref string_Ref tree_Ref
 *                  | 49 TREE len_Nat 12 TEMPLATEtree type_Ref sym_Ref numparents_Nat {tree_Ref} tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 13 BLOCKtree type_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 14 CASEtree type_Ref tree_Ref tree_Ref tree_Ref
 *                  | 49 TREE len_Nat 15 SEQUENCEtree type_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 16 ALTERNATIVEtree type_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 17 STARtree type_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 18 BINDtree type_Ref sym_Ref name_Ref tree_Ref
 *                  | 49 TREE len_Nat 19 UNAPPLYtree type_Ref tree_Ref {tree_Ref} 
 *                  | 49 TREE len_Nat 20 ARRAYVALUEtree type_Ref tree_Ref {tree_Ref} 
 *                  | 49 TREE len_Nat 21 FUNCTIONtree type_Ref sym_Ref tree_Ref {tree_Ref} 
 *                  | 49 TREE len_Nat 22 ASSIGNtree type_Ref tree_Ref tree_Ref 
 *                  | 49 TREE len_Nat 23 IFtree type_Ref tree_Ref tree_Ref tree_Ref 
 *                  | 49 TREE len_Nat 24 MATCHtree type_Ref tree_Ref {tree_Ref} 
 *                  | 49 TREE len_Nat 25 RETURNtree type_Ref sym_Ref tree_Ref
 *                  | 49 TREE len_Nat 26 TREtree type_Ref tree_Ref tree_Ref {tree_Ref} 
 *                  | 49 TREE len_Nat 27 THROWtree type_Ref tree_Ref 
 *                  | 49 TREE len_Nat 28 NEWtree type_Ref tree_Ref 
 *                  | 49 TREE len_Nat 29 TYPEDtree type_Ref tree_Ref tree_Ref 
 *                  | 49 TREE len_Nat 30 TYPEAPPLYtree type_Ref tree_Ref {tree_Ref} 
 *                  | 49 TREE len_Nat 31 APPLYtree type_Ref tree_Ref {tree_Ref} 
 *                  | 49 TREE len_Nat 32 APPLYDYNAMICtree type_Ref sym_Ref tree_Ref {tree_Ref} 
 *                  | 49 TREE len_Nat 33 SUPERtree type_Ref sym_Ref tree_Ref name_Ref
 *                  | 49 TREE len_Nat 34 THIStree type_Ref sym_Ref  name_Ref
 *                  | 49 TREE len_Nat 35 SELECTtree type_Ref sym_Ref tree_Ref name_Ref
 *                  | 49 TREE len_Nat 36 IDENTtree type_Ref sym_Ref name_Ref
 *                  | 49 TREE len_Nat 37 LITERALtree type_Ref constant_Ref 
 *                  | 49 TREE len_Nat 38 TYPEtree type_Ref 
 *                  | 49 TREE len_Nat 39 ANNOTATEDtree type_Ref tree_Ref tree_Ref
 *                  | 49 TREE len_Nat 40 SINGLETONTYPEtree type_Ref tree_Ref 
 *                  | 49 TREE len_Nat 41 SELECTFROMTYPEtree type_Ref tree_Ref name_Ref 
 *                  | 49 TREE len_Nat 42 COMPOUNDTYPEtree type_Ref tree_Ref 
 *                  | 49 TREE len_Nat 43 APPLIEDTYPEtree type_Ref tree_Ref {tree_Ref} 
 *                  | 49 TREE len_Nat 44 TYPEBOUNDStree type_Ref tree_Ref tree_Ref
 *                  | 49 TREE len_Nat 45 EXISTENTIALTYPEtree type_Ref tree_Ref {tree_Ref} 
 *                  | 50 MODIFIERS len_Nat flags_Long privateWithin_Ref {Annotation_Ref}
 
 *   Attarg         = Refltree | Constant
 *
 *   len is remaining length after `len'.
 */

