package net.foggin.rules.scala;

object ScalaToken {
  
  /** Reserved ids that can terminate a statement */
  val endStatements = _root_.scala.collection.immutable.Set(
      "this", "null", "true", "false", "return", "type", "_")
    
  /** Reserved ids that cannot start a statement */
  val cannotStartStatements = _root_.scala.collection.immutable.Set(
      "catch", "else", "extends", "finally", "forSome", "match", "requires", "with", "yield",
      "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "\u21D2")

  def canStartStatement(token : ScalaToken) = token match {
    case OpenRound | OpenCurly => true
    case _ : Delimiter => false
    case reserved : ReservedId => !cannotStartStatements.contains(reserved.id)
    case _ => true
  }

  def canEndStatement(token : ScalaToken) = token match {
    case null => false
    case CloseRound | CloseSquare | CloseCurly => true
    case _ : Delimiter => false
    case reserved : ReservedId => endStatements.contains(reserved.id)
    case _ => true
  }
}


abstract class ScalaToken {
  
}

abstract class ReservedId extends ScalaToken {
  def id : String
}

object ReservedId {
  val reserved = _root_.scala.collection.immutable.Set(
      "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final",
      "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null", "object",
      "override", "package", "private", "protected", "requires", "return", "sealed", "super", "this", 
      "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield",
      "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2")

  def apply(id : String) = isReserved(id)
  def isReserved(id : String) = reserved.contains(id)
}

case class Keyword(id : String) extends ReservedId
case class ReservedOperator(id : String) extends ReservedId

abstract class Delimiter extends ScalaToken
case object Newline extends Delimiter
case object Semicolon extends Delimiter
case object Dot extends Delimiter
case object Comma extends Delimiter
case object OpenRound extends Delimiter
case object CloseRound extends Delimiter
case object OpenSquare extends Delimiter
case object CloseSquare extends Delimiter
case object OpenCurly extends Delimiter
case object CloseCurly extends Delimiter

abstract class IdToken extends ScalaToken { def id : String }

case class Operator(id : String) extends IdToken
case class QuotedId(id : String) extends IdToken
case class VariableId(id : String) extends IdToken
case class NonVariableId(id : String) extends IdToken

abstract class Literal extends ScalaToken with Expression

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

case object Comment extends ScalaToken
case object Space extends ScalaToken

abstract class XMLToken extends ScalaToken
case object XMLComment extends XMLToken
case class StartElement(name : String) extends XMLToken
case class EndElement(name : String) extends XMLToken
case class AttributeName(name : String) extends XMLToken
case class AttributeValue(value : String) extends XMLToken
case object EmptyElement extends XMLToken
case object ElementContent extends XMLToken
case class TextContent(text : String) extends XMLToken

case class StartXML(space : Boolean, token : XMLToken) extends ScalaToken
