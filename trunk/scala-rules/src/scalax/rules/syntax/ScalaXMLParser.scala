// -----------------------------------------------------------------------------
//
//  Scalax - The Scala Community Library
//  Copyright (c) 2005-8 The Scalax Project. All rights reserved.
//
//  The primary distribution site is http://scalax.scalaforge.org/
//
//  This software is released under the terms of the Revised BSD License.
//  There is NO WARRANTY.  See the file LICENSE for the full text.
//
// -----------------------------------------------------------------------------

package scalax.rules.syntax

import Character._

/** An XML parser for Scala source code.
 *
 * @author Andrew Foggin
 *
 * based on Scala Language Specification.
 */
trait ScalaXMLParser extends ScalaScanner {

  def scalaPattern : Parser[Expression]
  def scalaExpr : Parser[Expression]
  
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
  val elementName = xmlName as "elementName"
  
  val xmlS = choice(" \t\r\n")+
  val xmlComment = "<!--" -~ anyChar *~- "-->" ^^ toString ^^ XMLComment as "xmlComment"
  val resolvedReference = "&amp;" -^ '&' | "&lt;" -^ '<' | "&gt;" -^ '>' | "&apos;" -^ '\'' | "&quot;" -^ '"'
  
  val startElement = '<' -~ (elementName&) as "startElement"
  val emptyElement = "/>" -^ None as "emptyElement"
  val tagEnd = '>' as "tagEnd"
  val endTag = "</" as "endTag"
  
  def debug(message : String) : Parser[Nothing] = token >> { t => s => println(message + " (next token: " + t + ")"); Failure }
  
  lazy val xmlExpr = (skip -~ (xmlElement  | cDataSect | pi) +) ^^ NodeList as "xmlExpr"
  lazy val xmlElement = startElement -~ elementName ~ (attribute*) ~- (xmlS?) >~> xmlElementRest
  def xmlElementRest(name : String, attributes : List[Attribute]) : Parser[XMLElement] = (emptyElement
      | tagEnd -~ (xmlContent  ^^ Some[Expression]) ~- endElement(name)) ^^ XMLElement(name, attributes)
  def endElement(name : String) = (endTag -~ elementName ~- (xmlS?) ~- tagEnd) filter (_ == name)
  lazy val xmlContent : Parser[Expression] = (xmlElement | xmlComment | charData | scalaExpr  | cDataSect | pi | entityRef *) ^^ NodeList

  lazy val xmlPattern = skip -~ startElement -~ elementName ~- (xmlS?) >> xmlPatternRest as "xmlPattern"
  def xmlPatternRest(name : String) : Parser[XMLPattern] = (emptyElement
      | tagEnd -~ xmlPatternContent ~- endElement(name)) ^^ XMLPattern(name)
  lazy val xmlPatternContent = (xmlPattern | xmlComment | charData | scalaPattern | cDataSect | pi | entityRef *) ^^ NodeList ^^ Some[Expression]

  lazy val cDataSect = "<![CDATA[" -~ anyChar *~- "]]>" ^^ toString ^^ CData
  lazy val pi = "<?" -~ xmlName ~ (xmlS -~ anyChar *~- "?>" ^^ toString | "?>" -^ "") ^~^ ProcessingInstruction
  lazy val entityRef = '&' -~ xmlName ~- ';' ^^ EntityRef

  val attributeName = xmlS -~ xmlName ~- '=' as "attributeName"
  val attributeValue : Parser[Expression] = (quoted('"') | quoted('\'') as "attributeValue") | scalaExpr
  def quoted(ch : Char) = positioned(ch -~ (resolvedReference | anyChar - choice("<&")) *~- ch ^^ toString ^^ lit[String])
  
  val attribute = attributeName ~ attributeValue ^~^ Attribute
  val charData = ("{{" -^ '{' | resolvedReference | anyChar - ("]]>" | '{' | '<' | '&') +) ^^ toString ^^ TextNode
  

}
