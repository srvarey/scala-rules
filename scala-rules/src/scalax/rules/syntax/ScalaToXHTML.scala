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

import _root_.scala.xml.{EntityRef => Entity, _}
/*
class ScalaToXHTML extends SimpleScalaParser {

  val index = position ^^ (_())
  def at(pos : Int) = index filter (_ == pos)
  
  val br = scanner.newline -^ List(<br/>, Text("\n"))
  val nbsp = scanner.elem(' ') -^ List(Entity("#160"))
  val textItem = scanner.item ^^ { ch => List(Text(ch.toString)) }
  
  val escapeItem = br | nbsp | textItem
    
  def escapeTo(pos : Int) : Rule[NodeSeq] = escapeItem *~- at(pos) ^^ List.flatten[Node] ^^ NodeSeq.fromSeq
  
  /** Look for a memoised result.  This is very ugly - try to think of a better way! */
    def recall(key : String) = (
        multiple(true) -~ lastTokenCanEndStatement(true) -~ createRule(key, failure)
        | multiple(true) -~ lastTokenCanEndStatement(false) -~ createRule(key, failure)
        | multiple(false) -~ lastTokenCanEndStatement(true) -~ createRule(key, failure)
        | multiple(false) -~ lastTokenCanEndStatement(false) -~ createRule(key, failure)) -~ index
      
  def escape(key : String) = ((recall(key) &) >> escapeTo &) ~- recall(key)
  def span(styleClass : String)(rule : Rule[NodeSeq]) = rule ^^ { body => List(<span class={styleClass}>{body}</span>) }
  def style(key : String) = span(key)(escape(key))
  
  val prettyPrint = (
        style("keyword") 
      | style("literal")
      | style("attributeValue")
      | style("comment")
      | style("xmlComment")
      | style("elementName")
      | style("attributeName")
      | span("xmlOther")(
            escape("startElement")
          | escape("emptyElement")
          | escape("tagEnd")
          | escape("endTag"))
      | escapeItem *) ^^ List.flatten[Node] ^^ NodeSeq.fromSeq
      
  def prettyPrintFor(rule : Rule[Any]) = expect(((rule&) | none) -~ prettyPrint)
}
*/