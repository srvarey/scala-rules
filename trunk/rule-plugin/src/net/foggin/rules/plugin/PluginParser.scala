package net.foggin.rules.plugin

import org.eclipse.jface.text._
import org.eclipse.jface.text.presentation._
import org.eclipse.jface.text.source._

import org.eclipse.swt.SWT
import org.eclipse.swt.custom.StyleRange
import org.eclipse.swt.graphics.Color
import org.eclipse.swt.graphics.RGB
import org.eclipse.swt.widgets.Display

/*
  
case class StyleToken(val start : Int, val end : Int, val attribute : TextAttribute) {
  private var added = false

  def addToPresentation(presentation : TextPresentation) {
    if (!added) {
      val style= attribute.getStyle
      val fontStyle = attribute.getStyle & (SWT.ITALIC | SWT.BOLD | SWT.NORMAL)
      val styleRange = new StyleRange(start, end - start, attribute.getForeground, attribute.getBackground, fontStyle)
      styleRange.strikeout = (attribute.getStyle & TextAttribute.STRIKETHROUGH) != 0
      styleRange.underline = (attribute.getStyle & TextAttribute.UNDERLINE) != 0
      styleRange.font = attribute.getFont
      presentation.addStyleRange(styleRange)
      added = true
    }
  }
}

class PluginParser extends scala.ScalaParser[ScalaDocumentInput] {
  //IncrementalInput.debug = true

  private val colours = new _root_.scala.collection.mutable.HashMap[RGB, Color]

  def colour(rgb: RGB) = colours.getOrElseUpdate(rgb, new Color(Display.getCurrent(), rgb))
  def dispose() = for (colour <- colours.values) colour.dispose
  
  val DEFAULT = new TextAttribute(colour(new RGB(0, 0, 0)))
  val KEYWORD = new TextAttribute(colour(new RGB(128, 128, 128)), null, SWT.BOLD)
  val LITERAL = new TextAttribute(colour(new RGB(0, 128, 0)))
  val COMMENT = new TextAttribute(colour(new RGB(128, 128, 255)), null, SWT.ITALIC)
  
  def style(rule : Rule[Any], attribute : TextAttribute) = {
    for (_ <- whitespace; start <- context; _ <- rule; end <- context) yield StyleToken(start.input.index, end.input.index, attribute)
  }
  
  val commentToken = style(comment, COMMENT)
  val literalToken = style(literal, LITERAL)
  val keywordToken = style(keyword, KEYWORD)
  val otherToken = style(space | newline | delimiter | reservedOp | id, DEFAULT)
  
  val styleToken = memo("styleToken", commentToken | keywordToken | literalToken | otherToken)
  val styleTokens = view(styleToken) _
}

*/