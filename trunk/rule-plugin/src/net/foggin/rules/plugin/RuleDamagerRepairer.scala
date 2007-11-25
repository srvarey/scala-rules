package net.foggin.rules.plugin

import org.eclipse.jface.text._
import org.eclipse.jface.text.presentation._
import org.eclipse.jface.text.source._

import org.eclipse.swt.SWT
import org.eclipse.swt.custom.StyleRange
import org.eclipse.swt.graphics.Color
import org.eclipse.swt.graphics.RGB
import org.eclipse.swt.widgets.Display

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

class PluginParser extends scala.ScalaParser[DefaultIncrementalInput] {

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

  def reconciler(sourceViewer : ISourceViewer) = new PresentationReconciler() {
    val dr = new RuleDamagerRepairer(PluginParser.this)
    setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE)
    setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE)
  }
}

// TODO: Implement a custom PresentationReconciler instead.
class RuleDamagerRepairer(parser : PluginParser) 
    extends IPresentationDamager with IPresentationRepairer with IDocumentListener {

  private var document : IDocument = null
  private var scalaDocument : DefaultDocument = null


  // @see IDocumentListener
  def documentAboutToBeChanged(event : DocumentEvent) = {
      // We have to apply the change here because 'getDamageRegion' can be called more than once for each event :-(
      scalaDocument.edit(event.getOffset, event.getLength, event.getText)
  }

  // @see IDocumentListener
  def documentChanged(event : DocumentEvent) { }

  // @see IPresentationDamager, IPresentationRepairer
  def setDocument(document : IDocument) {
    if (document ne this.document) {
      if (this.document ne null) this.document.removeDocumentListener(this)
      this.document = document;
      scalaDocument = new DefaultDocument
      scalaDocument.edit(0, 0, document.get)
      document.addDocumentListener(this)
    }
  }
    
  // @see IPresentationDamager
  def getDamageRegion(partition : ITypedRegion, event : DocumentEvent, documentPartitioningChanged : Boolean) : IRegion = {
    // Just return the whole lot
    partition
  }
        
  // @see IPresentationRepairer
  def createPresentation(presentation : TextPresentation, region : ITypedRegion) {
    val input = new scala.ScalaInput(scalaDocument.first)
    for (t <- parser.styleTokens(input)) t.addToPresentation(presentation)
  }
}
