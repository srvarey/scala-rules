package net.foggin.rules.plugin

import org.eclipse.jface.text._
import org.eclipse.jface.text.presentation._
import org.eclipse.jface.text.source._

import org.eclipse.swt.SWT
import org.eclipse.swt.custom.StyleRange
import org.eclipse.swt.graphics.RGB

class ScalaPresentationReconciler(editor : ScalaEditor) extends IPresentationReconciler with IPresentationReconcilerExtension {
  
  val DEFAULT = new TextAttribute(editor.colour(new RGB(0, 0, 0)))
  val KEYWORD = new TextAttribute(editor.colour(new RGB(128, 128, 128)), null, SWT.BOLD)
  val LITERAL = new TextAttribute(editor.colour(new RGB(0, 128, 0)))
  val COMMENT = new TextAttribute(editor.colour(new RGB(128, 128, 255)), null, SWT.ITALIC)

  val parser = new scala.ScalaParser[ScalaDocumentInput] { }
  
  def damage(from : Int, to : Int) {
    if (damageStart < 0 || from < damageStart) damageStart = from
    if (to > damageEnd) damageEnd = to
  }
  
  var damageStart = -1
  var damageEnd = -1

  class ScalaDocument extends EditableDocument[Char, ScalaDocumentInput] {
    val first = new ScalaDocumentInput
  }
  
  class ScalaDocumentInput extends IncrementalInput[Char, ScalaDocumentInput] {
    def element = new ScalaDocumentInput

    override protected def onSuccess[T](key : AnyRef,  result : Success[T, ScalaDocumentInput]) {
      //println(key + " -> " + result)
      key match {
        case ("keyword", _) => keyword(index, result.rest.index)
        case ("literal", _) => literal(index, result.rest.index)
        case ("comment", _) => comment(index, result.rest.index)
        case _ => // do nothing
      }
    }
    
    override def cleanResults(pos : Int) = {
      map foreach {
        case ((key : AnyRef, _), Success(_, elem)) if elem.index >= pos => remove(key, index, elem.index)
        case _ => 
      }
      super.cleanResults(pos)
    }
    
    private def remove(key : AnyRef, from : Int, to : Int) = key match {
      case "keyword" | "literal" | "comment" => damage(from, to)
      case _ =>
    }
    
    override def toString = "@" + index
  }

  def keyword(start : Int, end : Int) = applyStyle(start, end, KEYWORD)
  def literal(start : Int, end : Int) = applyStyle(start, end, LITERAL)
  def comment(start : Int, end : Int) = applyStyle(start, end, COMMENT)
  def default(start : Int, end : Int) = applyStyle(start, end, DEFAULT)

  def applyStyle(start : Int, end : Int, attribute : TextAttribute) {
    val style= attribute.getStyle
    val fontStyle = attribute.getStyle & (SWT.ITALIC | SWT.BOLD | SWT.NORMAL)
    val styleRange = new StyleRange(start, end - start, attribute.getForeground, attribute.getBackground, fontStyle)
    styleRange.strikeout = (attribute.getStyle & TextAttribute.STRIKETHROUGH) != 0
    styleRange.underline = (attribute.getStyle & TextAttribute.UNDERLINE) != 0
    styleRange.font = attribute.getFont
    fViewer.getTextWidget.setStyleRange(styleRange)
  }
  
   private var fViewer : ITextViewer = null
   private var scalaDocument : ScalaDocument = null

   private val inputListener = new ITextInputListener {
    def inputDocumentAboutToBeChanged(oldDocument : IDocument, newDocument : IDocument) {
      removeDocument(oldDocument)
    }

    def inputDocumentChanged(oldDocument : IDocument, newDocument : IDocument) {
      setDocument(newDocument)
    }
  }
     
  private val documentListener = new IDocumentListener {
    def documentAboutToBeChanged(event : DocumentEvent) {
      scalaDocument.edit(event.getOffset, event.getLength, event.getText)
      if (damageEnd > 0) damageEnd = damageEnd - event.getLength + event.getText.length
    }

    def documentChanged(e : DocumentEvent) { }
  }
   
  private val textListener = new ITextListener {
    def textChanged(e : TextEvent) {
      if (e.getViewerRedrawState()) updatePresentation(fViewer.getDocument())
    }
  }

  private def setDocument(document : IDocument) {
    if (document ne null) {
      document.addDocumentListener(documentListener)
      fViewer.addTextListener(textListener)
      scalaDocument = new ScalaDocument
      scalaDocument.edit(0, 0, document.get)
      updatePresentation(document)
    }
  }

  private def updatePresentation(document : IDocument) {
    if (document ne null) {
      if (damageStart > 0 && damageEnd > damageStart) default(damageStart, damageEnd)
      damageStart = -1
      damageEnd = -1
      val input = new scala.ScalaInput(scalaDocument.first)
      parser.compilationUnit(input)
    }
  }
  
  private def removeDocument(document : IDocument) {
    if (document ne null) {
      fViewer.removeTextListener(textListener)
      document.removeDocumentListener(documentListener)
    }
  }

  // @see org.eclipse.jface.text.presentation.IPresentationReconcilerExtension#getDocumentPartitioning()
  def getDocumentPartitioning() = IDocumentExtension3.DEFAULT_PARTITIONING

  // @see IPresentationReconciler#install(ITextViewer)
  def install(viewer : ITextViewer) {
    fViewer = viewer
    fViewer.addTextInputListener(inputListener)
    setDocument(viewer.getDocument())
  }

  // @see IPresentationReconciler#uninstall()
  def uninstall() {
    fViewer.removeTextInputListener(inputListener)
    removeDocument(fViewer.getDocument())
  }

  // @see IPresentationReconciler#getDamager(String)
  def getDamager(contentType : String) : IPresentationDamager = null

  // @see IPresentationReconciler#getRepairer(String)
  def getRepairer(contentType : String) : IPresentationRepairer = null
}
