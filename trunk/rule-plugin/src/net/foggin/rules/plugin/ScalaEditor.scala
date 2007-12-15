package net.foggin.rules.plugin

import org.eclipse.ui.editors.text._

import org.eclipse.jface.text._
import org.eclipse.jface.text.presentation._
import org.eclipse.jface.text.source._

import org.eclipse.swt.SWT
import org.eclipse.swt.custom.StyleRange
import org.eclipse.swt.graphics.Color
import org.eclipse.swt.graphics.RGB
import org.eclipse.swt.widgets.Display

import _root_.scala.collection.mutable.HashMap

class ScalaEditor extends TextEditor {

  private val colours = new HashMap[RGB, Color]

  val DEFAULT = new TextAttribute(colour(new RGB(0, 0, 0)))
  val KEYWORD = new TextAttribute(colour(new RGB(128, 128, 128)), null, SWT.BOLD)
  val LITERAL = new TextAttribute(colour(new RGB(0, 128, 0)))
  val COMMENT = new TextAttribute(colour(new RGB(128, 128, 255)), null, SWT.ITALIC)
  val XML_ELEMENT_NAME = new TextAttribute(colour(new RGB(128, 0, 0)), null, SWT.BOLD)
  val XML_OTHER = new TextAttribute(colour(new RGB(128, 0, 0)))
    
  val attributes = HashMap[AnyRef, TextAttribute](
      "keyword" -> KEYWORD,
      "literal" -> LITERAL,
      "attributeValue" -> LITERAL,
      "comment" -> COMMENT,
      "xmlComment" -> COMMENT,
      "elementName" -> XML_ELEMENT_NAME,
      "attributeName" -> XML_OTHER,
      "startElement" -> XML_OTHER,
      "emptyElement" -> XML_OTHER,
      "tagEnd" -> XML_OTHER,
      "endTag" -> XML_OTHER)

  def colour(rgb: RGB) = colours.getOrElseUpdate(rgb, new Color(Display.getCurrent(), rgb))
  
  setSourceViewerConfiguration(new SourceViewerConfiguration() {
    override def getPresentationReconciler(sourceViewer : ISourceViewer) = new ScalaHighlighter
  })
  
  setDocumentProvider(new FileDocumentProvider())
  
  override def dispose() {
    for (colour <- colours.values) colour.dispose()
    super.dispose()
  }
  
  class ScalaHighlighter extends IPresentationReconciler with IPresentationReconcilerExtension {
    private var viewer : ITextViewer = null
    private var input : ScalaDocumentInput = null
    private var damageStart = -1
    private var damageEnd = -1

    val parser = new scala.ScalaParser[ScalaDocumentInput] { }
    
    class ScalaDocumentInput extends IncrementalInput[Char, ScalaDocumentInput] {
      def element = new ScalaDocumentInput

      override protected def onSuccess[T](key : AnyRef,  result : Success[T, ScalaDocumentInput]) = {
        key match {
          case (realKey : AnyRef, _) if attributes contains realKey =>
              println(realKey + "@" + index + "-" + result.rest.index)
              applyStyle(index, result.rest.index, attributes(realKey))
          case _ => // do nothing
        }
        super.onSuccess(key, result)
      }
      
      override protected def cleanResults(pos : Int) = {
        damage(pos)
        super.cleanResults(pos)
      }
      
      override protected def delete() {
        if (hasNextElement) nextElement.damage(index)
        super.delete()
      }
      
      private def damage(pos : Int) = map foreach {
        case ((key : AnyRef, _), Success(_, elem)) if elem.index >= pos && attributes.contains(key) => 
          if (damageStart < 0 || index < damageStart) damageStart = index
          if (elem.index > damageEnd) damageEnd = elem.index
        case _ => 
      }
    }

    def applyStyle(start : Int, end : Int, attribute : TextAttribute) {
      val style= attribute.getStyle
      val fontStyle = attribute.getStyle & (SWT.ITALIC | SWT.BOLD | SWT.NORMAL)
      val styleRange = new StyleRange(start, end - start, attribute.getForeground, attribute.getBackground, fontStyle)
      styleRange.strikeout = (attribute.getStyle & TextAttribute.STRIKETHROUGH) != 0
      styleRange.underline = (attribute.getStyle & TextAttribute.UNDERLINE) != 0
      styleRange.font = attribute.getFont
      viewer.getTextWidget.setStyleRange(styleRange)
    }
    
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
        input.edit(event.getOffset, event.getLength, event.getText)
        if (damageEnd > 0) damageEnd = damageEnd - event.getLength + event.getText.length
      }

      def documentChanged(e : DocumentEvent) { }
    }
     
    private val textListener = new ITextListener {
      def textChanged(e : TextEvent) {
        if (e.getViewerRedrawState()) updatePresentation(viewer.getDocument())
      }
    }

    private def setDocument(document : IDocument) {
      if (document ne null) {
        document.addDocumentListener(documentListener)
        viewer.addTextListener(textListener)
        input = new ScalaDocumentInput
        input.edit(0, 0, document.get)
        updatePresentation(document)
      }
    }

    private def updatePresentation(document : IDocument) {
      if (document ne null) {
        if (damageStart > 0 && damageEnd > damageStart) applyStyle(damageStart, damageEnd, DEFAULT)
        damageStart = -1
        damageEnd = -1
        // reparse document
        parser.compilationUnit(new scala.ScalaInput(input))
      }
    }
    
    private def removeDocument(document : IDocument) {
      if (document ne null) {
        viewer.removeTextListener(textListener)
        document.removeDocumentListener(documentListener)
      }
    }

    // @see org.eclipse.jface.text.presentation.IPresentationReconcilerExtension#getDocumentPartitioning()
    def getDocumentPartitioning() = IDocumentExtension3.DEFAULT_PARTITIONING

    // @see IPresentationReconciler#install(ITextViewer)
    def install(viewer : ITextViewer) {
      this.viewer = viewer
      viewer.addTextInputListener(inputListener)
      setDocument(viewer.getDocument())
    }

    // @see IPresentationReconciler#uninstall()
    def uninstall() {
      viewer.removeTextInputListener(inputListener)
      removeDocument(viewer.getDocument())
    }

    // @see IPresentationReconciler#getDamager(String)
    def getDamager(contentType : String) : IPresentationDamager = null

    // @see IPresentationReconciler#getRepairer(String)
    def getRepairer(contentType : String) : IPresentationRepairer = null
  }
  

}
