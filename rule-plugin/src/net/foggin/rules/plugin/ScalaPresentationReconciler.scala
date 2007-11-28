package net.foggin.rules.plugin

import org.eclipse.swt.custom.StyleRange;

import org.eclipse.jface.text._
import org.eclipse.jface.text.presentation._

class ScalaPresentationReconciler(parser : PluginParser) extends IPresentationReconciler with IPresentationReconcilerExtension {

   private var fViewer : ITextViewer = null
   private var scalaDocument : DefaultDocument = null

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
      scalaDocument = new DefaultDocument
      scalaDocument.edit(0, 0, document.get)
      updatePresentation(document)
    }
  }

  private def updatePresentation(document : IDocument) {
    if (document ne null) {
      val presentation = new TextPresentation(document.getPartition(0), 1000)
      val input = new scala.ScalaInput(scalaDocument.first)
      for (t <- parser.styleTokens(input)) t.addToPresentation(presentation)
      fViewer.changeTextPresentation(presentation, false)
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
