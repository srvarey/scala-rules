package net.foggin.rules.plugin

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.ui.editors.text.FileDocumentProvider
import org.eclipse.ui.editors.text.TextEditor

class ScalaEditor extends TextEditor {

  val scanner = new IncrementalScalaScanner
  val colorManager = new ColorManager
  
  setSourceViewerConfiguration(new SourceViewerConfiguration() {
    override def getPresentationReconciler(sourceViewer : ISourceViewer) =
      new PresentationReconciler() {
        val dr = new RuleDamagerRepairer(scanner, colorManager)
        setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE)
        setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE)
      }
  })
  
  setDocumentProvider(new FileDocumentProvider())
  
  override def dispose() {
    colorManager.dispose()
    super.dispose()
  }
}
