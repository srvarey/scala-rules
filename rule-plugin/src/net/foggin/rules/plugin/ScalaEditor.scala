package net.foggin.rules.plugin

import org.eclipse.jface.text.source._
import org.eclipse.ui.editors.text._

class ScalaEditor extends TextEditor {

  val scanner = new PluginScanner
  
  setSourceViewerConfiguration(new SourceViewerConfiguration() {
    override def getPresentationReconciler(sourceViewer : ISourceViewer) = scanner.reconciler(sourceViewer)
  })
  
  setDocumentProvider(new FileDocumentProvider())
  
  override def dispose() {
    scanner.dispose()
    super.dispose()
  }
}
