package net.foggin.rules.plugin

import org.eclipse.jface.text.source._
import org.eclipse.ui.editors.text._

class ScalaEditor extends TextEditor {

  val parser = new PluginParser
  
  setSourceViewerConfiguration(new SourceViewerConfiguration() {
    override def getPresentationReconciler(sourceViewer : ISourceViewer) = new ScalaPresentationReconciler(parser)
  })
  
  setDocumentProvider(new FileDocumentProvider())
  
  override def dispose() {
    parser.dispose()
    super.dispose()
  }
}
