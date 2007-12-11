package net.foggin.rules.plugin

import org.eclipse.jface.text.source._
import org.eclipse.ui.editors.text._

import org.eclipse.swt.graphics.Color
import org.eclipse.swt.graphics.RGB
import org.eclipse.swt.widgets.Display

class ScalaEditor extends TextEditor {

  private val colours = new _root_.scala.collection.mutable.HashMap[RGB, Color]

  def colour(rgb: RGB) = colours.getOrElseUpdate(rgb, new Color(Display.getCurrent(), rgb))
  
  setSourceViewerConfiguration(new SourceViewerConfiguration() {
    override def getPresentationReconciler(sourceViewer : ISourceViewer) = new ScalaPresentationReconciler(ScalaEditor.this)
  })
  
  setDocumentProvider(new FileDocumentProvider())
  
  override def dispose() {
    for (colour <- colours.values) colour.dispose()
    super.dispose()
  }
}
