package net.foggin.rules.plugin

import org.eclipse.core.runtime.IAdaptable
import org.eclipse.jface, 
    jface.resource.ImageDescriptor, 
    jface.viewers.{IStructuredSelection, ITreeContentProvider, LabelProvider, SelectionChangedEvent, Viewer}
import org.eclipse.swt.widgets.Composite
import org.eclipse.ui.views.contentoutline.ContentOutlinePage

import scala._

class ScalaContentProvider extends ITreeContentProvider {
  
  private val empty = Array[Object]()
  
  // @see org.eclipse.jface.viewers.IContentProvider
  def dispose() { }

  // @see org.eclipse.jface.viewers.ITreeContentProvider
  def getChildren(parentElement : Object ) : Array[Object] = parentElement match {
    case ScalaElement(_, value : Object, _) => getChildren(value)
    case AnnotatedDefinition(_, _, definition) => getChildren(definition)
    case ObjectDefinition(_, _, template) => getChildren(template)
    case ClassDefinition(_, _, _, _, _, _, _, template) => getChildren(template)
    case ClassTemplate(_, _, _, _, Some(templateBody)) => getChildren(templateBody)
    case TraitDefinition(_, _, _, _, Some(templateBody)) => getChildren(templateBody)
    case TemplateBody(_, _, statements) => statements.toArray[Object]
    case Packaging(_, statements) => statements.toArray[Object]
    case _ => empty
  }

  // @see org.eclipse.jface.viewers.IStructuredContentProvider
  def getElements(inputElement : Object) : Array[Object] = inputElement match {
    case CompilationUnit(_, statements) => statements.toArray[Object]
    case _ => Array[Object]()
  }

  // @see org.eclipse.jface.viewers.ITreeContentProvider
  def getParent(element : Object) : Object = null

  // @see org.eclipse.jface.viewers.ITreeContentProvider
  def hasChildren(element : Object ) : Boolean = getChildren(element).length > 0

  // @see org.eclipse.jface.viewers.IContentProvider
  def inputChanged(viewer : Viewer, oldInput : Object, newInput : Object) { }
}

class ScalaLabelProvider extends LabelProvider {
  override def getText(element : Object) : String = element match {
    case ScalaElement(_, value : Object, _) => getText(value)
    case ImportStatement(imports) => "import ..."
    case AnnotatedDeclaration(_, _, declaration) => getText(declaration)
    case ValDeclaration(ids, _) => "val " + ids.mkString(", ")
    case VarDeclaration(ids, _) => "var " + ids.mkString(", ")
    case FunctionDeclaration(id, _, _, _, _) => "def " + id
    case TypeDeclaration(id, _, _, _) => "type " + id
    case AnnotatedDefinition(_, _, definition) => getText(definition)
    case ObjectDefinition(true, id, _) => "case object " + id
    case ObjectDefinition(false, id, _) => "object " + id
    case ClassDefinition(true, id, _, _, _, _, _, _) => "case class " + id
    case ClassDefinition(false, id, _, _, _, _, _, _) => "class " + id
    case TraitDefinition(id, _, _, _, _) => "trait " + id
    case ImplicitDefinition(definition) => getText(definition)
    case ValPatternDefinition(patterns, _, _) => "val " + patterns.map(getText(_)).mkString(", ")
    case VarPatternDefinition(patterns, _, _) => "var " + patterns.map(getText(_)).mkString(", ")
    case VarDefaultDefinition(ids, _) => "var " + ids.mkString(", ")
    case FunctionDefinition(id, _, _, _, _, _) => "def " + id
    case ProcedureDefinition(id, _, _, _, _) => "def" + id
    case ConstructorDefinition(_, _, _) => "this"
    case TypeDefinition(id, _, _) => "type " + id
    case Packaging(qualId, _) => "package " + qualId.mkString(".")
    case VariablePattern(id) => id
    case Name(id) => id
    case _ => super.getText(element)
  }
}

class ScalaOutliner(editor : ScalaEditor) extends ContentOutlinePage {
  
  private var controlCreated = false
  private var compilationUnit : CompilationUnit = null
  
  def show(compilationUnit : CompilationUnit) { 
    this.compilationUnit = compilationUnit
    if (controlCreated) getTreeViewer().setInput(compilationUnit)
  }
  
  override def createControl(parent : Composite) {
    super.createControl(parent)
    getTreeViewer().setContentProvider(new ScalaContentProvider)
    getTreeViewer().setLabelProvider(new ScalaLabelProvider)
    getTreeViewer().addSelectionChangedListener(this)
    if (compilationUnit ne null) getTreeViewer().setInput(compilationUnit)
    controlCreated = true
  }
  
  override def selectionChanged(event : SelectionChangedEvent) {
    super.selectionChanged(event)
    val selection = event.getSelection().asInstanceOf[IStructuredSelection];
    if (selection.isEmpty()) editor.resetHighlightRange()
    else selection.getFirstElement match {
       case element : ScalaElement[_] => highlight(element)
       case _ => 
    }
  }
  
  private def highlight(element : ScalaElement[_]) = try {
    editor.setHighlightRange(element.start, element.length, true)
  } catch {
    case x : IllegalArgumentException =>
  }

}
