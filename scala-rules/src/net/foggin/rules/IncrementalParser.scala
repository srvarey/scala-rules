package net.foggin.rules

trait Memoisable[Context] {
  def memo[A](key : AnyRef, f : Context => Result[A, Context]) : Result[A, Context]
}

trait MemoisableRules extends Rules {
  type Context <: Memoisable[Context]
  
  def memo[A](key : AnyRef, f : Context => Result[A]) : Rule[A] = createRule[A] { ctx => ctx.memo(key, f) }
}

class EditableDocument[A] {
  
  var debug = false
  val first = new Element(Failure[Element])
  
  class Element(var next : Result[A, Element]) 
      extends Input[A, Element] 
      with Memoisable[Element] 
      with Ordered[Element] {

    var index : Int = 0

    private val map = new scala.collection.mutable.HashMap[AnyRef, Result[Any, Element]]

    def compare(other : Element) = index - other.index
    
    def memo[B](key : AnyRef, f : Element => Result[B, Element]) : Result[B, Element] = {
      map.getOrElseUpdate(key, {
        val result = f(this)
        if(debug) result match {
          case Success(value, element) => println(key + " -> " + value)
          case _ =>
        }
        result
      }).asInstanceOf[Result[B, Element]]
    }

    /** Tail-recursive function.  Will only work from Scala 2.6.1. */
    private def edit(index: Int, pos : Int, deleted: Int, values : Iterator[A]) {
      this.index = index
      if (index <= pos) cleanResults(pos)
      if (index == pos) delete(deleted)
      if (index >= pos && values.hasNext) insert(values.next)
    
      // recursive call to next element
      next match {
       case Success(_, element) => element.edit(index + 1, pos, deleted, values)
       case _ => ()
     }
   }
    
    /** Delete all Failure results up to pos
     *  and all Success results up to pos that point beyond pos
     */
    def cleanResults(pos : Int) = map.retain { 
      case (_, Success(_, elem)) if elem.index < pos => true 
      case _ => false 
    }
    
    /** Delete elements */
    def delete(count : Int) = for (_ <- 1 to count) next match {
      case Success(_, element) => next = element.next
      case _ => ()
    }
    
    /** Insert an element */
    def insert(value : A) {
      next = Success(value, new Element(next))
    }
  }

  /**
   * Specifies a change to the document.
   *
   * @param pos number of elements before the change
   * @param deleted number of elements deleted
   * @param inserted values to insert
   */
  def edit(pos : Int, deleted: Int, inserted : Seq[A]) {
    // can do this instead from Scala 2.6.1. on
    //first.edit(0, pos, delete, insert.elements)

    var values = inserted.elements
    var current = first
    while (current ne null) {
      if (current.index <= pos) current.cleanResults(pos)
      if (current.index == pos) current.delete(deleted)
      if (current.index >= pos && values.hasNext) current.insert(values.next)
          
      current.next match {
        case Success(_, element) => 
          element.index = current.index + 1
          current = element
        case _ => 
          current = null
      }
    }
  }
}


trait IncrementalScanner extends Scanner with MemoisableRules {
  type Context = EditableDocument[Char]#Element
}

  
trait IncrementalEvaluator extends IncrementalScanner {
  lazy val expr : Rule[Int] = term ~*~ (op('+', _ + _) | op('-', _ - _))
  lazy val term : Rule[Int] = factor ~*~ (op('*', _ * _) | op('/', _ / _))
  lazy val factor : Rule[Int] = memo("factor", trim(number | '(' -~ expr ~- ')'))
  lazy val number = (range('0', '9')+) ^^ (toString(_).toInt)
  
  private def op(r : Rule[Any], f : (Int, Int) => Int) = r ^^^ f

  def evaluate = {
    expect(expr ~- !item | exception("Invalid expression"))
  }
}
  
object ExampleUsage extends IncrementalEvaluator with Application {
  val document = new EditableDocument[Char]
  val input = document.first
  
  document.debug = true
  
  // set up initial text and evaluate
  document.edit(0, 0, "7 + 5 * (5+ 6 / 2 - 1)")
  println(evaluate(input))
  
   // change to "7 + (5 + 1) * (5+ 6 / 2 - 1)"
  document.edit(4, 1, "(5 + 1)")
  println(evaluate(input))
  
   // change to "(5 + 1) * (5+ 6 / 2 - 1)"
  document.edit(0, 4, "")
  println(evaluate(input))
}
