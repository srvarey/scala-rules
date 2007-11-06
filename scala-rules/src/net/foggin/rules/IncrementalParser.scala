package net.foggin.rules

trait Memoisable[Context] {
  def memo[A](key : AnyRef, f : Context => Result[A, Context]) : Result[A, Context]
}

trait MemoisableRules extends Rules {
  type Context <: Memoisable[Context]
  
  def memo[A](key : AnyRef, f : Context => Result[A]) : Rule[A] = createRule[A] { ctx => ctx.memo(key, f) }
}

class EditableInput[A](var next : Result[A, EditableInput[A]]) 
    extends Input[A, EditableInput[A]] with Memoisable[EditableInput[A]] {
  
  def this() = this(Failure[EditableInput[A]])
  var index : Int = 0
  //var next : Result[A, EditableInput[A]] = Failure[EditableInput[A]]
  
  private val map = new scala.collection.mutable.HashMap[AnyRef, Result[Any, EditableInput[A]]]
  
  def memo[B](key : AnyRef, f : EditableInput[A] => Result[B, EditableInput[A]]) : Result[B, EditableInput[A]] = {
    map.getOrElseUpdate(key, {
      val result = f(this)
      
      // only for debugging purposes
      result match {
        case Success(value, element) => println(key + " -> " + value)
        case _ =>
      }
      result
    }).asInstanceOf[Result[B, EditableInput[A]]]
  }
  
  /**
   * Specifies a change to the input.
   *
   * @param pos number of elements before the change
   * @param deleted number of elements deleted
   * @param inserted values to insert
   */
  def edit(pos : Int, delete: Int, insert : Seq[A]) {
    // can do this instead from Scala 2.6.1. on
    //edit(0, pos, delete, insert.elements)

    var values = insert.elements
    
    var current = this
    while (current ne null) {
      
      // delete all Failure results up to pos
      // and all Success results up to pos that point beyond pos
      if (current.index <= pos) current.map.retain { 
        case (_, Success(_, elem)) if elem.index < pos => true 
        case _ => false 
      }
      
      // delete elements
      if (current.index == pos) for (_ <- 1 to delete) current.next match {
        case Success(_, element) => current.next = element.next
        case _ => 
      }
        
      // insert element
      if (current.index >= pos && values.hasNext) {
        current.next = Success(values.next(), new EditableInput(current.next))
      }
        
      current.next match {
        case Success(_, element) => 
          element.index = current.index + 1
          current = element
        case _ => 
          current = null
      }
    }
  }
  

  /** Tail-recursive function.  Will only work from Scala 2.6.1. */
  private def edit(index: Int, pos : Int, delete: Int, values : Iterator[A]) {
    // update index
    this.index = index
    
    // delete all Failure results up to pos
    // and all Success results up to pos that point beyond pos
    if (index <= pos) map.retain { 
      case (_, Success(_, elem)) if elem.index < pos => true 
      case _ => false 
    }
    
    // delete elements
    if (index == pos) for (_ <- 1 to delete) next match {
      case Success(_, element) => next = element.next
      case _ => 
    }
      
    // insert element
    if (index >= pos && values.hasNext) {
      next = Success(values.next(), new EditableInput(next))
    }
      
    // tail-recursive call to next element
    next match {
      case Success(_, element) => element.edit(index + 1, pos, delete, values)
      case _ => ()
    }
  }
}

trait IncrementalParser[A] extends Parser[A] with MemoisableRules {
  type Context = EditableInput[A]
}

trait IncrementalScanner extends Scanner with MemoisableRules {
  type Context = EditableInput[Char]
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
  var input = new EditableInput[Char]
  
  // set up initial text and evaluate
  input.edit(0, 0, "7 + 5 * (5+ 6 / 2 - 1)")
  println(evaluate(input))
  
   // change to "7 + (5 + 1) * (5+ 6 / 2 - 1)"
  input.edit(4, 1, "(5 + 1)")
  println(evaluate(input))
  
   // change to "(5 + 1) * (5+ 6 / 2 - 1)"
  input.edit(0, 4, "")
  println(evaluate(input))
}
