package net.foggin.rules

trait Memoisable[Context] {
  def memo[A](key : AnyRef, f : Context => Result[A, Context]) : Result[A, Context]
}

trait MemoisableRules extends Rules {
  type Context <: Memoisable[Context]
  
  def memo[A](key : AnyRef, f : Context => Result[A]) : Rule[A] = createRule[A] { ctx => ctx.memo(key, f) }
}

class EditableInput[A] extends Input[A, EditableInput[A]] with Memoisable[EditableInput[A]] {
  var index : Int = 0
  var next : Result[A, EditableInput[A]] = Failure[EditableInput[A]]
  
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
  def edit(pos : Int, delete: Int, insert : Seq[A]) : EditableInput[A] = {
    update(0, pos, delete, insert.elements)
  }
  
  private def update(index : Int, pos : Int, delete: Int, values : Iterator[A]) : EditableInput[A] = {
    if (pos > 0) {
      // delete all Failure results and all Success results
      // where the resulting element's index >= this index + pos
      map.retain { 
        case (_, Success(_, elem)) if elem.index < index + pos => true 
        case _ => false 
      }
      updateNext(index, pos - 1, delete, values)
      
    } else if (delete > 0) {
      // skip this element, return next element instead
      val Success(_, element) = next
      element.update(index, 0, delete - 1, values)
      
    } else if (values.hasNext) {
      // insert element
      val element = new EditableInput[A]
      element.next = Success(values.next(), this)
      element.updateNext(index, 0, 0, values)
      
    } else {
      // just update index
      updateNext(index, 0, 0, values)
    } 
  }
  
  private def updateNext(index : Int, pos : Int, delete: Int, values : Iterator[A]) : EditableInput[A] = {
    this.index = index
    next = next match {
      case Success(value, element) => Success(value, element.update(index + 1, pos, delete, values))
      case failure => failure
    }
    this
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
  lazy val number = (range('0', '9')+) ^^ literal ^^ (_ toInt)
  
  private def op(r : Rule[Any], f : (Int, Int) => Int) = r ^^^ f

  def evaluate = {
    expect(expr ~- !item | exception("Invalid expression"))
  }
}
  
object ExampleUsage extends IncrementalEvaluator with Application {
  var input = new EditableInput[Char]
  
  // set up initial text and evaluate
  input = input.edit(0, 0, "7 + 5 * (5+ 6 / 2 - 1)")
  println(evaluate(input))
  
   // change to "7 + (5 + 1) * (5+ 6 / 2 - 1)"
  input = input.edit(4, 1, "(5 + 1)")
  println(evaluate(input))
  
   // change to "(5 + 1) * (5+ 6 / 2 - 1)"
  input = input.edit(0, 4, "")
  println(evaluate(input))
}
