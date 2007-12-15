package net.foggin.rules

trait IncrementalScanner extends Scanner with MemoisableRules {
  type Context <: IncrementalInput[Char, Context]
}

class DefaultIncrementalInput extends IncrementalInput[Char, DefaultIncrementalInput] {
  def element = new DefaultIncrementalInput
}

object IncrementalInput {
  var debug = false
}

trait IncrementalInput[A, Context <: IncrementalInput[A, Context]]
    extends Input[A, Context] 
    with DefaultMemoisable[Context] 
    with Ordered[Context] { self : Context =>

  var next : Result[A, Context] = Failure[Context]
  var index : Int = 0

  /** Create a new element. */
  def element : Context
  
  def compare(other : Context) = index - other.index

  override protected def onSuccess[T](key : AnyRef,  result : Success[T, Context]) { 
    if(IncrementalInput.debug) println(key + " -> " + result) 
  }

  /**
   * Specifies a change to the document.
   *
   * @param pos number of elements before the change
   * @param deleted number of elements deleted
   * @param inserted sequence of values to insert
   */
  def edit(pos : Int, deleted: Int, inserted : Seq[A]) {
    // can do this instead from Scala 2.6.1. on
    //edit(0, pos, deleted, inserted.elements)

    var values = inserted.elements
    var current = this
    var finished = false
    while (!finished) {
      if (current.index <= pos) current.cleanResults(pos)
      if (current.index == pos) current.deleteElements(deleted)
      if (current.index >= pos && values.hasNext) current.insert(values.next)
      
      if (current hasNextElement) {
        current.nextElement.index = current.index + 1
        current = current.nextElement
      } else {
        finished = true
      }
    }
  }

  /** Tail-recursive function.  Will only work from Scala 2.6.1. */
  private def edit(index: Int, pos : Int, deleted: Int, values : Iterator[A]) {
    this.index = index
    if (index <= pos) cleanResults(pos)
    if (index == pos) deleteElements(deleted)
    if (index >= pos && values.hasNext) insert(values.next)

    // recursive call to next element
    if (hasNextElement) nextElement.edit(index + 1, pos, deleted, values)
  }

  /** Delete all Failure results up to pos
   *  and all Success results up to pos that point beyond pos
   */
  protected def cleanResults(pos : Int) = map.retain { 
    case (_, Success(_, elem)) if elem.index < pos => true 
    case _ => false 
  }

  /** Delete elements */
  protected def deleteElements(count : Int) = for (_ <- 1 to count) delete
  
  /** Delete current element value */
  protected def delete() = if (hasNextElement) next = nextElement.next

  /** Insert an element */
  protected def insert(value : A) {
    val elem = element
    elem.next = next
    next = Success(value, elem)
  }
  
  protected def hasNextElement = next match {
    case Success(_, _) => true
    case _ => false
  }
  
  protected def nextElement = next match {
    case Success(_, element) => element
    case _ => throw new RuntimeException("No next element")
  }
  
  override def toString = "@" + index
}
