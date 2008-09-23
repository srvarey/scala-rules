package scalax.rules.scalasig

case class Foo[T](bar : T) {
  @throws(classOf[java.io.IOException])
  def baz() = "Hello"
}

object ScalaSigTest {
  def main(args : Array[String]) {
    //DefaultMemoisable.debug = true
    
    //val byteCode = ByteCode.forClass(classOf[Option[_]]) // pick a class... any class
    //val byteCode = ByteCode.forClass(classOf[Foo[_]])
    //val classFile = ClassFileParser.parse(byteCode)
    //val Some(attribute) = classFile.attribute("ScalaSig")
    //val scalaSig = ScalaSigAttributeParsers.parse(attribute.byteCode)

    //println(scalaSig)
    
    val Some(scalaSig) = ScalaSigParser.parse(classOf[Foo[_]])
    
    for (c <- scalaSig.topLevelClass) ScalaSigPrinter.printSymbol(c)
    println
    for (o <- scalaSig.topLevelObject) ScalaSigPrinter.printSymbol(o)
  }
}

