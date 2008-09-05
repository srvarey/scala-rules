package scalax.rules.scalasig

case class Foo[T](bar : T) {
  @throws(classOf[java.io.IOException])
  def baz() = "Hello"
}

object ScalaSigTest {
  def main(args : Array[String]) {
    //DefaultMemoisable.debug = true
    
    //val byteCode = ByteCode.forClass(classOf[Option[_]]) // pick a class... any class
    val byteCode = ByteCode.forClass(classOf[Foo[_]])
    val classFile = ClassFileParser.parse(byteCode)

    /*
    println("ClassFile version: " + classFile.majorVersion + "." + classFile.minorVersion)
    println("Class: " + classFile.className)
    println("Superclass: " + classFile.superClass)
    println("Interfaces: " + classFile.interfaces.mkString(", "))
    println("Constant pool:")
    val constantPool = classFile.header.constants
    for (i <- 1 to constantPool.size) println(i + "\t" + constantPool(i))
    */

    val Some(attribute) = classFile.attribute("ScalaSig")
    val scalaSig = ScalaSigAttributeParsers.parse(attribute.byteCode)

    //println(scalaSig)
    
    for (c <- scalaSig.topLevelClass) ScalaSigPrinter.printSymbol(c)
    println
    for (o <- scalaSig.topLevelObject) ScalaSigPrinter.printSymbol(o)
  }
}

