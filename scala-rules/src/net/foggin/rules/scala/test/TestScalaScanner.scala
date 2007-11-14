package net.foggin.rules.scala.test

object TestScalaScanner extends ScalaScanner with TestScanner {
  
  checkRule(unicodeEscape)("\\u0030" -> '0', "\\u21D2" -> '\u21D2')
  checkRule(octalEscape)("\\061" -> '1')
  checkRule(anyChar)("\\u0030" -> '0', "\\u21D2" -> '\u21D2')
  checkRule(opChar)("\\u21D2" -> '\u21D2')
  
  checkFailure(integerLiteral)("l", "L", "0x")
  
  checkRule(integerLiteral) (
      "0l" -> 0,
      "12 " -> 12 -> " ",
      "012" -> 10,
      "0x12" -> 18)
      
  checkFailure(opChar)(".", ";", "(", "[", "}")
  
  checkRule(opChar) (
      "+" -> '+',
      "-" -> '-',
      "*" -> '*',
      "/" -> '/')
   
      
  // check reserved words aren't ids
  checkFailure(id)(keywords.keys.toList : _*)
  checkFailure(id)(reservedOps.keys.toList : _*)
  
  //checkRule(keyword)(keywords.keys.toList.map[String] { s => (s, s) } : _*)
  
  checkRule(keyword)(
      "abstract" -> "abstract",
      "_" -> "_")
  
  checkRule(quoteid)("`yield`" -> "yield")
  
  checkRule(id)(
      "`yield`" -> "yield", 
      "yield1" -> "yield1", 
      "yield_+" -> "yield_+",
      "`\\u21D2`" -> "\u21D2")

  checkRule(floatLiteral)(
      "1f" -> "1", 
      "1.0F" -> "1.0", 
      "1.e2F" -> "1.e2",
      ".12E3f" -> ".12E3")

  checkRule(doubleLiteral)(
      "1D" -> "1", 
      "1.0" -> "1.0", 
      "1e2" -> "1e2",
      ".12E3D" -> ".12E3")

  println("Scanner tests passed")
}

object TestIncrementalScalaScanner extends ScalaScanner with IncrementalScanner with Application {
  val document = new EditableDocument[Char]
  val input = document.first

  val tokens = view(memo("token", nl | semi | space ~- comment | separator |  literal | keyword | reservedOp | id)) _

  val line = memo("line", newline -^ "" | (!newline -~ item +) ~- (newline?) ^^ toString)
  val lines = view(line) _

  //var input = new EditableInput[Char]

  def printTokens() {
    println; println("Tokens: ")
    println(tokens(input).mkString(", "))
  }

  def printLines() {
    println; println("Lines: ")
    println(lines(input).mkString("\n"))
  }

  // set up initial text
  document.edit(0, 0, """
    package a.b.c
    
    /** my comment */
    object Hello extends Application {
      println("Hello World!")
    }
    """)

  printTokens()
  printLines()

 // insert something
 document.edit(19, 0, """
   class Dummy {
     val answer = 42
   }
   """)

  printTokens()
  printLines()
} 