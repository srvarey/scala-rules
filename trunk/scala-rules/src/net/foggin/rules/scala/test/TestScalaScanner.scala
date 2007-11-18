package net.foggin.rules.scala.test

object TestScalaScanner extends ScalaScanner with TestScanner {
  
  checkRule(unicodeEscape)("\\u0030" -> '0', "\\u21D2" -> '\u21D2')
  checkRule(octalEscape)("\\061" -> '1')
  checkRule(anyChar)("\\u0030" -> '0', "\\u21D2" -> '\u21D2')
  checkRule(opChar)("\\u21D2" -> '\u21D2')
  
  checkFailure(integerLiteral)("l", "L", "0x")
  
  checkRule(integerLiteral) (
      "0l" -> LongLiteral(0),
      "12 " -> IntegerLiteral(12) -> " ",
      "012" -> IntegerLiteral(10),
      "0x12" -> IntegerLiteral(18))
      
  checkFailure(opChar)(".", ";", "(", "[", "}")
  
  checkRule(opChar) (
      "+" -> '+',
      "-" -> '-',
      "*" -> '*',
      "/" -> '/')
   
      
  // check reserved words aren't ids
  checkFailure(id)(ReservedId.reserved.toList : _*)
  //checkFailure(id(false))(reservedOps.keys.toList : _*)
  
  checkRule(keyword)(
      "abstract" -> Keyword("abstract"),
      "_" -> Keyword("_"))
  
  checkRule(quoteId)("`yield`" -> "yield")
  
  checkRule(id)(
      "`yield`" -> IdToken(false, "yield"), 
      "yield1" -> IdToken(true, "yield1"), 
      "yield_+" -> IdToken(true, "yield_+"),
      "`\\u21D2`" -> IdToken(false, "\u21D2"))

  checkRule(floatLiteral)(
      "1f" -> FloatLiteral(1), 
      "1.0F" -> FloatLiteral(1), 
      "1.e2F" -> FloatLiteral(100),
      ".12E3f" -> FloatLiteral(.12E3f))

  checkRule(doubleLiteral)(
      "1D" -> DoubleLiteral(1), 
      "1.0" -> DoubleLiteral(1), 
      "1e2" -> DoubleLiteral(100),
      ".12E3D" -> DoubleLiteral(.12E3))

  println("Scanner tests passed")
}

object TestIncrementalScalaScanner extends ScalaScanner with IncrementalScanner with Application {
  val document = new EditableDocument[Char]
  val input = document.first

  val tokens = view(memo("token", token)) _

  val line = memo("line", newline -^ "" | (!newline -~ item +) ~- (newline?) ^^ toString)
  val lines = view(line) _

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