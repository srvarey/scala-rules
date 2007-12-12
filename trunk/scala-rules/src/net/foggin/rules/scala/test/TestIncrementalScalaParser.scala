package net.foggin.rules.scala.test;

object TestIncrementalScalaParser extends ScalaParser[DefaultIncrementalInput] with Application {
  
  val incrementalInput = new DefaultIncrementalInput
  val input = new ScalaInput(incrementalInput)
  
  IncrementalInput.debug = true

  val tokens = view((space*) -~ (comment | nl -^ "{nl}" | literal | reservedId | id | delimiter)) _

  val line = memo("line", newline -^ "" | (!newline -~ item +) ~- (newline?) ^^ toString)
  val lines = view(line) _

  def printTokens() {
    println; println("Tokens: ")
    println(tokens(input).mkString(", "))
  }

  def printCompilationUnit() {
    println; println("Compilation Unit: ")
    println(compilationUnit(input))
  }
  
  def printLines() {
    println; println("Lines: ")
    println(lines(input).mkString("\n"))
  }

  // set up initial text
  incrementalInput.edit(0, 0, """
    package a.b.c
    
    /** my comment */
    object Hello extends Application {
      println("Hello World!")
    }
    """)

  //printTokens()
  //printLines()
  printCompilationUnit()

 // insert something
 incrementalInput.edit(19, 0, """
   class Dummy {
     val question = <element with="attribute">and some text</element>
     val answer = 42
   }
   """)

   //printTokens()
   //printLines()
   printCompilationUnit()
} 

