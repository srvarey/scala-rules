package net.foggin.rules

import Character._

abstract class ScalaScanner extends Scanner {

  val decimalDigit = range('0', '9') ^^ (_ - 48)
  val octalDigit = decimalDigit.filter(_ < 8)
  val hexDigit = decimalDigit | range('A', 'F') ^^ (_ - 55) | range('a', 'f') ^^ (_ - 87)

  val unicodeEscape = "\\u" -~ hexDigit >> hex >> hex >> hex ^^ { _.asInstanceOf[Char] }
  val octalEscape = '\\' -~ octalDigit >> oct >> oct ^^ { _.asInstanceOf[Char] }
 
  def dec(n : Int) = decimalDigit ^^ (n * 10 + _)
  def oct(n : Int) = octalDigit ^^ (n * 8 + _)
  def hex(n : Int) = hexDigit ^^ (n * 16 + _)
 
  def decN(n : Int) : Rule[Int] = dec(n) >> decN | success(n)
  def octN(n : Int) : Rule[Int] = oct(n) >> octN | success(n)
  def hexN(n : Int) : Rule[Int] = hex(n) >> hexN | success(n)

  val anyChar = unicodeEscape | octalEscape | item
  
  def unicode(category : Int) = anyChar filter (getType(_) == category)
  
  val upper = anyChar filter isUpperCase
  val lower = anyChar filter isLowerCase
  val opChar = !choice(".()[]{}") -~ choice("!#%&*+-/:<=>?@\\^|~") | unicode(MATH_SYMBOL) | unicode(OTHER_SYMBOL)
  
  val op = opChar+
  val idrest = !('_' ~ opChar) -~ (letter | digit *) ~ ('_' ~++ op ?) ^^ { case a ~ Some(b) => a ::: '_' :: b case a ~ None => a }
  val varid = lower ~++ idrest
  val plainid = upper ~++ idrest | varid | op
  val id = (plainid | '`' -~ (anyChar+) ~- '`') ^^ literal
    
  val zero = elem('0')
  val hexNumeral = "0x" -~ hexDigit >> hexN
  val octalNumeral = '0' -~ octalDigit >> octN
  val decimal = decimalDigit >> decN
  val decimalNumeral = '0' ^^^ 0 | decimal
  val integerLiteral = (hexNumeral | octalNumeral | decimalNumeral) ~- (choice("Ll")?)
  val floatingPointLiteral = 
    decimal ~- '.' ~ (decN(0)) ~ (exponentPart?) ~ (floatType?) ^^^ "123" |
    '.' ~ decimal ~ (exponentPart?) ~ (floatType?) ^^^ "123" |
    decimal ~ exponentPart ~ (floatType?) ^^^ "123" |
    decimal ~ (exponentPart?) ~ floatType ^^^ "123"
  val exponentPart = choice("eE") -~ (choice("+-")?) ~ decimal
  val floatType = choice("fFdD")

  val booleanLiteral = "true" | "false"

  val charEscapeSeq = 
      "\\b" ^^^ '\b' | 
      "\\t" ^^^ '\t' | 
      "\\n" ^^^ '\n' | 
      "\\f" ^^^ '\f' | 
      "\\r" ^^^ '\r' | 
      "\\\"" ^^^ '\"' | 
      "\\\'" ^^^ '\'' | 
      "\\\\" ^^^ '\\'

  val characterLiteral = '\'' -~ (charEscapeSeq | !choice("\'\n\r\\") -~ anyChar) ~- '\''
  val stringLiteral = 
      '\"' -~ ((charEscapeSeq | !choice("\"\n\r\\") -~ anyChar)*) ~- '\"' ^^ literal |
      "\"\"\"" -~ ((!"\"\"\"" -~ anyChar)*) ~- "\"\"\"" ^^ literal
  val symbolLiteral = '\'' ~ plainid
  
  val lineComment = "//" -~ ((!newline -~ item)*)
  
  val semi = (";" | newline) ~- (newline*)
   
  //val name = idStart ~++ (idPart*) ^^ literal
  val operator = (opChar+) ^^ literal
  val special = choice(".()[]{}")
  val keyword = ("package" | "import" | "object" | "extends") ~- !(anyChar filter isJavaIdentifierPart)
  val other = (item filter { ch => !(ch isWhitespace) } +) ^^ literal
  //val stringLiteral = '"' -~ (item filter (_ != '"')*) ~- '"' ^^ literal

  // note multi-line comments can nest
  lazy val comment : Rule[String] = "/*" -~ ((!"*/") -~ (comment | anyChar)).* ~- "*/" ^^ literal

}

abstract class IncrementalScalaScanner extends ScalaScanner with IncrementalScanner {
  val token = memo("token", (whitespace?) -~ (special | keyword | stringLiteral | comment | id | other))
  val tokens = view(token) _

  val line = memo("line", newline ^^^ "" | (!newline -~ item +) ~- (newline?) ^^ literal)
  val lines = view(line) _
}

object TestScalaScanner extends IncrementalScalaScanner with Application {
  var input = new EditableInput[Char]

  //println(unicodeEscape("\\u0030"))
  //println(octalEscape("\\060"))

  def printTokens() {
    println; println("Tokens: ")
    println(tokens(input).mkString("\"", "\", \"", "\""))
  }

  def printLines() {
    println; println("Lines: ")
    println(lines(input).mkString("\n"))
  }

  // set up initial text
  input = input.edit(0, 0, """
    package a.b.c
    
    /** my comment */
    object Hello extends Application {
      println("Hello World!")
    }
    """)

  printTokens()
  printLines()

 // insert something
 input = input.edit(19, 0, """
   class Dummy {
     val answer = 42
   }
   """)

  printTokens()
  printLines()
} 