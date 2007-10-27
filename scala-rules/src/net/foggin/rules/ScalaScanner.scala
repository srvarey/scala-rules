package net.foggin.rules

abstract class ScalaScanner extends Scanner {

  val decimalDigit = choice("0123456789") ^^ (_ - 48)
  val octalDigit = decimalDigit.filter(_ < 8)
  val hexDigit = decimalDigit | choice("ABCDEF") ^^ (_ - 55) | choice("abcdef") ^^ (_ - 87)

  val unicodeEscape = "\\u" -~ hexDigit >> nextHexDigit >> nextHexDigit >> nextHexDigit ^^ { _.asInstanceOf[Char] }
  val octalEscape = '\\' -~ octalDigit >> nextOctalDigit >> nextOctalDigit ^^ { _.asInstanceOf[Char] }

  def nextOctalDigit(n : Int) = octalDigit ^^ (n * 8 + _)
  def nextHexDigit(n : Int) = hexDigit ^^ (n * 16 + _)

  val anyChar = unicodeEscape | octalEscape | item
  val idStart = anyChar.filter(Character isJavaIdentifierStart)
  val idPart = anyChar.filter(Character isJavaIdentifierPart)
  val opChar = (!idPart | choice(" \t\n\r(){}[]")) -~ anyChar

  val name = idStart ~++ (idPart*) ^^ literal
  val operator = (opChar+) ^^ literal
  val special = "(" | ")" | "{" | "}"
  val keyword = ("package" | "import" | "object") ~- (!idPart)
  val other = (item filter { ch => !(ch isWhitespace) } +) ^^ literal
  val stringLiteral = '"' -~ (item filter (_ != '"')*) ~- '"' ^^ literal

  val token = (whitespace?) -~ (special | keyword | stringLiteral | comment | operator | name | other)
  val tokens = (token*) ~- whitespace

  val line = ((!newline -~ item)*) ^^ literal
  val lines = line ~++ (newline -~ line *)

  lazy val comment : Rule[String] = "/*" -~ ((!"*/") -~ (comment | anyChar)).* ~- "*/" ^^ literal

}

object TestScalaScanner extends ScalaScanner with Application {
  type Context = ArrayInput[Char]

  println(unicodeEscape("\\u0030"))
  println(octalEscape("\\060"))

  val input = """
    package a.b.c

    /** my comment */
    object Hello extends Application {
      println("Hello World!")
    }
    """

  println(tokens(input))
  println(lines(input))
}

 