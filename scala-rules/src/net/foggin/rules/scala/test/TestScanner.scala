package net.foggin.rules.scala.test

trait TestScanner extends Scanner with Application {
  type Context = ArrayInput[Char]
  
  def check[A](input : String, actual : Result[A], expected : Result[A]) {
    (expected, actual) match {
      case (Success(ea, es), Success(aa, as)) if ea == aa && es.toString == as.toString => ()
      case (e, a) if e == a => ()
      case _ => error ("Input: " + input + 
        "\nExpected result: " + expected + 
        "\nActual result: " + actual)
    }
  }
  
  def checkFailure[A](rule : Rule[A])(input : String *) {
    for (i <- input) check(i, rule(i), Failure[Context])
  }
  
  def checkRule[A](rule : Rule[A])(expect : (String, Result[A]) *) {
    for ((input, result) <- expect) check(input, rule(input), result)
  }
  
  implicit def anyToSuccess[A](a : A) : Result[A] = Success(a, "")
  
  implicit def tripleToSuccess[A](triple : ((String, A), String)) : (String, Result[A]) = 
    triple match { case ((input, a), rest) => input -> Success(a, rest) }
}

