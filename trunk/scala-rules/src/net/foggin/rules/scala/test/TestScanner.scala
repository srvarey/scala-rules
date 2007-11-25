package net.foggin.rules.scala.test

trait TestScanner extends Scanner with Application {

  def input(string : String) : Context
  
  def checkSuccess[A](input : String, result : Result[A], expected : A) {
    result match {
      case Success(actual, rest) if actual == expected => ()
      case actual => fail(input, actual, expected, "")
    }
  }
  
  def check[A](input : String, actual : Result[A], expected : A, rest : String) {
    actual match {
      case Success(ea, es) => if (ea != expected && !es.toString.equals(rest)) 
        fail(input, actual, expected, rest)
      case _ => fail(input, actual, expected, rest)
    }
  }
  
  def fail[A](input : String, actual : Result[A], expected : A, rest : String) {
    error ("Input: " + input + 
      "\nExpected result: " + expected + 
      "\nWith remaining input: \"" + rest + "\"" +
      "\nActual result: " + actual)    
  }
  
  def checkFailure[A](rule : Rule[A])(inputs : String *) {
    for (string <- inputs) {
      val actual = rule(input(string))
      if (actual.isSuccess) error ("Input: " + string + 
        "\nExpected Failure" + 
        "\nActual result: " + actual)
    }
  }
  
  def checkRule[A](rule : Rule[A])(expect : (String, A) *) {
    for ((string, result) <- expect) {
      checkSuccess(string, rule(input(string)), result)
    }
  }
  
  def checkRuleWithRest[A](rule : Rule[A])(expect : ((String, A), String) *) {
    for (((string, result), rest) <- expect) {
      check(string, rule(input(string)), result, rest)
    }
  }
}

