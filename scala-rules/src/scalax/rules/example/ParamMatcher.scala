// -----------------------------------------------------------------------------
//
//  Scalax - The Scala Community Library
//  Copyright (c) 2005-8 The Scalax Project. All rights reserved.
//
//  The primary distribution site is http://scalax.scalaforge.org/
//
//  This software is released under the terms of the Revised BSD License.
//  There is NO WARRANTY.  See the file LICENSE for the full text.
//
// -----------------------------------------------------------------------------

package scalax.rules.example

class ParamMatcher extends Rules {
  type S = Map[String, String]
  
  implicit def param(name : String) = from[S] { params => 
    if (params.contains(name)) Success(params, params(name)) 
    else Failure 
  }
  
  def intParam(name : String) = param(name) ^^? { case string if string matches "\\d+" => string.toInt }
}

object ParamMatcherExample extends ParamMatcher with Application {
  val params = Map("foo" -> "123", "bar" -> "Hello!")
  
  // rule matching "foo" as an int parameter and "bar" as a string parameter
  val foobar1 = intParam("foo") ~ "bar" ^^ { case foo ~ bar => sayFooBar(foo, bar) }
  
  // a shorter way of defining the previous rule
  val foobar2 = intParam("foo") ~ "bar" ^~^ sayFooBar
  
  // using for comprehensions
  val foobar3 = for {
    foo <- intParam("foo")
    bar <- param("bar")
  } yield sayFooBar(foo, bar)
  
  def sayFooBar(foo : Int, bar : String) {
    println("foo is "+foo+" and bar is "+bar)
  }
  
  foobar1(params)
  foobar2(params)
  foobar3(params)
}
