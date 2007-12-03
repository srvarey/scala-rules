package net.foggin.rules.scala.test;

import java.io.{BufferedReader, File, FileReader, Reader}


object ReadFiles extends ScalaParser[ReaderInput] with Application {
  //process(new File("src/net/foggin/rules"))
  process(new File("../scala-trunk/src/compiler"))
  //process(new File("src/net/foggin/rules/scala/test/TestIncrementalScalaParser.scala"))
  //process(new File("../scala-trunk/src/compiler/scala/tools/nsc/ScriptRunner.scala"))
  
  def process(file : File) {
    if (file.isDirectory) file.listFiles().foreach(process)
    else if (file.getName.endsWith(".scala")) read(file)
  }
  
  def read(file : File) {
    print(file + "...")
    val input = new ScalaInput(ReaderInput.fromFile(file))
    val result = compilationUnit(input)
    result match {
      //case Success(value, rest) => println(value + "\nRemaining = \"" + rest)//.mkString("") + "\"")
      case Success(value, rest) if rest.mkString("") != "" => error(value + "\nRemaining = \"" + rest.mkString("") + "\"")
      case Success(value, rest) => println("Success!")
      case _ => error("Failure!")
    }
  }
}



trait DefaultMemoisable[Context <: Memoisable[Context]] extends Memoisable[Context] {
  
  self : Context =>

  protected val map = new _root_.scala.collection.mutable.HashMap[AnyRef, Result[Any, Context]]

  def memo[T](key : AnyRef, f : Context => Result[T, Context]) = {
    map.getOrElseUpdate(key, compute(key, f)).asInstanceOf[Result[T, Context]]
  }
  
  protected def compute[T](key : AnyRef, f : Context => Result[T, Context]) = {
    val result = f(this)
    //if (result.isSuccess) println(key + " -> " + result)
    //println(key + " -> " + result + " at " + this)
    result
  }
}


class ReaderInput(reader : Reader, val index : Int) extends Input[Char, ReaderInput] with DefaultMemoisable[ReaderInput] {
  
  def this(reader : Reader) = this(reader, 0)

  lazy val next = reader.read() match {
    case -1 => 
      //println("<EOF>@" + index)
      Failure[ReaderInput]
    case ch => 
      //println(ch.asInstanceOf[Char] + "@" + index)
      Success(ch.asInstanceOf[Char], new ReaderInput(reader, index + 1))
  }
  
  def close() = reader.close()
  
  override def toString = "@" + index
}

object ReaderInput {
  def fromFile(file : File) = new ReaderInput(new BufferedReader(new FileReader(file)))
}

