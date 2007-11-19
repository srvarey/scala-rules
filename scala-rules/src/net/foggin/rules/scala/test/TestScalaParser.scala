package net.foggin.rules.scala.test


object TestScalaParser extends ScalaParser with Application {
//checkRule(path)(
//    "A.bc.this" -> (List(IdElement("A"), IdElement("bc"), ThisElement)), 
//    "super[Def].gh" -> List(SuperElement(Some("Def")), IdElement("gh")))
    
//checkRule(stableId)(
//    "super[Def].gh" -> List(SuperElement(Some("Def")), IdElement("gh")))
    
val scanner = new ScalaScanner {
  type Context = ArrayInput[Char]
}

  def check[A](input : String, result : Result[A], expected : A) {
    result match {
      case Success(actual, rest) if actual == expected => ()
      case actual => error ("Input: " + input + 
        "\nExpected Success: " + expected + 
        "\nActual result: " + actual)
    }
  }
  
  def checkFailure[A](rule : Rule[A])(inputs : String *) {
    for (input <- inputs) {
      val tokens = scanner.tokens(new ArrayInput(input.toArray))
      val actual = rule(tokens)
      if (actual.isSuccess) error ("Input: " + input + 
        "\nExpected Failure" + 
        "\nActual result: " + actual)
    }
  }
  
  def checkRule[A](rule : Rule[A])(expect : (String, A) *) {
    for ((input, result) <- expect) {
      val tokens = scanner.tokens(new ArrayInput(input.toArray))
      check(input, rule(tokens), result)
    }
  }
  

checkFailure(stableId)("A.bc.this")

checkRule(typeSpec)(
    "A" -> TypeDesignator(Nil, "A"),
    "A.B" -> TypeDesignator(List(Name("A")), "B"),
    "A.type" -> SingletonType(List(Name("A"))),
    "(A, \nB)" -> TupleType(List(TypeDesignator(Nil, "A"), TypeDesignator(Nil, "B"))),
    "(A, )" -> TupleType(List(TypeDesignator(Nil, "A"))),
    "A#B[C, D]" -> ParameterizedType(
        TypeProjection(TypeDesignator(Nil, "A"), "B"), 
        List(TypeDesignator(Nil, "C"), TypeDesignator(Nil, "D"))),
    "A with B" -> CompoundType(TypeDesignator(Nil, "A"))(TypeDesignator(Nil, "B")),
   "A => B" -> FunctionType(List(ParameterType(false, TypeDesignator(Nil, "A"), false)), TypeDesignator(Nil, "B")),
   "() => B" -> FunctionType(List(), TypeDesignator(List(), "B")),
    "(=> A, B*) => C" -> FunctionType(List(ParameterType(true, TypeDesignator(Nil, "A"), false), ParameterType(false, TypeDesignator(Nil, "B"), true)), TypeDesignator(Nil, "C")),
    "A B C" -> InfixType(TypeDesignator(Nil, "A"))("B", TypeDesignator(Nil, "C"))
 )
 
 checkRule(dcl)(
     "val a : A" -> ValDeclaration("a" :: Nil, TypeDesignator(Nil, "A")),
     "val a, b, c : A" -> ValDeclaration("a" :: "b" :: "c" :: Nil, TypeDesignator(Nil, "A")),
     "var a : A" -> VarDeclaration("a" :: Nil, TypeDesignator(Nil, "A")),
     "var a, b, c : A" -> VarDeclaration("a" :: "b" :: "c" :: Nil, TypeDesignator(Nil, "A")),
     
     "def a[B, C](b : => B, c : C*)(implicit d : D) : A" -> FunctionDeclaration("a",
         Some(List(
             TypeParameter("B",None,None,None), 
             TypeParameter("C",None,None,None))),
         List(List(
             Parameter(List(), "b", Some(ParameterType(true, TypeDesignator(List(),"B"), false))), 
             Parameter(List(), "c", Some(ParameterType(false, TypeDesignator(List(),"C"), true))))), 
         Some(List(
             Parameter(List(), "d", Some(ParameterType(false, TypeDesignator(List(),"D"), false))))),
         Some(TypeDesignator(List(),"A"))),
         
     "type A[+B <: C, -D >: E, F <% G] >: H <: I" -> TypeDeclaration("A", 
         Some(List(
             VariantTypeParameter(Covariant, TypeParameter("B", None, Some(TypeDesignator(List(), "C")), None)), 
             VariantTypeParameter(Contravariant, TypeParameter("D", Some(TypeDesignator(List(), "E")), None, None)), 
             VariantTypeParameter(Invariant, TypeParameter("F", None, None, Some(TypeDesignator(List(), "G")))))), 
         Some(TypeDesignator(List(), "H")),
         Some(TypeDesignator(List(), "I")))
     )
     
     checkRule(importStat)(
         "import A.B, C._" -> ImportStatement(List(
             Import(List(Name("A")), List(ImportSelector("B", None))), 
             Import(List(Name("C")), List(ImportSelector("_", None))))),
             
         "import A.{b => c, _}" -> ImportStatement(List(
             Import(List(Name("A")),List(
                 ImportSelector("b",Some("c")), 
                 ImportSelector("_",None)))))
     )
     
     checkRule(expr)(
         "\"string\"" -> StringLiteral("string"),
         "'symbol" -> SymbolLiteral('symbol),
         "_" -> Underscore,
         "(1, 2, )" -> TupleExpression(List(IntegerLiteral(1), IntegerLiteral(2))),
         "1.toString" -> DotExpression(IntegerLiteral(1), Name("toString")),
         
         "a[B, C]" -> ExpressionTypeArgs(Name("a"),
             List(TypeDesignator(List(), "B"), TypeDesignator(List(), "C"))),
             
         "a(1, 2)" -> ApplyExpression(Name("a"), List(IntegerLiteral(1), IntegerLiteral(2))),
         
         "if (a) 1 else 2" -> IfExpression(Name("a"),IntegerLiteral(1),Some(IntegerLiteral(2))),
         
         "while (true) println(\"Hello\")" -> WhileExpression(True, ApplyExpression(Name("println"),List(StringLiteral("Hello")))),
         
         "do println(\"Hello\") while(true)" -> DoExpression(ApplyExpression(Name("println"),List(StringLiteral("Hello"))), True),
         
         "throw x" -> Throw(Name("x")),
         "return x" -> Return(Some(Name("x"))),
         "return" -> Return(None),
         
         "try { 1 } catch { case e => println(e) } finally { println(\"finally!\") }" -> TryCatchFinally(
             Block(List(),Some(IntegerLiteral(1))),
             Some(CaseClauses(List(
                 CaseClause(VariablePattern("e"), None, Block(
                     List(), Some(ApplyExpression(Name("println"), List(Name("e"))))))))),
             Some(Block(List(), Some(ApplyExpression(Name("println"),List(StringLiteral("finally!"))))))),
             
          "for (i <- list; val j = i; if true) yield j" -> ForComprehension(List(
              Generator(VariablePattern("i"), Name("list"), None), 
              ValEnumerator(VariablePattern("j"), Name("i")), 
              Guard(True)), 
              true, Name("j")),
              
          "a = 1" -> SimpleAssignment("a",IntegerLiteral(1)),
          
          "a.b = 1" -> DotAssignment(Name("a"), "b", IntegerLiteral(1)),
          
          "a(b) = 1" -> Update(Name("a"), List(Name("b")), IntegerLiteral(1)),
          
          "a b" -> PostfixExpression(Name("a"),"b"),
          
          "1 + 2 * 3" -> InfixExpression("+", IntegerLiteral(1), InfixExpression("*", IntegerLiteral(2), IntegerLiteral(3))),
          
          "-1" -> PrefixExpression("-", IntegerLiteral(1)),
          
          "a _" -> Unapplied(Name("a"))
         
     )
     
     checkRule(pattern)(
         "_" -> Underscore,
         "1" -> IntegerLiteral(1),
         "x" -> VariablePattern("x"),
         "X" -> StableIdPattern(List(Name("X")), None, false),
         "x.y" -> StableIdPattern(List(Name("x"), Name("y")), None, false),
         "X(a, b)" -> StableIdPattern(List(Name("X")), Some(List(VariablePattern("a"), VariablePattern("b"))),false),
         "X(_*)" -> StableIdPattern(List(Name("X")), Some(List()), true),
         "(x, y)" -> TupleExpression(List(VariablePattern("x"), VariablePattern("y"))),
         "a ~ b" -> InfixPattern(VariablePattern("a"),List(("~",VariablePattern("b")))),
         "a @ (x, y)" -> AtPattern("a",TupleExpression(List(VariablePattern("x"), VariablePattern("y")))),
         "a : A" -> TypedVariablePattern("a", TypeDesignator(List(), "A")),
         "_ : A" -> TypePattern(TypeDesignator(List(), "A")),
         "1 | 2" -> OrPattern(List(IntegerLiteral(1), IntegerLiteral(2)))
     )
     
  checkRule(compilationUnit)("""
    package a.b
    
    class Hello {
      def hello() {
        println("Hello World")
      }
    }""" -> CompilationUnit(Some(List("a", "b")), List(
        AnnotatedDefinition(List(),List(),ClassDefinition("Hello",None,List(),None,
            ClassParamClauses(List(),None),
            ClassTemplate(None,None,List(),List(),Some(TemplateBody(None,None,List(
                AnnotatedDefinition(List(),List(),ProcedureDefinition("hello",None,List(List()),None,
                    Block(List(),Some(
                        ApplyExpression(Name("println"),List(StringLiteral("Hello World"))))))))))))))))
    
println("ScalaParser tests passed")
}

