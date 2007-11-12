package net.foggin.rules.test

object TestScalaParser extends ScalaParser with TestScanner {
//checkRule(path)(
//    "A.bc.this" -> (List(IdElement("A"), IdElement("bc"), ThisElement)), 
//    "super[Def].gh" -> List(SuperElement(Some("Def")), IdElement("gh")))
    
//checkRule(stableId)(
//    "super[Def].gh" -> List(SuperElement(Some("Def")), IdElement("gh")))
    
checkFailure(stableId)("A.bc.this")

checkRule(typeSpec)(
    "A" -> TypeDesignator(Nil, "A"),
    "A.B" -> TypeDesignator(List(Name("A")), "B"),
    "A.type" -> SingletonType(List(Name("A"))),
    "(A, B)" -> TupleType(List(TypeDesignator(Nil, "A"), TypeDesignator(Nil, "B"))),
    "(A, )" -> TupleType(List(TypeDesignator(Nil, "A"))),
    "A#B[C, D]" -> ParameterizedType(
        TypeProjection(TypeDesignator(Nil, "A"), "B"), 
        List(TypeDesignator(Nil, "C"), TypeDesignator(Nil, "D"))),
    "A with B" -> CompoundType(List(
        TypeDesignator(Nil, "A"), 
        TypeDesignator(Nil, "B")), None),
   "A => B" -> FunctionType(List((TypeDesignator(Nil, "A"),false)),TypeDesignator(Nil, "B")),
    "(=> A) => B" -> FunctionType(List((TypeDesignator(Nil, "A"),true)),TypeDesignator(Nil, "B")),
    "A B C" -> InfixType("B",TypeDesignator(Nil, "A"),TypeDesignator(Nil, "C"))
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
             Parameter("b",true,Some(TypeDesignator(List(),"B")),false,List()), 
             Parameter("c",false,Some(TypeDesignator(List(),"C")),true,List()))),
         Some(List(
             Parameter("d",false,Some(TypeDesignator(List(),"D")),false,List()))),
         Some(TypeDesignator(List(),"A"))),
         
     "type A[+B <: C, -D >: E, F <% G] >: H <: I" -> TypeDeclaration("A", List(
         VariantTypeParameter("B", None, Some(TypeDesignator(List(), "C")), None, Covariant), 
         VariantTypeParameter("D", Some(TypeDesignator(List(), "E")), None,None,Contravariant), 
         VariantTypeParameter("F", None, None, Some(TypeDesignator(List(), "G")), Invariant)),
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
         "\"string\"" -> Success(Literal("string"), ""),
         "'symbol" -> Success(Literal('symbol), ""),
         "_" -> Underscore,
         "(1, 2, )" -> TupleExpression(List(Literal(1), Literal(2))),
         "1.toString" -> DotExpression(Literal(1), Name("toString")),
         
         "a[B, C]" -> ExpressionTypeArgs(Name("a"),
             List(TypeDesignator(List(), "B"), TypeDesignator(List(), "C"))),
             
         "a(1, 2)" -> ApplyExpression(Name("a"), List(Literal(1), Literal(2))),
         
         "if (a) 1 else 2" -> IfExpression(Name("a"),Literal(1),Some(Literal(2))),
         
         "while (true) println(\"Hello\")" -> WhileExpression(Literal("true"), ApplyExpression(Name("println"),List(Literal("Hello")))),
         
         "do println(\"Hello\") while(true)" -> DoExpression(ApplyExpression(Name("println"),List(Literal("Hello"))), Literal("true")),
         
         "throw x" -> Throw(Name("x")),
         "return x" -> Return(Some(Name("x"))),
         "return" -> Return(None),
         
         "try { 1 } catch { case e => println(e) } finally { println(\"finally!\") }" -> TryCatchFinally(
             Block(List(),Some(Literal(1))),
             Some(CaseClauses(List(
                 CaseClause(VariablePattern("e"), None, Block(
                     List(), Some(ApplyExpression(Name("println"), List(Name("e"))))))))),
             Some(Block(List(), Some(ApplyExpression(Name("println"),List(Literal("finally!"))))))),
             
          "for (i <- list; val j = i; if true) yield j" -> ForComprehension(List(
              Generator(VariablePattern("i"), Name("list"), None), 
              ValEnumerator(VariablePattern("j"), Name("i")), 
              Guard(Literal("true"))), 
              true, Name("j")),
              
          "a = 1" -> SimpleAssignment("a",Literal(1)),
          
          "a.b = 1" -> DotAssignment(Name("a"), "b", Literal(1)),
          
          "a(b) = 1" -> Update(Name("a"), List(Name("b")), Literal(1))
         
     )
     
     checkRule(pattern)(
         "_" -> Underscore,
         "1" -> Success(Literal(1), ""),
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
         "1 | 2" -> OrPattern(List(Literal(1), Literal(2)))
     )
     
println("ScalaParser tests passed")
}
