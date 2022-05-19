structure Compile : sig

  val compile        : string -> AST.term * TypeCheck.ty * AST.term
  val compileUntyped : string -> AST.term * AST.term

end = struct

  fun compile program =
    let
      val tokens = Scan.scan program
      val ast    = Parse.parse tokens
      val ty     = TypeCheck.typeof ast
      val result = Eval.eval ast
    in
      (ast, ty, result)
    end

  fun compileUntyped program =
    let
      val tokens = Scan.scan program
      val ast    = Parse.parse tokens
      val result = Eval.eval ast
    in
      (ast, result)
    end

end
