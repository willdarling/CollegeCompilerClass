structure Desugar : sig

  val term : AST.term -> ULC.term

end = struct

  fun term (AST.Var x)           = ULC.Var x
    | term (AST.Abs (x, t))      = ULC.Abs (x, term t)
    | term (AST.App (t1, t2))    = ULC.App (term t1, term t2)
    | term (AST.Let (x, t1, t2)) = ULC.App (ULC.Abs (x, term t2), term t1)

(* Notice how helpful the type system is for implementing this
 * operation. This is SML at its very best.
 *)

end
          
