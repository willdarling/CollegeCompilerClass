structure Desugar : sig

  val term : AST.term -> MULC.term

end = struct

  structure M = MULC
  structure A = AST

  fun term (A.Var x)            = M.Var x
    | term (A.Abs (xs,t1))      = M.Abs (xs, term t1)
    | term (A.App ts)           = 
        let
           fun lp [] = []
             | lp (t::ts) = (term t)::(lp ts) 
         in
           M.App (lp ts)
         end 
    | term (A.Let (x,t1,t2))    = M.App [M.Abs (x::[], term t2), term t1]
    | term (A.LetRec (x,t1,t2)) = 
        let
          val fix = M.App [M.Abs (["f"], M.Abs (["x"], M.App [M.Var "f", M.Abs (["y"], M.App [M.Var "x",M.Var "x",M.Var "y"])])), 
                           M.Abs (["x"], M.App [M.Var "f", M.Abs (["y"], M.App [M.Var "x",M.Var "x",M.Var "y"])])]
        in
          M.App [M.Abs (x::[], term t2), M.App [fix, M.Abs (x::[], term t1)]]
        end
    | term (A.True)             = M.Abs (["t","f"], M.Var "t")
    | term (A.False)            = M.Abs (["t","f"], M.Var "f")
    | term (A.If (t1,t2,t3))    = M.App [term t1, term t2, term t3]
    | term (A.Not t1)           = 
        let
          val nott = M.Abs (["b"], M.App [M.Var "b", term A.False, term A.True])
        in
          M.App [nott, term t1]
        end
    | term (A.And (t1,t2))      = 
        let
           val andd = M.Abs (["b","c"], M.App [M.Var "b", M.Var "c", term A.False]) 
         in
           M.App [andd, term t1, term t2]
         end 
    | term (A.Or (t1,t2))       =
        let
          val orr = M.Abs (["b","c"], M.App [M.Var "b", term A.True, M.Var "c"])
        in
          M.App [orr, term t1, term t2]
        end
    | term (A.Nat i)            = 
        let
           fun lp 0 = M.Var "z"
             | lp i = M.App [M.Var "s", lp (i-1)]
         in
           M.Abs (["s","z"], lp i)
         end 
    | term (A.Add (t1,t2))      =
        let
          val plus = M.Abs (["m","n","s","z"], 
                     M.App [M.Var "m", M.Var "s", M.App [M.Var "n", M.Var "s", M.Var "z"]])
        in
          M.App [plus, term t1, term t2]
        end
    | term (A.Mult (t1,t2))     =
        let
          val times = M.Abs (["m","n"], M.App [M.Var "m", 
                                               M.App [M.Abs (["m","n","s","z"], M.App [M.Var "m", 
                                                                                M.Var "s", 
                    (* as clean a way of writing plus as I could figure out *)  M.App [M.Var "n", 
                                                                                       M.Var "s", 
                                                                                       M.Var "z"]]),
                                                      M.Var "n"], 
                                               term (A.Nat 0)])
        in
          M.App [times, term t1, term t2]
        end
    | term (A.Pair (t1,t2))     =
        let
          val pair = M.Abs (["f","s","b"], M.App [M.Var "b",M.Var "f",M.Var"s"])
        in
          M.App [pair, term t1, term t2]
        end
    | term (A.Select1 t1)       =
        let
          val fst = M.Abs (["p"], M.App [M.Var "p", term A.True])
        in
          M.App [fst, term t1]
        end
    | term (A.Select2 t1)       =
        let
          val snd = M.Abs (["p"], M.App [M.Var "p", term A.False])
        in
          M.App [snd, term t1]
        end
    | term (A.ID x)             = M.Abs ([x], M.Var x)
      
end
          
