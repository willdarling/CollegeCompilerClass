structure Eval : sig

  val eval  : AST.term -> AST.term
  val steps : AST.term -> AST.term list
  val step  : AST.term -> AST.term option
	  
end = struct

  structure A = AST

  fun step t =
    let
      fun lp (A.Var x) = NONE
        | lp (A.Abs _) = NONE
	      | lp (A.App (t1, t2)) = 
           (case step t1 
              of SOME t1' => SOME (A.App (t1', t2))
               | NONE => (case step t2 
                            of SOME t2' => SOME (A.App (t1, t2'))
                             | NONE => (case t1 
                                          of A.Abs(x, _, t) => SOME (Subst.subst {replaceThis = x, withThis = t2, inThis = t})
                                           | _ => NONE)))
        | lp (A.Let (x, t1, t2)) = 
           (case step t1 
              of SOME t1' => SOME (A.Let (x, t1', t2))
               | NONE => SOME (Subst.subst {replaceThis=x, withThis=t1, inThis=t2}))
        | lp A.Unit = NONE
	      | lp A.True = NONE
	      | lp A.False = NONE
	      | lp (A.Not t1) =
    	     (case t1
    	        of A.True  => SOME A.False
    	         | A.False => SOME A.True
    		       | _ => (case lp t1
          		           of SOME t1' => SOME (A.Not t1')
          			          | NONE => NONE))
        | lp (A.If (t1, t2, t3)) =
    	     (case t1
    	        of A.True => SOME t2
    	         | A.False => SOME t3
               | _ => (case lp t1
                         of SOME t1' => SOME (A.If (t1', t2, t3))
                          | NONE => NONE))
        | lp (A.Alloc t1) = 
           (case (lp t1)
              of SOME t1' => SOME (A.Alloc t1')
               | NONE => SOME (A.Location (Store.malloc t1)))
        | lp (A.Read t1) = 
           (case (lp t1)
              of SOME t1' => SOME (A.Read t1')
               | NONE => (case t1
                            of A.Location loc => SOME (Store.read loc)
                             | _ => NONE))
        | lp (A.Write (t1, t2)) = 
           (case (lp t1)
              of SOME t1' => SOME (A.Write (t1', t2))
               | NONE => (case (lp t2)
                            of SOME t2' => SOME (A.Write (t1, t2'))
                             | NONE => (case t1
                                          of A.Location loc => if Store.write (loc, t2) = ()
                                                               then SOME (A.Unit)
                                                               else NONE
                                           | _ => NONE)))
        | lp (A.Location _) = NONE
        | lp (A.Record items) = NONE
    	  | lp (A.Select (label, t1)) = 
           (case (lp t1)
              of SOME t1' => SOME (A.Select (label, t1'))
               | NONE => (case t1
                            of (A.Record items) => (case (List.find (fn (f, _) => label = f) items)
                                                      of SOME (_, t) => SOME t
                                                       | NONE => NONE)
                             | _ => NONE))
	      | lp (A.Sequence ts) = 
           (case ts 
              of t1::[] => SOME t1
               | t1::ts' => (case (lp t1)
                               of SOME t1' => SOME (A.Sequence (t1'::ts'))
                                | NONE => SOME (A.Sequence ts'))
               | [] => NONE)
    in
      lp t
    end

  fun eval t =
    let
      val _ = Store.clear()
      fun lp t = 
        (case step t
           of SOME t' => lp t'
	    | NONE => t)
    in
      lp t
    end	

  fun steps t =
    let
      val _ = Store.clear()
      fun lp t =
        (case step t
           of SOME t' => t :: lp t'
            | NONE => [t])
    in
      lp t
    end

end
