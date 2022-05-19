structure Eval : sig

  val step : AST.term -> AST.term option
  val eval : AST.term -> AST.term

end = struct

  structure A = AST

  (* Boolean Evaluation Relations *)
  fun step (A.If (A.True, t2, t3)) = SOME(t2)
    | step (A.If (A.False, t2, t3)) = SOME(t3)
    | step (A.If (t1, t2, t3)) = 
        (case step t1 
          of SOME(t1') => SOME(A.If (t1', t2, t3))
          | NONE => NONE)
    (* Arithmetic Expression Evaluation Relations *)
    | step (A.Succ t1) =
        (case step t1 
          of SOME(t1') => SOME(A.Succ t1')
          | NONE => NONE)
    | step (A.Pred A.Zero) = SOME(A.Zero)
    | step (A.Pred (A.Succ nv1)) = if A.isNumericValue nv1 then SOME(nv1) else NONE
    | step (A.Pred t1) =
        (case step t1 
          of SOME(t1') => SOME(A.Pred t1')
          | NONE => NONE)
    | step (A.IsZero A.Zero) = SOME(A.True)
    | step (A.IsZero (A.Succ nv1)) = if A.isNumericValue nv1 then SOME(A.False) else NONE
    | step (A.IsZero t1) =
        (case step t1 
          of SOME(t1') => SOME(A.IsZero t1')
          | NONE => NONE)
    (* Short Circuit Logical-And Evaluation Relations *)
    | step (A.And (A.False, t2)) = SOME(A.False)
    | step (A.And (A.True, t2)) = SOME(t2)
    | step (A.And (t1, t2)) =
        (case step t1 
          of SOME(t1') => SOME(A.And (t1', t2))
          | NONE => NONE)
    (* Short Circuit Logical-Or Evaluation Relations *)
    | step (A.Or (A.True, t2)) = SOME(A.True)
    | step (A.Or (A.False, t2)) = SOME(t2)
    | step (A.Or (t1, t2)) =
        (case step t1 
          of SOME(t1') => SOME(A.Or (t1', t2))
          | NONE => NONE)
    (* Logical-Not Evaluation Relations *)
    | step (A.Not A.True) =SOME(A.False)
    | step (A.Not A.False) = SOME(A.True)
    | step (A.Not t1) =
        (case step t1 
          of SOME(t1') => SOME(A.Not t1')
          | NONE => NONE)
    (* Logical-Equal Evaluation Relations *)
    | step (A.Equal (A.True, A.True)) = SOME(A.True)
    | step (A.Equal (A.False, A.False)) = SOME(A.True)
    | step (A.Equal (A.False, A.True)) = SOME(A.False)
    | step (A.Equal (A.True, A.False)) = SOME(A.False)
    | step (A.Equal (A.Zero, A.Zero)) = SOME(A.True)
    | step (A.Equal (A.Succ nv1, A.Zero)) = if A.isNumericValue nv1 then SOME(A.False) else NONE
    | step (A.Equal (A.Zero, A.Succ nv1)) = if A.isNumericValue nv1 then SOME(A.False) else NONE
    | step (A.Equal (A.Succ nv1, A.Succ nv2)) = if A.isNumericValue nv1 andalso A.isNumericValue nv2 
                                              then SOME(A.Equal (nv1, nv2))
                                              else NONE
    | step (A.Equal (t1, t2)) =
        (case step t1 
          of SOME(t1') => SOME(A.Equal (t1', t2))
          | NONE => case step t2 (* This means t1 is a value *)
                      of SOME(t2') => SOME(A.Equal (t1, t2'))
                      | NONE => NONE)  
    | step _ = NONE

  fun eval term = 
    (case step term
      of NONE => term
      |  SOME(term) => eval term)
    (* in eval, take a step at a time until you can step no more *)

end
