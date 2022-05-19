structure Eval : sig

  val step  : AST.term -> AST.term option
  val subst : string * AST.term * AST.term -> AST.term
  val eval  : AST.term -> AST.term

  val isNormalForm : AST.term -> bool
  val isStuck      : AST.term -> bool

end = struct

  structure A = AST

(* read subst as "rewrite x to v in t" *)
  fun subst (x, v, A.True)            = A.True
    | subst (x, v, A.False)           = A.False
    | subst (x, v, A.Zero)            = A.Zero
    | subst (x, v, A.If (t1, t2, t3)) = A.If (subst (x,v,t1), subst(x,v,t2), subst(x,v,t3))
    | subst (x, v, A.Succ t)          = A.Succ (subst (x,v,t))
    | subst (x, v, A.Pred t)          = A.Pred (subst (x,v,t))
    | subst (x, v, A.IsZero t)        = A.IsZero (subst (x,v,t))
    | subst (x, v, A.And (t1, t2))    = A.And (subst (x,v,t1), subst(x,v,t2))
    | subst (x, v, A.Or (t1, t2))     = A.Or (subst (x,v,t1), subst(x,v,t2))
    | subst (x, v, A.Not t)           = A.Not (subst (x,v,t))
    | subst (x, v, A.Equal (t1, t2))  = A.Equal (subst (x,v,t1), subst(x,v,t2))
    | subst (x, v, A.Unit)            = A.Unit
    | subst (x, v, A.Var s)           = if String.compare (x, s) = EQUAL then
                                          v
                                        else A.Var s
    | subst (x, v, A.Let (y, t1, t2)) = 
        if String.compare (x, y) = EQUAL then
          A.Let (y, subst (x,v,t1), t2)
        else
          A.Let (y, subst (x,v,t1), subst (x,v,t2))
    | subst (x, v, A.Pair (t1, t2))   = A.Pair (subst (x,v,t1), subst (x,v,t2))
    | subst (x, v, A.Select1 t)       = A.Select1 (subst (x,v,t))
    | subst (x, v, A.Select2 t)       = A.Select2 (subst (x,v,t))
    | subst (x, v, A.Some t)          = A.Some (subst (x,v,t))
    | subst (x, v, A.None ty)         = A.None ty
    | subst (x, v, A.OptCase (t1, (y, t2), t3)) =
        if String.compare (x, y) = EQUAL then
          A.OptCase (subst (x,v,t1), (y, t2), subst (x,v,t3))
        else
          A.OptCase (subst (x,v,t1), (y, subst (x,v,t2)), subst (x,v,t3)) 

(* please note that most of step can be copied from hw2 *)
(* step should not raise an error on stuck terms, it should just return NONE *)	 
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
          | NONE => (case step t2 (* This means t1 is a value *)
                      of SOME(t2') => SOME(A.Equal (t1, t2'))
                      | NONE => NONE))
    (* Let Evaluation Relations *)
    | step (A.Let(x, t1, t2)) =
        (case step t1
          of SOME(t1') => SOME(A.Let (x, t1', t2))
          | NONE => SOME(subst (x,t1,t2))) (* This means t1 is a value *)
    (* Pair Evaluation Relations *)
    | step (A.Pair (t1, t2)) = 
        (case step t1
          of SOME(t1') => SOME(A.Pair (t1', t2))
          | NONE => (case step t2 (* This mean t1 is a value*)
                       of SOME(t2') => SOME(A.Pair (t1, t2'))
                       | NONE => NONE))
    | step (A.Select1 t) =
        (case t
          of A.Pair (t1, _) => SOME(t1)
          | _ => NONE)
    | step (A.Select2 t) =
        (case t
          of A.Pair (_, t2) => SOME(t2)
          | _ => NONE)
    (* Option Evaluation Relations *)
    | step (A.Some t) =
        (case step t
          of SOME(t') => SOME(A.Some t')
          | NONE => NONE)
    (* OptCase Evaluation Relations *)
    | step (A.OptCase (t1, (x, t2), t3)) =
        (case step t1
          of SOME(t1') => SOME(A.OptCase (t1', (x, t2), t3))
          | NONE => case t1 (* This means t1 is a some(value) or none *)
                    of A.Some(v1) => SOME(subst (x,v1,t2))
                    | A.None(ty) => SOME(t3)
                    | _ => NONE)
    | step _ = NONE


(* note: isNormalForm and isStuck have one-line implementations *)

  fun isNormalForm t = 
    (case step t
       of NONE => true
       | _ => false)
			     
  fun isStuck t = if isNormalForm t andalso A.isValue t then
                    false
                  else true

  fun eval t =
   (case step t
      of NONE => t
       | SOME t' => eval t')

end
