structure TypeCheck : sig

  datatype ty 
    = Nat
    | Bool
  
  val typeof : AST.term -> ty
  val check  : AST.term -> unit

end = struct

  structure A = AST

  datatype ty = Nat | Bool

  fun typeof t = 
    (case t
      of A.True => Bool
      | A.False => Bool
      | A.Zero => Nat
      | A.If (t1, t2, t3) => (case (typeof t1, typeof t2, typeof t3)
                                of (Bool, Nat, Nat) => Nat
                                | (Bool, Bool, Bool) => Bool
                                | (_, _, _) => raise Fail "ill-typed")
      | A.Succ t' => (case typeof t'
                        of Nat => Nat
                        | Bool => raise Fail "ill-typed")

      | A.Pred t' => (case typeof t'
                        of Nat => Nat
                        | Bool => raise Fail "ill-typed")
      | A.IsZero t' => (case typeof t'
                          of Nat => Bool
                          | Bool => raise Fail "ill-typed")
      
      | A.And (t1, t2) => (case (typeof t1, typeof t2)
                            of (Bool, Bool) => Bool
                            | (_, _) => raise Fail "ill-typed")
      | A.Or (t1, t2) => (case (typeof t1, typeof t2)
                            of (Bool, Bool) => Bool
                            | (_, _) => raise Fail "ill-typed")
      | A.Not t1 => (case typeof t1
                      of Bool => Bool
                      | _ => raise Fail "ill-typed")
      | A.Equal (t1, t2) => (case (typeof t1, typeof t2) 
                              of (Nat, Nat) => Bool
                              | (Bool, Bool) => Bool
                              | (_, _) => raise Fail "ill-typed"))



  fun check t = 
    let
      val _ = typeof t
    in
      ()
    end

end
