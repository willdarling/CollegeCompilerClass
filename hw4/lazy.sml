structure Lazy : REDUCTION_SYSTEM = struct
  
  structure U = ULC

  fun step (U.App (t1,t2)) = 
       (case step t1 (* if we can step on t1 *)
          of SOME t1' => SOME (U.App (t1',t2)) (* then E-App1*)
           | NONE =>
             (case t1 (* else assume t1's an abstraction *)
                of U.Abs (x,t12) => SOME(Subst.subst(x,t12,t2))
                 | _ => NONE))
    | step _ = NONE

  fun reduce t =
    (case step t
      of NONE => t
       | SOME t' => reduce t')

  fun steps t  =
    (case step t
      of NONE => []
       | SOME t' => t :: steps t')
  
end
