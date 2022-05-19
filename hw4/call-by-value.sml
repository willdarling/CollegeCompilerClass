structure CallByValue : REDUCTION_SYSTEM = struct

  structure U = ULC

  fun step (U.App (t1, t2)) = 
       (case step t1 (* if we can step on t1 *)
          of SOME t1' => SOME (U.App (t1', t2)) (* E-App1 *)
           | NONE => 
              (case step t2 (* then if we can step on t2 *)
                 of SOME t2' => SOME (U.App (t1, t2')) (* E-App2 *)
                  | NONE => 
                    (case t1 (* check for our last rule, which requires a lambda t1*)
                       of U.Abs (x, t12) => 
                         (if ULC.isValue t2 (* if t2's a value *) 
                            then SOME (Subst.subst(x, t2, t12)) (*then E-AppAbs *)
                            else NONE)
                        | _ => NONE))) (* otherwise stuck *)
    | step _ = NONE

  fun reduce t =
    (case step t
      of NONE => t
       | SOME t' => reduce t')

  fun steps t  =
    (case step t
      of NONE => t :: []
       | SOME t' => t :: steps t')

end
