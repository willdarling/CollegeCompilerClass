structure FullBeta : REDUCTION_SYSTEM = struct

  structure U = ULC

  fun step (U.Abs (x, t1)) =
       (case step t1
          of SOME t1' => SOME (U.Abs (x, t1'))
           | NONE => NONE)
    | step (U.App (t1, t2)) = 
       (case step t1
          of SOME t1' => SOME (U.App (t1', t2))
           | NONE => 
             (case step t2
                of SOME t2' => SOME (U.App (t1, t2'))
                 | NONE => 
                   (case t1
                      of (U.Abs (x, t12)) => SOME (Subst.subst (x, t2, t12))
                       | _ => NONE)))
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
