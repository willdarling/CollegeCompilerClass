structure Check : CHECK = struct

  fun msg s m = "Check." ^ s ^ " failure: " ^ m ^ "\n"

  fun assertT (b, m) =
    if b then () else raise Fail (msg "assertT" m)

  fun assertF (b, m) =
    if not b then () else raise Fail (msg "assertF" m)
                              
  fun expect (x, y, m) =
    if x=y then () else raise Fail (msg "expect" m)
                              
  fun expectBy (eq, x, y, m) =
    if eq(x,y) then () else raise Fail (msg "expectBy" m)

  fun among (x, ys, m) =
    let
      fun lp [] = raise Fail (msg "among" m)
        | lp (y::ys) = if x=y then () else lp ys
    in
      lp ys
    end
                                                      
  fun amongBy (eq, x, ys, m) =
    let
      fun lp [] = raise Fail (msg "amongBy" m)
        | lp (y::ys) = if eq(x,y) then () else lp ys
    in
      lp ys
    end

  fun within (eps:real, x, y, m) =
    if abs(x-y)<=eps then () else raise Fail (msg "within" m)
                                        
  fun exn (compute, m) =
    let
      val x = SOME (compute ()) handle _ => NONE
    in
     (case x
        of NONE => ()
        |  SOME _ => raise Fail (msg "exn" m))
    end

  fun eyeball (tos, x, y, m) =
    let
      val pr = fn s => (TextIO.print s; TextIO.print "\n")
      val _ = pr ("eyeball test (" ^ m ^ "):")
      val _ = pr (tos x)
      val _ = pr (tos y)		   
    in
      pr ""
    end

end
