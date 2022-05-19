structure TypeCheck : sig

  val typeof : AST.term -> Ty.ty
  val check  : AST.term -> unit

end = struct

  structure A = AST
  structure L = List

  structure TypeEnv : sig
    type env   = (string * Ty.ty) list
    val empty  : env
    val lookup : string * env -> Ty.ty option
    val extend : env * string * Ty.ty -> env
  end = struct
    type env   = (string * Ty.ty) list
    val empty = []
    fun lookup (x:string, []) = NONE
      | lookup (x, (y,ty)::gamma') =
	  if x=y
	  then SOME ty
	  else lookup (x, gamma')
    fun extend (gamma,x,t) = (x,t)::gamma
  end
  
  infix <+>
  fun g <+> (x,ty) = TypeEnv.extend (g, x, ty)

  fun typeof t =
    let
      fun lp (g, A.Var x) =
           (case TypeEnv.lookup (x, g)
	      of SOME ty => ty
	       | NONE => raise Fail ("type error: free variable " ^ x))
	| lp (g, A.Abs (x, ty1, t1)) = Ty.Function (ty1, lp (g<+>(x, ty1), t1))
	| lp (g, A.App (t1, t2)) = 
      (case (lp (g, t1), lp (g, t2))
         of (Ty.Function (ty1, ty2), ty3) =>
            if Ty.subtype (ty3, ty1)
            then ty2
            else raise Fail "type error: type mismatch in app branches"
          | _ => raise Fail "type error: no function in app")
	| lp (g, A.Let (x, t1, t2)) = lp (g <+> (x, lp (g, t1)), t2)
	| lp (_, A.Unit) = Ty.Unit
	| lp (_, A.True) = Ty.Bool
	| lp (_, A.False) = Ty.Bool
	| lp (g, A.Not t1) =
	    if Ty.eq (Ty.Bool, lp (g, t1))
	    then Ty.Bool
	    else raise Fail "type error: not applied to non-bool"
	| lp (g, A.If (t1, t2, t3)) =
	    (case (lp (g, t1), lp (g, t2), lp (g, t3))
	       of (Ty.Bool, ty2, ty3) =>
	          if Ty.eq (ty2, ty3)
		        then ty2
            else raise Fail "type error: type mismatch in if branches"
		      | _ => raise Fail "type error: non-bool test in if")
  | lp (g, A.Alloc t1) = Ty.Ref (lp (g, t1))
	| lp (g, A.Read t1) = 
      (case lp (g, t1)
         of Ty.Ref ty1 => ty1
          | _ => raise Fail "type error: cannot dereference a non-ref type")
  | lp (g, A.Write (t1, t2)) = 
      (case (lp (g, t1), lp (g, t2))
         of (Ty.Ref ty1, ty2) =>
            if Ty.eq (ty1, ty2)
            then Ty.Unit
            else raise Fail "type error: type mismatch in :="
          | _ => raise Fail "type error: cannot write to a non-ref type")
	| lp (_, A.Location _) = raise Fail "BUG: there no locations in surface language; this shouldn't happen"
  | lp (g, A.Record items) = Ty.Record (L.map (fn (x, t1) => (x, lp (g, t1))) items)
	| lp (g, A.Select (label, t1)) = 
      (case lp (g, t1)
         of (Ty.Record items) => 
          (case (L.find (fn (f, _) => label = f) items)
             of SOME (_, ty1) => ty1
              | NONE => raise Fail ("type error: no field "^label^" in record"))
          | _ => raise Fail "type error: trying to select from a non-record type")
	| lp (g, A.Sequence (terms:A.term list)) = 
      let
         fun lp' (ty::[]) = ty
           | lp' (ty::tys) =
               (case ty
                  of Ty.Unit => lp' tys
                   | _ => raise Fail "type error: non-unit type in sequence")
           | lp' _ = raise Fail "type error: empty sequence; this should be caught in the parser"
       in
         lp' (L.map (fn (t1) => lp (g, t1)) terms)
       end   
    in
      lp (TypeEnv.empty, t)
    end
    
  fun check t = (typeof t; ())

end
