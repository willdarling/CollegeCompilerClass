structure TypeCheck : sig

  val typeof : AST.term -> Ty.ty
  val check  : AST.term -> unit

end = struct

  structure A = AST

(* A module within a module! *)
(* Typing environments track which variable has what type. *)
  structure TypeEnv : sig
    type env   = (string * Ty.ty) list
    val empty  : env
    val lookup : string * env -> Ty.ty option
    val extend : env * string * Ty.ty -> env
  end = struct
    type env   = (string * Ty.ty) list
    val empty = []
    fun lookup (_, []) = NONE
      | lookup (x, (y, ty) :: g) = if String.compare(x, y) = EQUAL then 
                                    SOME ty 
                                   else lookup (x, g)
    fun extend (g, x, ty) = (x, ty) :: g
  end

(* A convenience: infix notation for extending typing environments. *)	    
  infix <+>
  fun gamma <+> (x,ty) = TypeEnv.extend (gamma, x, ty)

  fun typeof t =
    let
      (* lp is an internal function that has a typing environment argument as well as a term *)
      (* The typing environment is capital gamma in the formal presentation (and "g" here). *)
      fun lp (g, A.True) = Ty.Bool
        | lp (g, A.False) = Ty.Bool
        | lp (g, A.Zero) = Ty.Nat
        | lp (g, A.If (t1, t2, t3)) =
           (let
             val ty1 = lp (g, t1)
             and ty2 = lp (g, t2)
             and ty3 = lp (g, t3)
           in
             if Ty.equal(ty1, Ty.Bool) andalso Ty.equal(ty2, ty3) then 
               ty3 
             else raise Fail "ill-typed if"
           end)
        | lp (g, A.Succ t) = 
           (case lp (g, t)
              of Ty.Nat => Ty.Nat
               | _ => raise Fail "ill-typed +1")
        | lp (g, A.Pred t) =
           (case lp (g, t)
              of Ty.Nat => Ty.Nat
               | _ => raise Fail "ill-typed -1")
        | lp (g, A.IsZero t) =
           (case lp (g, t)
              of Ty.Nat => Ty.Bool
               | _ => raise Fail "ill-typed isz")
        | lp (g, A.And (t1, t2)) =
           (case (lp (g, t1), lp (g, t2))
              of (Ty.Bool, Ty.Bool) => Ty.Bool
               | _ => raise Fail "ill-typed &&")
        | lp (g, A.Or (t1, t2)) =
           (case (lp (g, t1), lp (g, t2))
              of (Ty.Bool, Ty.Bool) => Ty.Bool
               | _ => raise Fail "ill-typed ||")
        | lp (g, A.Not t) =
           (case lp (g, t)
              of Ty.Bool => Ty.Bool
               | _ => raise Fail "ill-typed !")
        | lp (g, A.Equal (t1, t2)) =
            if Ty.equal (lp (g, t1), lp (g, t2)) then
              Ty.Bool 
            else raise Fail "ill-typed =="
        | lp (g, A.Unit) = Ty.Unit
        | lp (g, A.Var x) =
           (case TypeEnv.lookup (x, g)
              of SOME ty => ty
               | NONE => raise Fail ("ill-typed variable " ^ x))
        | lp (g, A.Let (x, t1, t2)) = lp (g <+> (x, lp (g, t1)), t2)
        | lp (g, A.Pair (t1, t2)) = Ty.Pair (lp (g, t1), lp (g, t2))
        | lp (g, A.Select1 (A.Pair (t1, _))) = lp (g, t1)
        | lp (g, A.Select2 (A.Pair (_, t2))) = lp (g, t2)  
        | lp (g, A.Some t) = Ty.Opt (lp (g, t))
        | lp (g, A.None ty) = Ty.Opt ty
        | lp (g, A.OptCase (t1, (x, t2), t3)) = 
           (case lp (g, t1)
              of Ty.Opt ty1 => 
               (let
                  val ty2 = lp (g <+> (x,ty1), t2)
                  and ty3 = lp (g, t3)
                in
                  if Ty.equal (ty2, ty3) then 
                    ty3 
                  else raise Fail "ill-typed let"
                end)
               | _ => raise Fail "ill-typed let")
    in
      lp (TypeEnv.empty, t)
    end

(* ignore is a build-in function of type 'a -> unit *)
  fun check t = ignore (typeof t)
    
end
