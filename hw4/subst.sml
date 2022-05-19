structure Subst : sig

  val subst    : string * ULC.term * ULC.term -> ULC.term
  val freeVars : ULC.term -> string list
			       
end = struct

  structure U = ULC

(* Note: variables, in this compiler, are just strings.
 * You do not need to implement a fast set.
 * Quadratic-time and/or quadratic-space operations are acceptable.
 * In a production setting, they wouldn't be.
 *)		    
  structure VarSet : sig
    type set
    val empty     : set
    val isEmpty   : set -> bool
    val size      : set -> int
    val singleton : string -> set
    val member    : string * set -> bool
    val union     : set * set -> set
    val remove    : set * string -> set
    val toList    : set -> string list
  end = struct
    type set  = string list 
    val empty = []
    fun isEmpty l = List.null l
    fun size l    = List.length l
    fun singleton x     =  x::[]
    fun member (x, [])   = false
      | member (x, a::l) = 
         (case String.compare (x, a) 
            of EQUAL => true
             | _ => member (x, l))
    fun union (l1, l2)   = l1 @ l2
    fun remove ([], x)   = []
      | remove (a::l, x) = 
         (case String.compare (a, x) 
            of EQUAL => l
             | _ => a :: remove(l, x))
    fun toList l  = l
  end

  val counter = ref 0
  fun freshVarName () =
    let
      val n = "@" ^ Int.toString (!counter)
      val _ = (counter := (1 + !counter))
    in
      n
    end

(* Follow the definition of FV as in the text and the README. *)
  fun FV (t : U.term) : VarSet.set = 
     (case t
        of U.Var x => VarSet.singleton x
         | U.Abs (x, t1) => VarSet.remove (FV t1, x)
         | U.App (t1, t2) => VarSet.union (FV t1, FV t2))

  val freeVars = VarSet.toList o FV
	
  fun subst (x, s, U.Var y) = 
       (case String.compare (x, y) 
          of EQUAL => s
           | _ => U.Var y)
    | subst (x, s, U.App (t1, t2)) = U.App (subst (x, s, t1), subst (x, s, t2))
    | subst (x, s, U.Abs (y, t1)) =
       (case String.compare (x, y)
          of EQUAL => U.Abs (y, t1)
           | _ => 
              (case VarSet.member (y, FV s)
                 of false => U.Abs (y, subst (x, s, t1))
                  | true => 
                    (let
                       val y' = freshVarName ()
                     in
                       subst (x, s, U.Abs (y', subst (y, U.Var y', t1)))
                     end)))

	    
end
