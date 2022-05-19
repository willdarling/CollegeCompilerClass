structure ULC : sig

(* ULC stands for Untyped Lambda Calculus *)
              
  datatype term
    = Var of string
    | Abs of string * term
    | App of term * term

  val isValue      : term -> bool
  val isStuck      : term -> bool
  val isNormalForm : term -> bool
                                 
  val tos : term -> string
  val tos': term list -> string
  
end = struct

  datatype term
    = Var of string
    | Abs of string * term
    | App of term * term

  fun isValue (Abs _) = true
    | isValue _ = false 

  fun isStuck (Var _) = true
    | isStuck (App (t1, t2)) = isStuck t1 orelse isStuck t2
    | isStuck _ = false

  fun isNormalForm t = isValue t orelse isStuck t
                      
  fun tos t =
    let
      fun lp (Var x) = x
        | lp (Abs (x, t1)) = "{" ^ x ^ " " ^ lp t1 ^ "}"
        | lp (App (t1, t2)) = "(" ^ lp t1 ^ " " ^ lp t2 ^ ")"
    in
      lp t
    end

  fun myconcat (s1, s2) = s1 ^ "->" ^ s2
  fun tos' tlist = foldr op myconcat "" (map tos tlist)  

end
