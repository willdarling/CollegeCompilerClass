structure AST : sig

  datatype term
    = True
    | False
    | Zero
    | If of term * term * term
    | Succ of term
    | Pred of term
    | IsZero of term
    | And of term * term
    | Or of term * term
    | Not of term
    | Equal of term * term

  val unparse : term -> string
  val equal : term * term -> bool  
  
  val isValue : term -> bool
  val isNumericValue : term -> bool
  
end = struct

  datatype term
    = True
    | False
    | Zero
    | If of term * term * term
    | Succ of term
    | Pred of term
    | IsZero of term
    | And of term * term
    | Or of term * term
    | Not of term
    | Equal of term * term

  fun unparse True = "T"
    | unparse False = "F"
    | unparse Zero = "O"
    | unparse (If (t1, t2, t3)) = "{if " ^ unparse t1 ^ " then " ^ unparse t2 ^ " else " ^ unparse t3 ^"}"
    | unparse (Succ t) = "{+1 " ^ unparse t ^ "}"
    | unparse (Pred t) = "{-1 " ^ unparse t ^ "}"
    | unparse (IsZero t) = "{isz " ^ unparse t ^ "}"
    | unparse (And (t1, t2)) = "{&& " ^ unparse t1 ^ " " ^ unparse t2 ^ "}"
    | unparse (Or (t1, t2)) = "{|| " ^ unparse t1 ^ " " ^ unparse t2 ^ "}"
    | unparse (Not t) = "{! " ^ unparse t ^ "}"
    | unparse (Equal (t1, t2)) = "{== " ^ unparse t1 ^ " " ^ unparse t2 ^ "}"

  fun equal (t1, t2) = if String.compare (unparse t1, unparse t2) = EQUAL then true else false
  
  fun isNumericValue Zero = true
    | isNumericValue (Succ t) = isNumericValue t
    | isNumericValue (Pred t) = isNumericValue t
    | isNumericValue _ = false

  fun isValue True = true
    | isValue False = true
    | isValue t = isNumericValue t

end
