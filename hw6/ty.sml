structure Ty : sig

  datatype ty
    = Bool
    | Unit
    | Function of ty * ty
    | Ref of ty
    | Record of (string * ty) list

  val tos : ty -> string
  val eq  : ty * ty -> bool

  val subtype : ty * ty -> bool
			 
end = struct

  structure L = List

  datatype ty
    = Bool
    | Unit
    | Function of ty * ty
    | Ref of ty
    | Record of (string * ty) list

  val spc = String.concatWith " "

  fun tos Bool = "Bool"
    | tos Unit = "Unit"
    | tos (Function (t1, t2)) = "(-> " ^ tos t1 ^ " " ^ tos t2 ^ ")"
    | tos (Ref t1) = "(Ref " ^ tos t1 ^ ")"
    | tos (Record items) = "{" ^ record items ^ "}"
  and record items = String.concatWith "," (L.map (fn (l,t) => l^":"^(tos t)) items)

  fun eq (t1:ty, t2) = (t1=t2)

(* Return true if t1 is a subtype of t2, false otherwise. *)
  fun subtype (Record items1, Record items2) = 
        let
          fun finder (f2, ty2) = L.exists (fn (f1, ty1) => f1 = f2 andalso subtype (ty1, ty2)) items1 
        in
          L.all finder items2
        end
    | subtype (Function (s1, s2), Function (ty1, ty2)) = subtype (ty1, s1) andalso subtype (s2, ty2)
    | subtype (Ref ty1, Ref ty2) = subtype (ty1, ty2) andalso subtype (ty2, ty1)
    | subtype (ty1, ty2) = eq (ty1, ty2)
			 
end
