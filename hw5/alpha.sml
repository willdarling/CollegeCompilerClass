structure Alpha : sig

  val equiv : ULC.term * ULC.term -> bool

end = struct

  structure U = ULC
  structure S = Subst

(* Determine if any two terms are the same except for their variable names. *)

(* For example, {a . a} and {b . b} are alpha-equivalent. *)
(* Similarly, {t . {f . f}} and {s . {z . z}} are alpha-equivalent. *)
(* {t . {f . f}} and {@3 . {@4 . @4}} are alpha-equivalent. *)
(* {t . {f . t}} and {s . {z . z}} are not equivalent. *)

  fun newVar i = "@" ^ Int.toString i

  fun equiv (t1, t2) = 
    let
      fun lp (U.Var x, i) = 
           (case (explode x)
              of (#"@"::cs) => U.Var x
               | _          => U.Var (newVar i))

        | lp (U.Abs (x,t1), i) = 
           (case (explode x) 
              of (#"@"::cs) => U.Abs (x, lp (t1,i+1))
               | _          => U.Abs (newVar i, lp (S.subst (x,U.Var (newVar i),t1),i+1)))

        | lp (U.App (t1,t2), i) = U.App (lp (t1,i), lp (t2,i))
    in
      lp (t1, 0) = lp (t2, 0)
    end

(* Some tests would be nice here... *)

val _ = Check.expect(equiv(U.Abs("a", U.Var("a")), U.Abs("b", U.Var("b"))), true, "equiv {a . a} {b . b}")
val _ = Check.expect(equiv(U.Abs("t", U.Abs("f", U.Var("f"))), U.Abs("s", U.Abs("z", U.Var("z")))), true, "equiv {t . {f . f}} {s . {z . z}}")
val _ = Check.expect(equiv(U.Abs("t", U.Abs("f", U.Var("f"))), U.Abs("3", U.Abs("4", U.Var("4")))), true, "equiv {t . {f . f}} {@3 . {@4 . @4}}")
val _ = Check.expect(equiv(U.Abs("t", U.Abs("f", U.Var("t"))), U.Abs("s", U.Abs("z", U.Var("z")))), false, "equiv {t . {f . t}} {s . {z . z}}")

val term1  = U.Abs("x", U.Abs("y", U.Var("z")))                      (* {x.{y.z}} *)
val term2  = U.Abs("z", U.Abs("y", U.Abs("x", U.Var("z"))))          (* {z.{y.{x.z}}} *)
val term3  = U.Abs("a", U.Abs("b", U.Var("c")))                      (* {a.{b.c}} *)
val term4  = U.Abs("c", U.Abs("b", U.Abs("a", U.Var("c"))))          (* {c.{b.{a.c}}} *)
val term5  = U.App(term1, term2)                                     (* ({x.{y.z}}, {z.{y.{x.z}}}) *)
val term6  = U.App(term3, term4)                                     (* ({a.{b.c}}, {c.{b.{a.c}}}) *)
val term7  = U.App(term5, term6)                                 
val term8  = U.App(term6, term5)
val term9  = U.App(term1, term4)
val term10 = U.App(term3, term2)
val term11 = U.App(term2, term3)
val term12 = U.Abs("x", U.Var("y"))                                  (* {x.y} *)
val term13 = U.Abs("z", U.Var("w"))                                  (* {z.w} *)
val term14 = U.Abs("x", U.Abs("z", U.Var("w")))                      (* {x.{z.w}} *)
val term15 = U.Abs("x", U.Abs("y", U.Var("x")))                      (* {x.{y.x}} *)
val term16 = U.App(term12,term14)                                    (* ({x.y} {x.{z.w}}) *)
val term17 = U.App(term15,term13)                                    (* ({x.{y.x}} {z.w}) *)

val _ = Check.expect(equiv(term1,term2),   false, "equiv: t1, t2")
val _ = Check.expect(equiv(term1,term3),   true,  "equiv: t1, t3")
val _ = Check.expect(equiv(term2,term3),   false, "equiv: t2, t3")
val _ = Check.expect(equiv(term2,term4),   true,  "equiv: t2, t4")
val _ = Check.expect(equiv(term5,term6),   true,  "equiv: t5, t6")
val _ = Check.expect(equiv(term7,term8),   true,  "equiv: t7, t8")
val _ = Check.expect(equiv(term10,term9),  true,  "equiv: t10, t9")
val _ = Check.expect(equiv(term10,term11), false, "equiv: t10,t11")
val _ = Check.expect(equiv(term12,term13), true,  "equiv: t12,t13")
val _ = Check.expect(equiv(term13,term14), false, "equiv: t13,t14")
val _ = Check.expect(equiv(term14,term15), false, "equiv: t14,t15")
val _ = Check.expect(equiv(term16,term17), false, "equiv: t16,t17")

end
