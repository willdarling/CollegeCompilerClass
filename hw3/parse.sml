structure Parse : sig

  val parse : Scan.token list -> AST.term

end = struct

  structure S = Scan
  structure A = AST

  fun nextType (tokens : S.token list) : (Ty.ty * S.token list) option =
    let
      fun lp [] = NONE
        | lp (S.Identifier(s)::ts) =
           (case explode s
               of (#"N" :: #"a" :: #"t" :: []) => SOME (Ty.Nat, ts)
                | (#"B" :: #"o" :: #"o" :: #"l" :: []) => SOME (Ty.Bool, ts)
                | (#"U" :: #"n" :: #"i" :: #"t" :: []) => SOME (Ty.Unit, ts)
                | _ => raise Fail ("parse error: unrecognized type" ^ s))
        | lp (S.LParen::S.Identifier(s)::ts) = 
            if String.compare(s, "Opt") = EQUAL then
              lp1RParen (Ty.Opt, ts, "Opt")
            else 
              raise Fail "parse error: expected Opt"
        | lp (S.LParen::S.Star::ts) = lp2RParen (Ty.Pair, ts, "*")
        | lp _ = raise Fail "error or unimplemented type expression form"

      and lp1RParen (constructor, toks, nameOfThing) =
       (case lp toks
          of SOME (t1, S.RParen::ts1) => SOME (constructor t1, ts1)
           | SOME _ => raise Fail ("expected ) in " ^ nameOfThing)
           | NONE => raise Fail ("parse error: " ^ nameOfThing ^ " ended unexpectedly"))
      
      and lp2RParen (constructor, toks, nameOfThing) = 
       (case lp toks
          of SOME (t1, ts1) =>
           (case lp ts1
              of SOME (t2, S.RParen::ts2) => SOME (constructor (t1, t2), ts2)
              | SOME _ => raise Fail ("expected ) in " ^ nameOfThing) 
              | NONE => raise Fail ("parse error: " ^ nameOfThing ^ " ended unexpectedly (2)"))
          | NONE => raise Fail ("parse error: " ^ nameOfThing ^ " ended unexpectedly (1)"))
    in
      lp tokens
    end

  fun noneHandler tokens =
   (case nextType tokens
      of SOME (ty, (S.RBrace::ts)) => SOME (A.None ty, ts)
       | SOME _ => raise Fail ("parse error: none ended before }")
       | NONE => raise Fail "uh oh")

(* Note: you may erase this implementation and replace with your own parser. *)
(* It is an optional starting point. *)
  fun nextTerm (tokens : S.token list) : (A.term * S.token list) option = 
    let
      fun lp [] = NONE
        | lp (S.T::ts)    =             SOME (A.True, ts)
        | lp (S.F::ts)    =             SOME (A.False, ts)
        | lp (S.Zero::ts) =             SOME (A.Zero, ts)
        | lp (S.LParen::S.RParen::ts) = SOME (A.Unit, ts)
        | lp (S.LBrace::S.If::ts) = 
           (case lp ts
              of SOME (t1, S.Then::ts1) =>
                  (case lp ts1
                     of SOME (t2, S.Else::ts2) =>
                         (case lp ts2
                            of SOME (t3, S.RBrace::ts3) => SOME (A.If (t1, t2, t3), ts3)
                             | SOME _ => raise Fail "expected }"
                             | NONE => raise Fail "parse error: if ended before }")
                      | SOME _ => raise Fail "expected else"
                      | NONE => raise Fail "parse error: if ended before else")
                | SOME _ => raise Fail "expected then"
                | NONE => raise Fail "parse error: if ended before then")
        | lp (S.LBrace::S.PlusOne::ts)  = lp1RBrace (A.Succ, ts, "+1")
        | lp (S.LBrace::S.MinusOne::ts) = lp1RBrace (A.Pred, ts, "-1")
        | lp (S.LBrace::S.IsZ::ts)      = lp1RBrace (A.IsZero, ts, "isz") 
        | lp (S.LBrace::S.Bang::ts)     = lp1RBrace (A.Not, ts, "!")
        | lp (S.LBrace::S.Hash1::ts)    = lp1RBrace (A.Select1, ts, "#1")
        | lp (S.LBrace::S.Hash2::ts)    = lp1RBrace (A.Select1, ts, "#2")
        | lp (S.LBrace::S.Some::ts)     = lp1RBrace (A.Some, ts, "some")
        | lp (S.LBrace::S.DoubleAmpersand::ts) = lp2RBrace (A.And, ts, "&&")
        | lp (S.LBrace::S.DoublePipe::ts)      = lp2RBrace (A.Or, ts, "||")
        | lp (S.LBrace::S.DoubleEqual::ts)     = lp2RBrace (A.Equal, ts, "==")
        | lp (S.LBrace::S.Hash::ts)            = lp2RBrace (A.Pair, ts, "#")
        | lp (S.LBrace::S.Identifier(s)::S.LeftArrow::ts) = 
            if Char.isLower((String.sub (s, 0))) then 
             (case lp ts
                of SOME (t1, S.In::ts1) =>
                    (case lp ts1
                       of SOME (t2, S.RBrace::ts2) => SOME (A.Let (s, t1, t2), ts2)
                        | SOME _ => raise Fail "expected }"
                        | NONE => raise Fail "parse error: let ended before }")
                 | SOME _ => raise Fail "expected in"
                 | NONE => raise Fail "parse error: let ended before in")
            else
              raise Fail "parse error: type signature in let"
        | lp (S.LBrace::S.Case::ts) =
           (case lp ts
              of SOME (t1, S.Of::S.Some::S.Identifier(s)::S.RightArrow::ts1) =>
                (case lp ts1
                   of SOME (t2, S.Pipe::S.None::S.RightArrow::ts2) =>
                      (case lp ts2
                         of SOME (t3, S.RBrace::ts3) => SOME (A.OptCase (t1, (s, t2), t3), ts3)
                          | SOME _ => raise Fail "expected }"
                          | NONE => raise Fail "parse error: case ended before }")
                    | SOME _ => raise Fail "expected | none ->"
                    | NONE => raise Fail "parse error: case ended before | none ->")
              | SOME _ => raise Fail "expected of some x ->"
              | NONE => raise Fail "parse error: case ended before some x ->")
        | lp (S.LBrace::S.None::ts) = noneHandler ts
        | lp (S.Identifier(s)::ts) = SOME (A.Var s, ts)
        | lp _ = raise Fail "error or unimplemented expression form" (* todo: the rest of the cases *)
      
      and lp1RBrace (constructor, toks, nameOfThing) =
       (case lp toks
          of SOME (t1, S.RBrace::ts1) => SOME (constructor t1, ts1)
           | SOME _ => raise Fail ("expected } in " ^ nameOfThing)
           | NONE => raise Fail ("parse error: " ^ nameOfThing ^ " ended unexpectedly"))
      
      and lp2RBrace (constructor, toks, nameOfThing) = 
       (case lp toks
          of SOME (t1, ts1) =>
           (case lp ts1
              of SOME (t2, S.RBrace::ts2) => SOME (constructor (t1, t2), ts2)
              | SOME _ => raise Fail ("expected } in " ^ nameOfThing) 
              | NONE => raise Fail ("parse error: " ^ nameOfThing ^ " ended unexpectedly (2)"))
          | NONE => raise Fail ("parse error: " ^ nameOfThing ^ " ended unexpectedly (1)"))
    in
      lp tokens
    end

  fun parse tokens = 
   (case nextTerm tokens
      of SOME (term, []) => term
       | SOME (term, extra) => raise Fail "extra tokens after main term in program"
       | NONE => raise Fail "empty program")

  val proof = A.None (Ty.Pair(Ty.Opt Ty.Bool, Ty.Nat))
  val _ = Check.expect(parse(S.scan("{none (* (Opt Bool) Nat)}")), proof, "test 2")

end
