structure Parse : sig

  val parse : Scan.token list -> AST.term

end = struct

  structure S = Scan
  structure A = AST

  fun nextTerm (tokens : S.token list) : (A.term * S.token list) option = 
    let
      fun lp [] = NONE
        (* Parse T *)
        | lp(S.T :: toks) = SOME(A.True, toks)
        (* Parse F *)
        | lp(S.F :: toks) = SOME(A.False, toks)
        (* Parse 0 *)
        | lp(S.Zero :: toks) = SOME(A.Zero, toks)
        (* Parse {if t then t else t} *)
        | lp(S.LBrace :: S.If :: toks) =
            (case lp toks
              of SOME(t1, S.Then :: toks) => 
                (case lp toks
                  of SOME(t2, S.Else :: toks) => 
                    (case lp toks
                      of SOME(t3, S.RBrace :: toks) => SOME(A.If (t1, t2, t3), toks)
                      | SOME _ => raise Fail "parse error: expected } in if"
                      | NONE => raise Fail "parse error: if ended unexpectedly")
                  | SOME _ => raise Fail "parse error: expected else in if"
                  | NONE => raise Fail "parse error: if ended unexpectedly")
              | SOME _ => raise Fail "parse error: expected then in if"
              | NONE => raise Fail "parse error: if ended unexpectedly")
        (* Parse {+1 t} *)
        | lp(S.LBrace :: S.PlusOne :: toks) =
            (case lp toks
              of SOME(t1, S.RBrace :: toks) => SOME(A.Succ t1, toks)
              | SOME _ => raise Fail "parse error: expected } in +1"
              | NONE => raise Fail "parse error: +1 ended unexpectedly")
        (* Parse {-1 t} *)
        | lp(S.LBrace :: S.MinusOne :: toks) =
            (case lp toks
              of SOME(t1, S.RBrace :: toks) => SOME(A.Pred t1, toks)
              | SOME _ => raise Fail "parse error: expected } in -1"
              | NONE => raise Fail "parse error: -1 ended unexpectedly")
        (* Parse {isz t} *)
        | lp(S.LBrace :: S.IsZ :: toks) =
            (case lp toks
              of SOME(t1, S.RBrace :: toks) => SOME(A.IsZero t1, toks)
              | SOME _ => raise Fail "parse error: expected } in isz"
              | NONE => raise Fail "parse error: isz ended unexpectedly")
        (* Parse {&& t t} *)
        | lp(S.LBrace :: S.DoubleAmpersand :: toks) =
            (case lp toks
              of SOME(t1, toks) => 
                (case lp toks
                  of SOME(t2, S.RBrace :: toks) => SOME(A.And (t1, t2), toks)
                  | SOME _ => raise Fail "parse error: expected } in &&"
                  | NONE => raise Fail "parse error: && ended unexpectedly")
              | NONE => raise Fail "parse error: && ended unexpectedly")
        (* Parse {|| t t} *)
        | lp(S.LBrace :: S.DoublePipe :: toks) =
            (case lp toks
              of SOME(t1, toks) => 
                (case lp toks
                  of SOME(t2, S.RBrace :: toks) => SOME(A.Or (t1, t2), toks)
                  | SOME _ => raise Fail "parse error: expected } in ||"
                  | NONE => raise Fail "parse error: || ended unexpectedly")
              | NONE => raise Fail "parse error: || ended unexpectedly")
        (* Parse {! t} *)
        | lp(S.LBrace :: S.Bang :: toks) =
            (case lp toks
              of SOME(t1, S.RBrace :: toks) => SOME(A.Not t1, toks)
              | SOME _ => raise Fail "parse error: expected } in !"
              | NONE => raise Fail "parse error: ! ended unexpectedly")
        (* Parse == t t} *)
        | lp(S.LBrace :: S.DoubleEqual :: toks) =
            (case lp toks
              of SOME(t1, toks) => 
                (case lp toks
                  of SOME(t2, S.RBrace :: toks) => SOME(A.Equal (t1, t2), toks)
                  | SOME _ => raise Fail "parse error: expected } in =="
                  | NONE => raise Fail "parse error: == ended unexpectedly")
              | NONE => raise Fail "parse error: == ended unexpectedly")
        | lp _ = raise Fail "parse error: unrecognized token"
    in
      lp tokens
  end

(*
LBrace PlusOne LBrace PlusOne LBrace PlusOne Zero RBrace RBrace RBrace

LBrace DoubleAmpersand T F RBrace
*)

  fun parse tokens = 
    (case nextTerm tokens
      of SOME(term, []) => term
      | SOME(term, _) => raise Fail "parse error: extra tokens after expression"
      | NONE => raise Fail "parse error: given empty program")
    (* parsing the whole token sequence should result in exactly one term *)
end