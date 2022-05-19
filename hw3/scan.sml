structure Scan : sig

  datatype token
    = LBrace
    | RBrace
    | T
    | F
    | Zero
    | If
    | Then
    | Else
    | PlusOne
    | MinusOne
    | IsZ
    | DoubleAmpersand
    | DoublePipe
    | Bang
    | DoubleEqual
    | Identifier of string
    | In
    | Of
    | LeftArrow   (* this is "<-" *)
    | RightArrow  (* this is "->" *)
    | Hash
    | Hash1
    | Hash2
    | Some
    | None
    | LParen
    | RParen
    | Case
    | Pipe
    | Star
    | Plus

  val scan : string -> token list
  val tos  : token -> string

end = struct

  datatype token
    = LBrace    
    | RBrace
    | T
    | F
    | Zero
    | If
    | Then
    | Else
    | PlusOne
    | MinusOne
    | IsZ
    | DoubleAmpersand
    | DoublePipe
    | Bang
    | DoubleEqual
    | Identifier of string(**)
    | In
    | Of
    | LeftArrow
    | RightArrow
    | Hash
    | Hash1
    | Hash2
    | Some
    | None
    | LParen
    | RParen
    | Case
    | Pipe
    | Star
    | Plus

  fun afterNewline cs =
    let
      fun lp [] = []
        | lp (#"\n"::cs) = cs
        | lp (_::cs) = lp cs

    in
      lp cs
    end

  fun alphaMaker [] = NONE
      | alphaMaker (#"_" :: cs') = SOME(#"_", cs')
      | alphaMaker (a :: cs') = (case Char.isAlphaNum a
                                  of true => SOME(a, cs')
                                  | false => NONE)

  fun alphaHandler (cs : char list) : char list =
    let
      fun lp cs = 
       (case alphaMaker cs
          of NONE => []
           | SOME (a, cs') => a :: lp cs')
    in
      lp cs
    end

  fun nextToken (cs : char list) : (token * char list) option =
    let
(* This tokenizer handles the symbolic tokens, but none of the alphabetic ones. *)
(* Your scanner needs to determine whether a string of characters is a reserved word or an identifier. *)
(* An identifier is the broad category including variable names and type names. *)
      fun lp [] = NONE
        | lp (#"{" :: cs) = SOME (LBrace, cs)
        | lp (#"}" :: cs) = SOME (RBrace, cs)
        | lp (#"0" :: cs) = SOME (Zero, cs)
        | lp (#"+" :: #"1" :: cs) = SOME (PlusOne, cs)
        | lp (#"-" :: #"1" :: cs) = SOME (MinusOne, cs)
        | lp (#"&" :: #"&" :: cs) = SOME (DoubleAmpersand, cs)
        | lp (#"|" :: #"|" :: cs) = SOME (DoublePipe, cs)
        | lp (#"|" :: cs) = SOME (Pipe, cs)
        | lp (#"!" :: cs) = SOME (Bang, cs)
        | lp (#"=" :: #"=" :: cs) = SOME (DoubleEqual, cs)
        | lp (#"(" :: cs) = SOME (LParen, cs)
        | lp (#")" :: cs) = SOME (RParen, cs)
        | lp (#"<" :: #"-" :: cs) = SOME (LeftArrow, cs)
        | lp (#"-" :: #">" :: cs) = SOME (RightArrow, cs)
        | lp (#"#" :: #"1" :: cs) = SOME (Hash1, cs)
        | lp (#"#" :: #"2" :: cs) = SOME (Hash2, cs)
        | lp (#"#" :: cs) = SOME (Hash, cs)
        | lp (#"*" :: cs) = SOME (Star, cs)
        | lp (#"+" :: cs) = SOME (Plus, cs)
        | lp (#"/" :: #"/" :: cs) = lp (afterNewline cs)
        | lp (#" " :: cs) = lp cs
        | lp (#"\n" :: cs) = lp cs
        | lp (#"\t" :: cs) = lp cs
        | lp cs = case alphaHandler cs
                    of [] => raise Fail ("scan failure or unimplemented token at " ^ implode cs)
                    | (#"T" :: []) => SOME (T, List.drop(cs, 1))
                    | (#"F" :: []) => SOME (F, List.drop(cs, 1))
                    | (#"i" :: #"f" :: []) => SOME (If, List.drop (cs, 2))
                    | (#"t" :: #"h" :: #"e" :: #"n" :: []) => SOME (Then, List.drop(cs, 4))
                    | (#"e" :: #"l" :: #"s" :: #"e" :: []) => SOME (Else, List.drop(cs, 4))
                    | (#"i" :: #"s" :: #"z" :: []) => SOME (IsZ, List.drop(cs, 3))
                    | (#"i" :: #"n" :: []) => SOME (In, List.drop(cs, 2))
                    | (#"o" :: #"f" :: []) => SOME (Of, List.drop(cs, 2))
                    | (#"s" :: #"o" :: #"m" :: #"e" :: []) => SOME (Some, List.drop(cs, 4))
                    | (#"n" :: #"o" :: #"n" :: #"e" :: []) => SOME (None, List.drop(cs, 4))
                    | (#"c" :: #"a" :: #"s" :: #"e" :: []) => SOME (Case, List.drop(cs, 4))
                    | alpha => SOME (Identifier (implode alpha), List.drop(cs, length(alpha)))
    in
      lp cs
    end

  fun scan program =
    let
      fun lp cs = 
       (case nextToken cs
          of NONE => []
           | SOME (tok, cs') => tok :: lp cs')
    in
      lp (explode program)
    end  

  val proof = [LBrace, None, LParen, Star, LParen, Identifier("Opt"), Identifier("Bool"), RParen, Identifier("Nat"), RParen, RBrace]
  val _ = Check.expect(scan("{none (* (Opt Bool) Nat)}"), proof, "test 1")

  fun tos LBrace = "{"
    | tos RBrace = "}"
    | tos T = "T"
    | tos F = "F"
    | tos Zero = "0"
    | tos If = "if"
    | tos Then = "then"
    | tos Else = "else"
    | tos PlusOne = "+1"
    | tos MinusOne = "-1"
    | tos IsZ = "isz"
    | tos DoubleAmpersand = "&&"
    | tos DoublePipe = "||"
    | tos Bang = "!"
    | tos DoubleEqual = "=="
    | tos (Identifier s) = "Identifier(" ^ s ^ ")"
    | tos In = "in"
    | tos Of = "of"
    | tos LeftArrow = "<-"
    | tos RightArrow = "->"
    | tos Hash = "#"
    | tos Hash1 = "#1"
    | tos Hash2 = "#2"
    | tos Some = "some"
    | tos None = "none"
    | tos LParen = "("
    | tos RParen = ")"
    | tos Case = "case"
    | tos Pipe = "|"
    | tos Star = "*"
    | tos Plus = "+"
    
end
