structure Compile : sig

(* todo: read this signature carefully to see what's available. *)
              
  datatype evaluation_strategy
    = CBV
    | FullBeta
    | Lazy
                                       
(* These compile programs directly. *)
  val compile  : evaluation_strategy -> string -> ULC.term
  val compile' : evaluation_strategy -> string -> ULC.term list

(* These open files and compile the programs in the them. *)
  val file     : evaluation_strategy -> string -> ULC.term
  val file'    : evaluation_strategy -> string -> ULC.term list

(* These last three are provided for convenience and quick testing. *)
  val cbv      : string -> ULC.term
  val fullBeta : string -> ULC.term
  val lazy     : string -> ULC.term
                                                           
end = struct

  fun println s = (TextIO.print s; TextIO.print "\n")

  datatype evaluation_strategy
    = CBV
    | FullBeta
    | Lazy

(* choose a reduction system *)    
  fun reducer CBV      = CallByValue.reduce
    | reducer FullBeta = FullBeta.reduce
    | reducer Lazy     = Lazy.reduce

  fun steps CBV      = CallByValue.steps
    | steps FullBeta = FullBeta.steps
    | steps Lazy     = Lazy.steps

  fun openTermWarning (t : ULC.term) : unit =
   (case Subst.freeVars t
      of [] => ()
       | ss => println ("*** warning: term contains free variables " ^
                        String.concatWith ", " ss))

  fun compile strategy program =
    let
      val tokens = Scan.scan program
      val ast    = Parse.parse tokens
      val ulc    = Desugar.term ast
      val _      = openTermWarning ulc
      val result = (reducer strategy) ulc
    in
      result
    end

  fun compile' strategy program =
    let
      val tokens = Scan.scan program
      val ast    = Parse.parse tokens
      val ulc    = Desugar.term ast      
      val _      = openTermWarning ulc
      val result = (steps strategy) ulc
    in
      result
    end

  fun file strategy filename  = compile strategy (Read.file filename)

  fun file' strategy filename = compile' strategy (Read.file filename)

  val cbv      = compile CBV
  val fullBeta = compile FullBeta
  val lazy     = compile Lazy

end

