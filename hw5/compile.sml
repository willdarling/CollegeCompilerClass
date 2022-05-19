structure Compile : sig

  datatype 'a attempt
    = Success of 'a
    | Failure of {reason: string}
	      
  datatype evaluation_strategy
    = CBV
    | FullBeta
    | Lazy

(* These compile programs directly. *)
  val compile   : evaluation_strategy -> string -> ULC.term attempt
  val compile'  : evaluation_strategy -> string -> ULC.term list attempt
  val compile'' : evaluation_strategy -> string -> unit
							   
(* These open files and compile the programs in the them. *)
  val file      : evaluation_strategy -> string -> ULC.term attempt
  val file'     : evaluation_strategy -> string -> ULC.term list attempt
  val file''    : evaluation_strategy -> string -> unit
      
(* These are provided for convenience and quick testing. *)
  val cbv        : string -> ULC.term attempt
  val fullBeta   : string -> ULC.term attempt
  val lazy       : string -> ULC.term attempt

  val cbv'       : string -> ULC.term list attempt
  val fullBeta'  : string -> ULC.term list attempt
  val lazy'      : string -> ULC.term list attempt

  val cbv''      : string -> unit
  val fullBeta'' : string -> unit
  val lazy''     : string -> unit
                               
end = struct

  datatype evaluation_strategy
    = CBV
    | FullBeta
    | Lazy
	  
  datatype 'a attempt
    = Success of 'a
    | Failure of {reason: string}

    fun reducer CBV      = CallByValue.reduce
    | reducer FullBeta = FullBeta.reduce
    | reducer Lazy     = Lazy.reduce

  fun steps CBV      = CallByValue.steps
    | steps FullBeta = FullBeta.steps
    | steps Lazy     = Lazy.steps

  fun openTermWarning (t : ULC.term) : unit =
   (case Subst.freeVars t
      of [] => ()
       | ss => Utils.println ("*** warning: term contains free variables " ^
                              String.concatWith ", " ss))

  fun compile strategy program =
    let
      val tokens = Scan.scan program
      val ast    = Parse.parse tokens
      val mulc   = Desugar.term ast
      val ulc    = Curry.term mulc
      val _      = openTermWarning ulc
      val result = (reducer strategy) ulc
    in
      Success result
    end
    handle (Fail s) => Failure {reason = s}
         | _  => Failure {reason = "unexpected exception"}

  fun compile' strategy program =
    let
      val tokens = Scan.scan program
      val ast    = Parse.parse tokens
      val mulc   = Desugar.term ast
      val ulc    = Curry.term mulc
      val _      = openTermWarning ulc
      val result = (steps strategy) ulc
    in
      Success result
    end
    handle (Fail s) => Failure {reason = s}
         | _  => Failure {reason = "unexpected exception"}

  fun compile'' strategy program =
    let
      val _ = Utils.println ("original program: " ^ program)
      val _ = Utils.println ""
    in
      case compile' strategy program
       of Failure {reason} => Utils.println ("compilation failed: " ^ reason)
	| Success terms => (Utils.println "evaluation:" ;
			    List.app (Utils.println o ULC.tos) terms)
    end
	
  fun file strategy filename   = compile strategy (Read.file filename)
  fun file' strategy filename  = compile' strategy (Read.file filename)
  fun file'' strategy filename = compile'' strategy (Read.file filename)

  fun cbv prog      = compile CBV prog
  fun fullBeta prog = compile FullBeta prog
  fun lazy prog     = compile Lazy prog
			 
  fun cbv' prog      = compile' CBV prog
  fun fullBeta' prog = compile' FullBeta prog
  fun lazy' prog     = compile' Lazy prog

  fun cbv'' prog      = compile'' CBV prog
  fun fullBeta'' prog = compile'' FullBeta prog
  fun lazy'' prog     = compile'' Lazy prog

end
