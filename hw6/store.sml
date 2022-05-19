structure Store : sig

(* This is like CS154, but 1000x easier. *)
	      
  exception OutOfMemory
  exception UnallocatedMemory
  exception SegmentationFault

  type loc = int

(* Allocate a place to put the data, put it there, and return the location. *)		 
(* Raise an OutOfMemory exception if no more memory is available. *)
  val malloc : AST.term -> loc

(* Read the data at the location. *)			       
(* Raise an UnallocatedMemory exception if the location contains unitialized data. *)
(* Raise a SegmentationFault if the location is outside the heap. *)
  val read   : loc -> AST.term

(* Write the data to the location. *)			  
(* Raise an UnallocatedMemory exception if the location contains unitialized data. *)
(* Raise a SegmentationFault if the location is outside the heap. *)
  val write  : loc * AST.term -> unit

(* Erase all heap data and make all memory available again. *)
  val clear  : unit -> unit

end = struct

  structure A = Array

  exception OutOfMemory
  exception UnallocatedMemory
  exception SegmentationFault

  type loc = int
  val heap : AST.term option array = A.array (1000, NONE)

  fun malloc t = 
    let
      val update = 
       (case (A.findi (fn (_, t : AST.term option) => t = NONE) heap)
          of SOME (i, _) => (i, A.update (heap, i, SOME t))
           | NONE => raise OutOfMemory)
    in
      #1 update
    end

  fun read loc = 
   (case A.sub (heap, loc) handle Subscript => raise SegmentationFault
      of NONE => raise UnallocatedMemory
       | SOME t => t)

  fun write (loc, t) = 
    if A.sub (heap, loc) = NONE handle Subscript => raise SegmentationFault 
    then raise UnallocatedMemory
    else A.update (heap, loc, SOME t)

  fun clear () = A.modify (fn _ => NONE) heap

end
