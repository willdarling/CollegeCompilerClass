structure HW1 = struct

  (* === Problem 1 === *)
			    
  (* Here are inductively defined natural numbers. *)
			      
  datatype nat
    = Zero
    | Succ of nat

  (* In natToInt, replace the raise expression with an
   * implementation of the function.
   *)
		
  fun natToInt (n : nat) : int =
    case n of
      Zero => 0
      | Succ(n') => 1 + natToInt(n')

  (* Having written natToInt, uncomment the following tests. *)
	  
  val _ = Check.expect (natToInt Zero, 0, "natToInt Zero")
  val _ = Check.expect (natToInt (Succ Zero), 1, "natToInt (Succ Zero)")
  val _ = Check.expect (natToInt (Succ(Succ(Succ Zero))), 3, "natToInt (Succ Succ Succ Zero)")
	  
  (* Continue by implementing intToNat and uncommenting the tests immediately below. *)
	  
  fun intToNat (i : int) : nat =
    case i of
      0 => Zero
      | _ => Succ(intToNat(i-1)) 

  val _ = Check.expect (intToNat 0, Zero, "intToNat 0")
  val _ = Check.expect (intToNat 1, Succ Zero, "intToNat 1")
  val _ = Check.expect (intToNat 3, Succ(Succ(Succ Zero)), "intToNat 3")

  fun natAdd (m : nat, n : nat) : nat =
    case n of
      Zero => m
      | Succ(n') => Succ(natAdd(m,n'))
	  
  (* Write some of your own Check.expect tests here, and continue
   * doing so throughout this set of exercises.
   *)

  val _ = Check.expect (natAdd(Succ Zero, Zero), Succ Zero, "natAdd 1 + 0");
  val _ = Check.expect (natAdd(Zero, Succ Zero), Succ Zero, "natAdd 0 + 1");
  val _ = Check.expect (natAdd(Succ(Succ Zero), Succ(Succ Zero)), intToNat(4), "natAdd 2 + 2");

  (* Write natEq without calling natToInt or intToNat. *)	  

  fun natEq (m : nat, n : nat) : bool =
    case (n,m) of
      (Zero, Zero) => true
      | (Zero, _) => false
      | (_, Zero) => false
      | (Succ(m'), Succ(n')) => natEq(m',n') 

	val _ = Check.expect (natEq(Succ Zero, Succ Zero), true, "natEq 1 == 1");
  val _ = Check.expect (natEq(Succ Zero, Succ(Succ Zero)), false, "natEq 1 == 2");
  val _ = Check.expect (natEq(Succ(Succ Zero), Succ Zero), false, "natEq 2 == 1");

  (* natGT checks if m is strictly greater than n. *)

  (* Write natGT without calling natToInt or intToNat. *)

  fun natGT (m : nat, n : nat) : bool =
    case (n,m) of
      (_, Zero) => true
      | (Zero, _) => false
      | (Succ(m'), Succ(n')) => natGT(m', n')

  val _ = Check.expect (natGT(Succ Zero, Succ Zero), false, "natEq 1 > 1");
  val _ = Check.expect (natGT(Succ Zero, Succ(Succ Zero)), false, "natEq 1 > 2");
  val _ = Check.expect (natGT(Succ(Succ Zero), Succ Zero), true, "natEq 2 > 1");
	  	  
  (* natToString should build a strings like "Zero",
   * "Succ(Zero)","Succ(Succ(Zero))", etc.
   * The string concatenation operator is ^, as in "a" ^ "b".
   *)
	  
  fun natToString (n : nat) : string =
    case n of
      Zero => "Zero"
      | Succ(n') => "Succ(" ^ natToString(n') ^ ")"

  val _ = Check.expect (natToString(Zero), "Zero", "natToString Zero");
  val _ = Check.expect (natToString(Succ(Succ Zero)), "Succ(Succ(Zero))", "natToString Succ(Succ(Zero))");
	  
  (* === Problem 2 === *)
	  
  datatype filesize
    = B of int
    | KB of real
    | MB of real
    | GB of real
    | TB of real
	      
  fun toMB (s : filesize) : real =
    case s of
      B(i) => real(i) * 1E~6
      | KB(r) => r * 1E~3
      | MB(r) => r
      | GB(r) => r * 1E3
      | TB(r) => r * 1E6

  val _ = Check.within (0.0, toMB(B(512)), 0.000512, "toMB B(512)")
  val _ = Check.within (0.0, toMB(KB(512.0)), 0.512, "toMB KB(512)")
  val _ = Check.within (0.0, toMB(MB(512.0)), 512.0, "toMB MB(512)")
  val _ = Check.within (0.0, toMB(GB(512.0)), 512000.0, "toMB GB(512)")
  val _ = Check.within (0.0, toMB(TB(512.0)), 512000000.0, "toMB TB(512)")

  (* === Problem 3 === *)
	  
  (* Here is a useful type synonym. *)
  type 'a pred = 'a -> bool

  (* The infix directive instructs the parser that the
   * given identifier is an infix operator. 
   *)
	  
  infix \/
  infix /\

  (* \/ is a "disjunctive composition" operator for tests.
   * Assuming you have tests isPrime and isOdd, then
   * the test (isPrime \/ isOdd) identifies primes and/or odds.
   *)

  fun (p : 'a pred) \/ (q : 'a pred) : 'a pred =
    fn a => p(a) orelse q(a)

  val tp1 = fn a => a = 4
  val tq1 = fn a => a = 1 

  val _ = Check.expect ((tp1 \/ tq1) 1, true, "tester1 1")
  val _ = Check.expect ((tp1 \/ tq1) 4, true, "tester1 4")
  val _ = Check.expect ((tp1 \/ tq1) 3, false, "tester1 3")
  

  (* /\ is a "conjunctive composition" operator for tests.
   * Assuming you have tests isPrime and isOdd, then
   * the test (isPrime /\ isOdd) identifies odd primes.
   *)
	  
  fun (p : 'a pred) /\ (q : 'a pred) : 'a pred =
    fn a => p(a) andalso q(a)
	
  val tp2 = fn a => a < 4
  val tq2 = fn a => a > 1 

  val _ = Check.expect ((tp1 /\ tq1) 3, true, "1 < 3 < 4")
  val _ = Check.expect ((tp1 /\ tq1) 0, false, "1 < 0 < 4")
  val _ = Check.expect ((tp1 /\ tq1) 5, false, "1 < 5 < 4")


  (* === Problem 4 === *)
	  
  (* Here is a mutually recursive datatype for trees
   * that alternate between having 2 and 3 children at
   * each level, and furthermore alternate between 
   * having 'a and 'b data at each level. 
   *)
	  
  (* E2 is an empty tree of type t2; E3 likewise for t3. *)
	  
  datatype ('a, 'b) t2
    = E2
    | Nd2 of 'a * ('a, 'b) t3 * ('a, 'b) t3
  and ('a, 'b) t3
    = E3
    | Nd3 of 'b * ('a, 'b) t2 * ('a, 'b) t2 * ('a, 'b) t2
							 
  (* Count the number of nodes in a given tree. Nodes to be 
   * counted are Nd2 and Nd3 values, not E2 or E3.
   *)
						       
  fun numNodes2 (t : ('a, 'b) t2) : int =
    case t of
      E2 => 0
      | Nd2(d, c1, c2) => 1 + numNodes3(c1) + numNodes3(c2)
  and numNodes3 (t : ('a, 'b) t3) : int =
    case t of
      E3 => 0
      | Nd3(d, c1, c2, c3) => 1 + numNodes2(c1) + numNodes2(c2) + numNodes2(c3)

  val sampleTree1 = E2
  val sampleTree2 = E3
  val sampleTree3 = Nd2("one", E3, E3)
  val sampleTree4 = Nd3(1, Nd2("one", E3, E3), Nd2("one", E3, E3), Nd2("one", E3, E3))
  val sampleTree5 = Nd3(1, Nd2("one", Nd3(1, E2, E2, E2), Nd3(1, E2, E2, E2)), Nd2("one", E3, E3), E2)

  val _ = Check.expect (numNodes2(sampleTree1), 0, "numNodes2 sampleTree1")
  val _ = Check.expect (numNodes3(sampleTree2), 0, "numNodes2 sampleTree2")
  val _ = Check.expect (numNodes2(sampleTree3), 1, "numNodes2 sampleTree3")
  val _ = Check.expect (numNodes3(sampleTree4), 4, "numNodes2 sampleTree4")
  val _ = Check.expect (numNodes3(sampleTree5), 5, "numNodes2 sampleTree5")


  (* === Problem 5 === *)
	    
  datatype rank
    = Ace | Two | Three | Four | Five | Six | Seven
      | Eight | Nine | Ten | Jack | Queen | King
					      
  datatype suit
    = Diamond | Clubs | Hearts | Spades
				   
  datatype card
    = Card of rank * suit
		       
  fun sameSuit (c1 : card, c2 : card) : bool =
    case (c1, c2) of
      (Card(_, s1), Card(_, s2)) => s1 = s2

  val _ = Check.expect (sameSuit(Card(Ace, Diamond), Card(Jack, Diamond)), true, "sameSuit Ad Jd")
  val _ = Check.expect (sameSuit(Card(Five, Clubs), Card(Five, Hearts)), false, "sameSuit 5c 5h")
	  
  fun differentRank (c1 : card, c2 : card) : bool =
    case (c1, c2) of
      (Card(r1, _), Card(r2, _)) => r1 <> r2

  val _ = Check.expect (differentRank(Card(Ace, Diamond), Card(Jack, Diamond)), true, "differentRank Ad Jd")
  val _ = Check.expect (differentRank(Card(Five, Clubs), Card(Five, Hearts)), false, "differentRank 5c 5h")
	  
  fun sameColor (c1 : card, c2 : card) : bool =
    case (c1, c2) of
      (Card(r1, s1), Card(r2, s2)) => 
      case (s1, s2) of
        (Diamond, Diamond) => true
        | (Diamond, Hearts) => true
        | (Hearts, Diamond) => true
        | (Hearts, Hearts) => true
        | (Spades, Spades) => true
        | (Spades, Clubs) => true
        | (Clubs, Spades) => true
        | (Clubs, Clubs) => true
        | (_, _) => false

  val _ = Check.expect (sameColor(Card(Ace, Diamond), Card(Jack, Diamond)), true, "sameColor Ad Jd")
  val _ = Check.expect (sameColor(Card(Five, Clubs), Card(Five, Hearts)), false, "sameColor 5c 5h")
  val _ = Check.expect (sameColor(Card(Ace, Diamond), Card(Jack, Hearts)), true, "sameColor Ad Jh")

end
