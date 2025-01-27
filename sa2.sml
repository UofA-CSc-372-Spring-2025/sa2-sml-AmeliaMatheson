(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Amelia Matheson     *)
(* Time spent on SA2: 6 *)

(* Collaborators and references:
  ChatGPT - Explanation of unit testing examples
  ChatGPT - Explanation of how to use foldl
  ChatGPT - Fixing error with function binding while using foldl and lambda functions
  Claude - Fixing failing unit test for firstVowel
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(* FORBIDDEN FUNCTIONS
  length, hd, tl, null, mynull
*)

(* Unit Testing: checkExpectWith, checkAssert, checkExnWith 

val checkExpectInt     = Unit.checkExpectWith Unit.intString
val checkExpectIntList = Unit.checkExpectWith (Unit.listString Unit.intString)
val checkExpectStringList = Unit.checkExpectWith (Unit.listString Unit.stringString)
val checkExpectISList =
  Unit.checkExpectWith (Unit.listString
                        (Unit.pairString Unit.intString Unit.stringString))
val checkExpectIntListList = 
  Unit.checkExpectWith (Unit.listString (Unit.listString Unit.intString))

*)

(**** Problem A ****)

fun mynull []       = true
  | mynull (_::_)   = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true



(**** Problem B ****)

(* without using if*)
fun firstVowel (#"a"::_) = true
  | firstVowel (#"e"::_) = true
  | firstVowel (#"i"::_) = true
  | firstVowel (#"o"::_) = true
  | firstVowel (#"u"::_) = true
  | firstVowel _         = false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true




(**** Problem C ****)
(* Use foldl to reverse a list - foldl (fn input & acc => result) acc_init lst*)

fun reverse list = foldl (fn (x, acc) => x :: acc) [] list;

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]




(**** Problem D ****)
(* Use foldl to find the minimum of a list *)

fun minlist [] = raise Match
  | minlist list = foldl (fn (x, acc) => Int.min(x, acc)) (valOf(Int.maxInt)) list;

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0



(**** Problem E ****)
(*
exception Mismatch

fun zip _ = []
*)


(**** Problem F ****)

fun concat [] = []
  | concat (x::xs) = x @ concat xs   (*QUESTION: Is using @ allowed?*)

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[1,2],[],[3,4]] should be [1,2,3,4]"
  (fn () => concat [[1,2],[],[3,4]])
  [1,2,3,4]
val () = 
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[1,2,3], [4,5,6], [7,8,9]] should be [1,2,3,4,5,6,7,8,9]"
  (fn () => concat [[1,2,3], [4,5,6], [7,8,9]])
  [1,2,3,4,5,6,7,8,9]
val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[]] should be []"
  (fn () => concat [[]])
  []



(**** Problem G ****)
(*
fun isDigit _    = false;
*)


(**** Problem H ****)
(*
fun isAlpha c = false
*)


(**** Problem I ****)
(*
fun svgCircle (cx, cy, r, fill) = "NOT IMPLEMENTED YET"

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";
*)


(**** Problem J ****)
(*
fun partition p (x :: xs) = ([],[])

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);
*)

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
