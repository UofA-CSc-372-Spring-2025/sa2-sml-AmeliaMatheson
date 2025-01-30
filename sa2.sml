(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Amelia Matheson     *)
(* Time spent on SA2: 10 *)

(* Collaborators and references:
  ChatGPT - Explanation of unit testing examples
  ChatGPT - Explanation of how to use foldl
  ChatGPT - Fixing error with function binding while using foldl and lambda functions
  Claude - Fixing failing unit test for firstVowel
  Claude - Fixing failing unit test for zip
  https://cs.wellesley.edu/~cs251/s16/slides/sml_lists_solns_1up.pdf - How to apply a function to a list
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

(** MY SOLUTIONS **)



(**** Problem A ****)

fun mynull []       = true
  | mynull (_::_)   = false

val () =
  Unit.checkExpectWith Bool.toString "mynull [] should be true"
  (fn () => mynull [])
  true
val () =
  Unit.checkAssert
  "mynull [1,2,3] should be false"
  (fn () => mynull [1,2,3] = false)


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
val () =
    Unit.checkAssert
    "firstVowel 'b' should be false"
    (fn () => firstVowel [#"b"] = false)
val () =
    Unit.checkAssert
    "firstVowel [] should be true"
    (fn () => firstVowel [] = false)



(**** Problem C ****)
(* Use foldl to reverse a list - foldl (fn input & acc => result) acc_init lst*)

fun reverse list = foldl (fn (x, acc) => x :: acc) [] list;

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]
val () =
  Unit.checkAssert
  "reverse [1,2,3] should be [3,2,1]"
  (fn () => reverse [1,2,3] = [3,2,1])
val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "reverse [1,2,3,4,5] should be [5,4,3,2,1]"
  (fn () => reverse [])
  []



(**** Problem D ****)
(* Use foldl to find the minimum of a list *)

fun minlist [] = raise Match
  | minlist list = foldl (fn (x, acc) => Int.min(x, acc)) (valOf(Int.maxInt)) list;

val () =
  Unit.checkAssert
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0] = 0)

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0
val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])


(**** Problem E ****)

exception Mismatch;

fun zip ([], []) = []
  | zip (x::xs, y::ys) = (x, y) :: zip (xs, ys)
  | zip _ = raise Mismatch


val () =
  Unit.checkExnWith (Unit.listString
                        (Unit.pairString Unit.intString Unit.intString))
  "zip ([1,2,3], [4,5]) should raise an exception"
  (fn () => zip ([1,2,3], [4,5]))
val () =
  Unit.checkExnWith (Unit.listString
                        (Unit.pairString Unit.intString Unit.intString))
  "zip ([], [4,5]) should raise an exception"
  (fn () => zip ([], [4,5]))
val () =
  Unit.checkExpectWith (Unit.listString
                        (Unit.pairString Unit.intString Unit.intString))
  "zip ([1,2], [3,4]) should be [(1,3),(2,4)]"
  (fn () => zip ([1,2], [3,4]))
  [(1,3),(2,4)]
val () = 
  Unit.checkAssert
  "zip ([], []) should be []"
  (fn () => zip ([], []) = [])
val () =
  Unit.checkExpectWith (Unit.listString
                        (Unit.pairString Unit.intString Unit.stringString))
  "zip ([1,2,3], [\"a\",\"b\",\"c\"]) should be [(1,\"a\"),(2,\"b\"),(3,\"c\")]"
  (fn () => zip ([1,2,3], ["a","b","c"]))
  [(1,"a"),(2,"b"),(3,"c")]



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
    Unit.checkAssert "concat [[1, 2], [3]] should be [1, 2, 3]"
    (fn () => concat [] = [])



(**** Problem G ****)

fun isDigit #"0" = true
  | isDigit #"1" = true
  | isDigit #"2" = true
  | isDigit #"3" = true
  | isDigit #"4" = true
  | isDigit #"5" = true
  | isDigit #"6" = true
  | isDigit #"7" = true
  | isDigit #"8" = true
  | isDigit #"9" = true
  | isDigit _    = false

val () =
    Unit.checkExpectWith Bool.toString "isDigit '1' should be true"
    (fn () => isDigit #"1")
    true
val () =
    Unit.checkExpectWith Bool.toString "isDigit 'a' should be false"
    (fn () => isDigit #"a")
    false
val () =
  Unit.checkAssert
  "isDigit '0' should be true"
  (fn () => isDigit #"0" = true)
val () =
  Unit.checkAssert
  "isDigit 'n' should be false"
  (fn () => isDigit #"n" = false)


(**** Problem H ****)

fun isAlpha c = (Char.ord(c) >= 65 andalso Char.ord(c) <= 90) orelse (Char.ord(c) >= 97 andalso Char.ord(c) <= 122)

val () =
  Unit.checkExpectWith Bool.toString "isAlpha 'a' should be true"
  (fn () => isAlpha #"a")
  true
val () =
  Unit.checkAssert
  "isAlpha 'A' should be true"
  (fn () => isAlpha #"A" = true)
val () =
  Unit.checkAssert
  "isAlpha '1' should be false"
  (fn () => isAlpha #"1" = false)


(**** Problem I ****)

fun svgCircle (cx, cy, r, fill) = "<circle cx=\"" ^ Int.toString cx ^ "\" cy=\"" ^ Int.toString cy ^ "\" r=\"" ^ Int.toString r ^ "\" fill=\"" ^ fill ^ "\" />"

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";
val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (100, 200, 50, \"blue\") should return <circle cx=\"100\" cy=\"200\" r=\"50\" fill=\"blue\" />"
  (fn () => svgCircle (100, 200, 50, "blue"))
  "<circle cx=\"100\" cy=\"200\" r=\"50\" fill=\"blue\" />";


(**** Problem J ****)

(* Google Gemini: "let", "in", and "end" are used together to create a local binding
where you define variables within a specific scope, with "let" introducing the
binding, "in" marking the expression to be evaluated using those local variables,
and "end" signifying the end of that local scope*)

fun partition f [] = ([], [])
  | partition f (x::xs) = 
    let
      val (trues, falses) = partition f xs
    in
      if f x
      then (x::trues, falses)  
      else (trues, x::falses)  
    end

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);
val () =
  Unit.checkAssert
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5]; should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] = ([2, 4], [1, 3, 5]))
val () =
  Unit.checkAssert
  "partition (Char.isAlpha) [] should return ([], [])"
  (fn () => partition (Char.isAlpha) [] = ([], []))

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
