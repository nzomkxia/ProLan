(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(*1*)
fun only_capitals string_list =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) string_list
(*2*)
fun longest_string1 string_list =
    foldl (fn (h,t) => if String.size(h) > String.size(t) then h else t) "" string_list
(*3*)
fun longest_string2 string_list =
    foldl (fn (h,t) => if String.size(h) >= String.size(t) then h else t) "" string_list

(*4*)
fun longest_string_helper f string_list =
    foldl (fn (h,t) => if f(String.size(h), String.size(t)) then h else t) "" string_list
(*4*)
val longest_string3 = longest_string_helper (fn (x, y) => x>y)
(*4*)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)
(*5*)
val longest_capitalized = longest_string2 o only_capitals

(*6*)
fun rev_string string_list = (implode o rev o explode) string_list

(*7*)
fun first_answer f any_list =
    case any_list of
        [] => raise NoAnswer
        | head::rest => case f(head) of
                            NONE => first_answer f rest
                            | SOME v => v

(*8*)
fun all_answers f any_list =
    let
    fun helper(local_list, acc) =
        case local_list of
            [] => SOME acc
            | head::rest => case f(head) of
                            NONE => NONE
                            | SOME v => helper(rest, v @ acc)
    in
        helper(any_list, [])
    end

(*9a*)
val count_wildcards = g (fn() => 1) (fn x => 0)
(*9b*)
val count_wild_and_variable_lengths = g (fn() => 1) (fn x => String.size(x))

(*9c*)
fun count_some_var(s, p) =
    let
        val helper = g (fn() => 0) (fn x => if x = s then 1 else 0)
    in
        helper p
    end

(*10*)
fun form_list pattern_list =
    case pattern_list of
         Variable x => [x]
        | TupleP px => List.concat(map form_list px)
        | ConstructorP(_,ps) => form_list ps
        | _ => []

fun check_duplicated string_list =
    case string_list of
        [] => true
        | head::rest => if List.exists (fn x => x=head) rest
                      then false
                     else check_duplicated rest

fun check_pat pattern_list =
    let
        val strings = form_list pattern_list
    in
        check_duplicated strings
    end

(*11*)
fun match val_pat =
    case val_pat of
        (_,Wildcard) => SOME []
        | (v, Variable s) => SOME [(s,v)]
        | (Unit, UnitP) => SOME []
        | (Const a, ConstP a') => if a=a' then SOME []
                                  else NONE
        | (Tuple ps, TupleP ps') => if (length ps) = (length ps')
                                    then all_answers match (ListPair.zip(ps, ps'))
                                    else NONE
        | (Constructor(s,v), ConstructorP(s',v')) => if s=s' then match (v, v')
                                                       else NONE
        | _ => NONE

(*12*)
fun first_match v ps =
    SOME (first_answer (fn(p) => match (v,p)) ps)
    handle NoAnswer => NONE

