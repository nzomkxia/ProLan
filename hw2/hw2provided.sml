(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(word, word_list) =
    case word_list of
    [] => NONE
    | x::xs => case same_string(word, x) of
                true => SOME xs
                | false => case all_except_option(word, xs) of
                NONE => NONE
                | SOME xy => SOME (x::xy)

fun get_substitutions1(string_list_list, word) =
    case string_list_list of
    [] => []
    | x::xs => case all_except_option(word, x) of
                NONE => get_substitutions1(xs, word)
                | SOME y => y @ get_substitutions1(xs, word)

fun get_substitutions2(string_list_list, word) =
    let
        fun helper(strings, local_word) =
            case strings of
            [] => []
            | x::xs => case all_except_option(local_word,x) of
                        NONE => helper(xs, local_word)
                        | SOME y => y @ helper(xs, word)
        in
            helper(string_list_list, word)
        end

fun similar_names(string_list_list, {first = x, middle = y, last = z}) =
        let
        fun helper ({first = x, middle = y, last = z}, strings) =
            case strings of
            [] => []
            | head::rest => case get_substitutions1(strings, x) of
                            [] => []
                            | string_list =>
                                    let fun joint(string_list)=
                                        case string_list of
                                        [] => []
                                        | head::rest=>
                                        {first = head, middle = y, last = z}
                                        :: joint(rest)
                                    in
                                        joint(string_list)
                                    end
        in
            {first=x, middle=y, last=z}::
                helper({first=x, middle=y, last=z}, string_list_list)
        end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (suit,_) =
    case suit of
      Clubs => Black
    | Diamonds => Red
    | Spades => Black
    | Hearts => Red

fun card_value (_,rank) =
    case rank of
      Ace => 11
    | Jack => 10
    | Queen => 10
    | King => 10
    | Num x => x

fun remove_card (cards, card, e) =
    (*let
        val remove = 0
    in*)
        let
            fun helper (cards, result, remove) =
                case cards of
                [] => if remove=1 then result
                                else raise e
                | head::rest => if head = card then
                                            if remove = 0 then
                                                helper(rest, result, 1)

                                            else
                                            let
                                                val result=head::result
                                            in
                                                helper(rest, result, 1)
                                            end
                                else
                                    let
                                        val result=head::result
                                    in
                                        helper(rest, result,remove)
                                    end
        in
            helper(cards, [], 0)
        end
    (*end*)

fun all_same_color cards =
    case cards of
      [] => true
    | head::[] =>true
    | head::next::[] => card_color head = card_color next
    | head::next::rest=> if not(card_color head = card_color next) then false
                         else case rest of
                              first::tl => if card_color next = card_color first
                                           then all_same_color rest
                                           else false

fun sum_cards cards =
    let
        fun helper (card_list, sum) =
            case card_list of
              [] => sum
              | card::rest => let
                                 val sum = sum + card_value card
                              in
                                helper(rest, sum)
                              end
    in
        helper(cards, 0)
    end

fun score (cards, goal) =
    let
        fun calculate_score(pre_score, score) =
            let
                val sum = sum_cards cards
            in
                if sum > goal then let
                                      val pre_score = 3*(sum - goal)
                                   in
                                      case all_same_color cards of
                                      false => let
                                                val score = pre_score
                                              in
                                                score
                                              end
                                      | true => let
                                                    val score = pre_score div 2
                                                 in
                                                    score
                                                 end
                                   end
                else
                    let
                       val pre_score = goal - sum
                    in
                        case all_same_color cards of
                        false => let
                                    val score = pre_score
                                in
                                    score
                                end
                        | true => let
                                    val score = pre_score div 2
                                in
                                    score
                                end
                    end
            end
    in
        calculate_score(0, 0)
    end

fun officiate (card_list, move_list, goal) =
    let
        fun helper(cards, moves, held_list, temp_goal) =
            case moves of
            [] => score(held_list, temp_goal)
            | head_m::rest_m => case head_m of
                            Discard c =>
                                        let
                                        val held_list =
                                                remove_card (held_list, c, IllegalMove)
                                        in
                                        helper (cards, rest_m, held_list, temp_goal)
                                        end
                            | Draw => case cards of
                                      [] => score(held_list, temp_goal)
                                      | head_c::rest_c =>
                                      let
                                          val held_list = head_c::held_list
                                      in
                                          if sum_cards held_list > temp_goal
                                          then score(held_list, temp_goal)
                                  else helper(rest_c, rest_m, held_list, temp_goal)
                                      end
    in
        helper(card_list, move_list, [], goal)
    end

fun score_challenge (cards, goal) =
    let
        val count = 0
        val score = 0
    in
        let
            fun helper(card_list, temp_goal, temp_score) =
                case card_list of
                    [] => temp_score
                    | card::rest => case card of
                                      (_, Ace) => let
                                                   val count = count+1
                                                  in
                                                    count
                                                  end
                                    | (_, _) => let
                                            val temp_score = card_value card + temp_score
                                                in
                                                    helper(rest, temp_goal, temp_score)
                                                end
        in
            let
                val local_score = helper(cards, goal, 0)
            in
            if score > goal then
            score = (local_score+count)*3-goal
            else
                let
                    fun calculate(temp_score, temp_goal, count) =
                        if count > 0 then
                            if temp_score+11+count-1 < temp_goal then
                                    calculate(temp_score+11, temp_goal, count-1)
                            else temp_score+count
                        else
                            if temp_score <= temp_goal then
                                temp_goal - temp_score
                            else
                                temp_score*3 - temp_goal
                in
                    score = calculate(score, goal, count)
                end
            end
        end
    end

