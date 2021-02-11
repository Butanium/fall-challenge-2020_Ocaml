open Array;;
module Toolbox = struct
    let (+=) i v = i:= !i+v
    let (+=.) i v = i:= !i +. v
    let rec contains l e = match l with
    |x::xs -> if e = x then true else contains xs e
    |[] -> false
    let (@@) e l = contains l e
    let (+++) a b = let m = min (Array.length a) (Array.length b)
    in let l = Array.make m 0 in
    for k = 0 to m-1 do
    l.(k) <- (a.(k) + b.(k));
    done;
    l
    end;;

open Toolbox;;

type poids = Inf | P of int
let (++) a b = match a,b with
    |P(a),P(b) -> P(a+b)
    |_ -> Inf
let (>>) a b = match a,b with
    |P(a), P(b) -> a > b
    |Inf, P(a) -> true
    |_ -> false
let (<<) a b = b>>a
let (++=) a b = a := !a ++ b;;




class action (actionid:int) (actiontype:string) (delta0:int) delta1 delta2 delta3 (price:int) (tomeindex:int) (taxcount:int) (castable:bool) (repeatable:bool)
 =
 object (self)
 val id = actionid
 val atype = actiontype
 val delta = [|delta0; delta1; delta2; delta3|]
 val price = price
 val tomeindex = tomeindex
 val taxcount = taxcount
 val castable = castable
 val repeatable = repeatable
 method id = id
 method atype = atype
 method delta = delta
 method price = price
 method tomeindex = tomeindex
 method taxcount = taxcount
 method castable = castable
 method repeatable = repeatable
 end;;


class witch (inv0:int) inv1 inv2 inv3 (score:int) =
object (self)
val inventory = [|inv0;inv1;inv2;inv3|]
val score = score
method inventory = inventory
method score = score
end;;

(* Auto-generated code below aims at helping you parse *)
(* the standard input according to the problem statement. *)







let rec getGains spells output = match spells with
|x::xs ->
    for i = 0 to 3 do
        if x#deltas.(i) > 0 then output.(i) <- x::output.(i);
    done;
    getGains xs output;
|[] -> ();;


let getMissingsI p inv =
    let r = make 4 0 and t = ref true in
    for i = 0 to 3 do
        let d = p.(i) + inv.(i) in
        r.(i) <- d;
        if d < 0 then t := false;
    done;
    r, !t;;

(**)
let getLen potion inventory gains maxDepth usedSpells=
    let fut_inv, t = getMissingsI potion inventory in if t then P 0 else
    begin
        let rec aux inv len oldInvs path usedSP =
            let i = ref 0 in
            while !i < 4 && inv.(!i) > 0 do
                i += 1;
            done;
            if !i = 3 then len, path else
            begin
                if len >= maxDepth then Inf, path else
                begin
                let tempLen = ref P 0 and i = ref 3 and t = ref true in (*todo a clean*)
                while !i >= 0 && !t do
                    if inv.(i) < 0 then
                    begin
                        t := false;
                        let bestPath = ref []
                        and minLen = ref Inf in
                        let rec crossGains g = match g with
                          |x::xs -> let newUsedSP = [] in
                        let ii = (inv +++ x#deltas)
                        and cast = if x @@ usedSP  then P 1 else begin newUsedSP = usedSP; P 0 end in
                        let l,p = if not (ii @@ oldInvs) then
                        aux ii (len ++ (P 1) ++ cast) (ii::oldInvs) x::path newUsedSP else
                        Inf, [] in
                        if !minLen >> l then begin minLen := l; bestPath := p end crossGains xs
                        |[] -> () in crossGains gains.(i);
                    end;
                    else i += (-1) done;
                    !minLen, bestPath;
                end;
            end;
        aux fut_inv (P 0) [fut_inv] [] usedSpells;
    end;;


        (*todo: start tree search *)
let getLen potion_d inventory gains maxDepth =
    let q = Queue.create () in
    Queue.add ((potion_d +++ inventory), 0, prevPath) q
    let rec aux x = let inv, len, prevPath = x in
        let i = ref 0 in
            while !i < 4 && inv.(!i) > 0 do
                incr i;
            done;
            if !i = 3 then true, len, path else
    begin let i = ref 0 and t = ref false in
     while !i >= 0 && not !t do
        if inv.(3 - !i) < 0 then
        begin
        t:= true;
        let rec crossGains l =
            match l with
            |[] -> ()
            |x::xs -> Queue.add ((x+++inv), (len+1), (x::prevPath))
        in crossGains gains.(i)
        end
     end
     false, len, []
     in let test = ref false and res = ref [] in
     while (not Queue.is_empty) && not !test do
        let t, len, path = aux (Queue.take q) in if t then
        (test := true && res := path)
     done;
     !path;;



(* game loop *)
while true do
    let actioncount = int_of_string (input_line stdin) (* the number of spells and recipes in play *)
    in let actions = make actioncount (new action 0 0 0 0 0 0 0 0 0 0 0) and actionListR = ref [] in
    for i = 0 to actioncount - 1 do
        (* actionid: the unique ID of this spell or recipe *)
        (* actiontype: in the first league: BREW; later: CAST, OPPONENT_CAST, LEARN, BREW *)
        (* delta0: tier-0 ingredient change *)
        (* delta1: tier-1 ingredient change *)
        (* delta2: tier-2 ingredient change *)
        (* delta3: tier-3 ingredient change *)
        (* price: the price in rupees if this is a potion *)
        (* tomeindex: in the first two leagues: always 0; later: the index in the tome if this is a tome spell, equal to the read-ahead tax; For brews, this is the value of the current urgency bonus *)
        (* taxcount: in the first two leagues: always 0; later: the amount of taxed tier-0 ingredients you gain from learning this spell; For brews, this is how many times you can still gain an urgency bonus *)
        (* castable: in the first league: always 0; later: 1 if this is a castable player spell *)
        (* repeatable: for the first two leagues: always 0; later: 1 if this is a repeatable player spell *)
        let actionid, actiontype, delta0, delta1, delta2, delta3, price, tomeindex, taxcount, castable, repeatable = Scanf.sscanf (input_line stdin) " %d  %s  %d  %d  %d  %d  %d  %d  %d  %d  %d" (fun actionid actiontype delta0 delta1 delta2 delta3 price tomeindex taxcount castable repeatable -> (actionid, actiontype, delta0, delta1, delta2, delta3, price, tomeindex, taxcount, castable = 1, repeatable = 1)) in
            in let a = (new action actionid actiontype delta0 delta1
            delta2 delta3 price tomeindex taxcount castable repeatable) in
            actions.(i) <- a; l:= a:: !l;

    done;
    let witches = make 2 (new witch 0 0 0 0 0);
    for i = 0 to 1 do
        (* inv0: tier-0 ingredients in inventory *)
        (* score: amount of rupees *)
        let inv0, inv1, inv2, inv3, score = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d  %d" (fun inv0 inv1 inv2 inv3 score -> (inv0, inv1, inv2, inv3, score)) in
        witches.(i) <- (new witch inv0 inv1 inv2 inv3 score);
    done;
    let spellList = let rec isSpell l = match l with
    |x::xs -> let t = (isSpell xs) in if x#atype = "CAST" then x::t else t
    |[] -> []; in
    isSpell !actionListR;;

    for i = 0 to actioncount - 1 do
    let a = actions.(i) in
    if a#atype = "CAST" then
        spellList

    let myInv = witches.(0)#inventory in
    for i=0 to actioncount - 1 do



    (* Write an action using print_endline *)
    (* To debug: prerr_endline "Debug message"; *)


    (* in the first league: BREW <id> | WAIT; later: BREW <id> | CAST <id> [<times>] | LEARN <id> | REST | WAIT *)
    print_endline "BREW 0";
    ();
    done;;
