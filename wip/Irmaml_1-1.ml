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
let (++=) a b = a := !a ++ b
let minP a b = if a>>b then b else a
let maxP a b = if a>>b then a else b
let intOfP a = match a with
|Inf -> max_int
|P(x) -> x;;



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



let rec idToA sid spells = match spells with
|[] -> failwith "couldn't find the action"
|x::xs -> if x#id = sid then x else idToA sid xs;;


let getGains spells = let output = make 4 [] in
let rec aux spells = match spells with
|x::xs ->
    (for i = 0 to 3 do
        if x#delta.(i) > 0 then output.(i) <- x::output.(i);
    done;
    aux xs)
|[] -> () in aux spells; output;;


let getMissingsI p inv =
    let r = make 4 0 and t = ref true in
    for i = 0 to 3 do
        let d = p.(i) + inv.(i) in
        r.(i) <- d;
        if d < 0 then t := false;
    done;
    r, !t;;

(*todo add return value diff which allow us to do tree search every turn*)
let getLen potion_d inventory gains maxDepth=
    let q = Queue.create () in
    Queue.add ((potion_d +++ inventory), 0, []) q;
    let rec crossGains l inv len prPath =
        match l with
        |[] -> ()
        |x::xs -> Queue.add (((x#delta)+++inv), (len+1), ((x#id)::prPath)) q;
        crossGains xs inv len prPath in
    let rec aux x = let inv, len, prevPath = x and
        i = ref 0 and d = ref 0 in
            while !i < 4 do
             let di = inv.(!i)
                if di < 0 then d += (-di)

                incr i;
            done;
    if !d = 0 then (0, len, prevPath) else
    begin
        let i = ref 0 and t = ref false in
         while !i <= 3 && not !t do
            if inv.(3 - !i) < 0 then
            begin
                t:= true;
                crossGains gains.(!i) inv len prevPath
            end
            incr i;
        done;
     !d, len, prevPath
     end
     in let test = ref false and resP = ref [] and
     resLen = ref 0 and resD = ref Inf in
     while (not (Queue.is_empty q)) && not !test do
        let d, len, path = aux (Queue.take q) in
        if !resD >> P d then (resD := P d; resP := path; resLen := len;)
        if len >= maxDepth || d = 0 then test := true;
     done;
     !resP, !resLen, (intOfP !resD);;


let chosenPath = ref [];;
let nullAc = new action 0 "" 0 0 0 0 0 0 0 false false;;
(* game loop *)
while true do

    let resultString = ref "WAIT gnégné" and
    actioncount = int_of_string (input_line stdin) (* the number of spells and recipes in play *)
    in let actions = make actioncount nullAc and actionListR = ref [] in

    for i = 0 to actioncount - 1 do
        (* actionid: the unique ID of this spell or recipe
         actiontype: in the first league: BREW; later: CAST, OPPONENT_CAST, LEARN, BREW
         delta0: tier-0 ingredient change
         delta1: tier-1 ingredient change
         delta2: tier-2 ingredient change
         delta3: tier-3 ingredient change
         price: the price in rupees if this is a potion
         tomeindex: in the first two leagues: always 0; later: the index in the tome if this is a tome spell, equal to the read-ahead tax; For brews, this is the value of the current urgency bonus
         taxcount: in the first two leagues: always 0; later: the amount of taxed tier-0 ingredients you gain from learning this spell; For brews, this is how many times you can still gain an urgency bonus
         castable: in the first league: always 0; later: 1 if this is a castable player spell
         repeatable: for the first two leagues: always 0; later: 1 if this is a repeatable player spell *)
        let actionid, actiontype, delta0, delta1, delta2, delta3, price, tomeindex, taxcount, castable, repeatable = Scanf.sscanf (input_line stdin) " %d  %s  %d  %d  %d  %d  %d  %d  %d  %d  %d" (fun actionid actiontype delta0 delta1 delta2 delta3 price tomeindex taxcount castable repeatable -> (actionid, actiontype, delta0, delta1, delta2, delta3, price, tomeindex, taxcount, castable = 1, repeatable = 1)) in
            let a = (new action actionid actiontype delta0 delta1
            delta2 delta3 price tomeindex taxcount castable repeatable) in
                actions.(i) <- a; actionListR:= a:: !actionListR;
    done;


    let witches = make 2 (new witch 0 0 0 0 0) in
    for i = 0 to 1 do
        (* inv0: tier-0 ingredients in inventory *)
        (* score: amount of rupees *)
        let inv0, inv1, inv2, inv3, score = Scanf.sscanf (input_line stdin) " %d  %d  %d  %d  %d" (fun inv0 inv1 inv2 inv3 score -> (inv0, inv1, inv2, inv3, score)) in
        witches.(i) <- (new witch inv0 inv1 inv2 inv3 score);
    done;
    let spellList = let rec isSpell l = match l with
    |x::xs -> let t = (isSpell xs) in if x#atype = "CAST" then x::t else t
    |[] -> []; in
    isSpell !actionListR in

    if !chosenPath = [] then (
        let bestRatio = ref (-1.) and myInv = witches.(0)#inventory
        and bestPot = ref 0 and gains = getGains spellList in
    for i = 0 to actioncount - 1 do
        let a = actions.(i) in
        if a#atype = "BREW" then

            let price = a#price and path, len, diff =  getLen a#delta myInv gains 5
            in let newRatio =  (float_of_int(price) /. float_of_int(len+diff)) in
            if (!bestRatio = (-1.) || !bestRatio < newRatio)  then
                (bestRatio := newRatio; chosenPath := path; bestPot := a#id)
            done; chosenPath := List.rev (!bestPot::!chosenPath))
    else (
        match !chosenPath with
        |[] -> failwith "wtf c vide alors queyapa le vide"
        |x::xs -> (let y = idToA x spellList in
            if y#castable then
                (resultString := y#atype ^ " " ^ string_of_int(y#id);
                chosenPath := xs;)
            else resultString := "REST"
        )

        );






    (* Write an action using print_endline *)
    (* To debug: prerr_endline "Debug message"; *)


    (* in the first league: BREW <id> | WAIT; later: BREW <id> | CAST <id> [<times>] | LEARN <id> | REST | WAIT *)
    print_endline !resultString;


    done;;
