module Toolbox = struct
    let (+=) i v = i:= !i+v
    let (+=.) i v = i:= !i +. v
    let rec contains l e = match l with
        |x::xs -> if e = x then true else contains xs e
        |[] -> false
    let (@@) e l = contains l e
    let rec remove e l = match l with
        |[] -> l
        |x::xs -> if x = e then xs else x::remove e xs
    let (+++) a b = let m = min (Array.length a) (Array.length b)
    in let l = Array.make m 0 in
    for k = 0 to m-1 do
    l.(k) <- (a.(k) + b.(k));
    done;
    l
    let print_bool b = if b then prerr_endline("true") else
        prerr_endline("false")

	let print_liste_int t = let r = ref "[" in
    let rec aux l = match l with
    | [] -> ();
    |[x] -> r:= !r^string_of_int(x);
    | x::xs -> r:= !r^string_of_int(x)^"; "; aux xs
    in (aux t; prerr_endline (!r^"]");)
    let string_of_list_int t = let r = ref "[" in
    let rec aux l = match l with
    | [] -> ()
    |[x] -> r:= !r^string_of_int(x);
    | x::xs -> r:= !r^string_of_int(x)^"; "; aux xs in (aux t;
    !r^"]";)


	let rec explode s = match s with
		|"" -> []
		|x -> x.[0]:: (explode (String.sub s 1 ((String.length s) - 1)))
	let rec maxl l = match l with
	|[a;b] -> max a b
	|[a] -> a
	|[] -> failwith "empty list gné gné"
	|x::xs -> max x (maxl xs)
	let rec leng l = match l with
		|[] -> 0
		|x::xs -> 1 + (leng xs)
	let rec minl l = match l with
	|[a;b] -> min a b
	|[a] -> a
	|[] -> failwith "empty list gné gné"
	|x::xs -> min x (minl xs)
	let listToArray l = let r = Array.make (leng l) (List.hd l) in
		let rec aux t i = match t with
		|[] -> ()
		|x::xs -> r.(i) <- x; aux xs (i+1);
		in aux l 0;
	r
	let arrayToList t = let r = ref [] in
	for i = (Array.length t) - 1 downto 0 do
	    r := t.(i)::!r
	done;
	!r
	let print_array_int t = print_liste_int (arrayToList t)
    end;;



open Toolbox;;
open Array;;



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



let rec idToA sid actions = match actions with
|[] -> failwith ("couldn't find the action : "^string_of_int(sid))
|x::xs -> if x#id = sid then x else idToA sid xs;;


let getGains spells witchInv= let output = make 4 [] in
let rec aux spells = match spells with
|x::xs ->
    (for i = 1 to 3 do
        if x#delta.(i) > 0 then output.(i) <- x::output.(i);
    done;
    if x#delta.(0) > 0 && witchInv.(0) >= x#tomeindex then(
        prerr_endline (string_of_int(x#id)^", tomeindex : "^string_of_int(x#tomeindex));
        prerr_endline(string_of_int(witchInv.(0))^" >= "^string_of_int(x#tomeindex));
        output.(0) <- x::output.(0););
    aux xs)
|[] -> () in aux spells; output;;



let getLen potion_d inventory gains maxDepth=
    let q = Queue.create () in
    Queue.add ((potion_d +++ inventory), 0, [], []) q;
    let rec crossGains l inv len prPath learnL=
        match l with
        |[] -> ()
        |x::xs -> let tL = x#tomeindex>0 && not ((x#id)@@learnL) in
        Queue.add (((x#delta)+++inv+++[|if tL then -x#tomeindex else 0;0;0;0|]), (len+1), ((x#id)::prPath),
        (if tL then (x#id)::learnL else learnL)) q;
        crossGains xs inv len prPath learnL in
    let rec aux x = let inv, len, prevPath, learnList = x and
        i = ref 0 in
(*            print_liste_int prevPath;*)
(*            prerr_endline "vv inventory vv";*)
(*            print_array_int inv;*)
            while !i < 4 && inv.(!i) >= 0 do
                incr i;
            done;
    if !i = 4  then (true, len, prevPath) else
    begin
        let i = ref 0 and t = ref false in
         while !i <= 3 && not !t do
            if inv.(3 - !i) < 0 then
            begin
                t:= true;
                crossGains gains.(3 - !i) inv len prevPath learnList;
            end;
            incr i;
        done;
     false, len, prevPath
     end
     in let test = ref false and resP = ref [] and resLen = ref 0 in
     while (not (Queue.is_empty q)) && not !test do
        let t, len, path = aux (Queue.take q) in if t then
        ((*prerr_endline "vvv I'm returnig this vvv"; print_liste_int path;*)test := true; resP := path; resLen := len) else (if len> maxDepth then (test := true;
        prerr_endline("search went too long"); resLen := (-1); resP := path))
     done;
     !resP, !resLen;;


let chosenPath = ref [];;
let toLearnSpells = ref [];;
let nullAc = new action 0 "" 0 0 0 0 0 0 0 false false;;
let goalPotion = ref (-1);;
(* game loop *)
while true do
    (*todo add beamSearch and add a clock for the 50 ms*)

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
    |x::xs -> let t = (isSpell xs) in if x#atype = "CAST" || x#atype = "LEARN" then x::t else t
    |[] -> []; in
    isSpell !actionListR in

    (
    (*todo add support for learnable spells *)
    let bestRatio = ref (-1.) and myInv = witches.(0)#inventory
        in let gains = getGains spellList (myInv) in prerr_endline  "gains : ";
        Array.map (fun l -> List.map (fun a-> print_array_int (a#delta)) l;prerr_endline "\n") gains;
        prerr_endline "________________________";
    let i = ref 0 in
    while !i < actioncount do
        let a = actions.(!i) in
        if a#atype = "BREW" then(
            let price = a#price + (if a#taxcount > 0 then a#tomeindex else 0) and path, len =  getLen a#delta myInv gains 5
            in let newRatio =  (float_of_int(price) /. float_of_int(len)) in
            prerr_endline ("potion : "^string_of_int(a#id)^", ratio: "^string_of_float(newRatio)^", chosen? : "
                ^string_of_bool(!bestRatio < newRatio || !bestRatio < 0.) );
            print_liste_int path;
            prerr_endline "";
            if !bestRatio < newRatio || !bestRatio < 0. then
            (bestRatio := newRatio; chosenPath := path; goalPotion := a#id);
            );
        i += 1;
    done; chosenPath := !chosenPath@[!goalPotion]; print_liste_int !chosenPath;);
    resultString := "REST";
    toLearnSpells := (let rec ll p = match p with
        |[]->[]
        |x::xs -> let y = idToA x !actionListR in if y#atype = "LEARN" then x::(ll xs) else ll xs in ll !chosenPath);
    let need_action = ref true and needBlue = ref false and learnObj = ref (-1) and minC = ref 0 in
    if !toLearnSpells != [] then
    begin
        let rec crossLearn l = match l with
            |[] -> ()
            |x::xs ->  if !learnObj = -1 || (idToA x !actionListR)#tomeindex < !minC then
                (learnObj := x; minC := (idToA x !actionListR)#tomeindex) in crossLearn !toLearnSpells;
        if  witches.(0)#inventory.(0) - !minC >= 0 then (resultString := "LEARN" ^" "^string_of_int(!learnObj);
        need_action := false;)
        else needBlue := true;
    end;


    if !need_action then (
        if !needBlue then (
        let needRest = ref false and trySpell = ref nullAc and maxTryScore = ref (-1) in
        let rec crossPathB l = match l with
                    |[] -> (*needBlue := !needRest*) ()
                    |x::xs ->  let y = idToA x !actionListR in
                    if  (Array.fold_left (&&) true (Array.map (fun x -> x>= 0) (witches.(0)#inventory +++ y#delta))) &&
                    y#delta.(0) > 0 then (let useful = y#delta.(0)+witches.(0)#inventory.(0) >= !minC in
                        if y#castable && useful then
                           (resultString := y#atype ^ " " ^ string_of_int(y#id); trySpell := y;
                           needRest := false)
                        else (if useful then (needRest := true; resultString := "REST")
                                else (if not !needRest && (y#delta.(0)> !maxTryScore ||
                                (y#castable && not !trySpell#castable)) then
                                    (trySpell := y;maxTryScore := y#delta.(0);
                                    resultString := y#atype ^ " " ^ string_of_int(y#id); )
                                     );
                             crossPathB xs);)
                in crossPathB !chosenPath;
            if not !needRest then chosenPath := remove !trySpell#id !chosenPath;
        );
        if not !needBlue then (
        let rec crossPath l = match l with
            |[] -> ()
            |x::xs ->  let y = idToA x !actionListR in
                if y#castable &&
                    (Array.fold_left (&&) true (Array.map (fun x -> x>=0) (witches.(0)#inventory +++ y#delta))) then
                    (resultString := y#atype ^ " " ^ string_of_int(y#id); chosenPath := remove x !chosenPath)
                    else crossPath xs
        in crossPath !chosenPath;
        if !resultString = "REST" then (
            print_liste_int(!chosenPath);
            match !chosenPath with
            |[x] -> let y = idToA x !actionListR in resultString := y#atype ^ " " ^ string_of_int(y#id); chosenPath := [];
            |_ -> ()
            )
        );
    );





    (* Write an action using print_endline *)
    (* To debug: prerr_endline "Debug message"; *)


    (* in the first league: BREW <id> | WAIT; later: BREW <id> | CAST <id> [<times>] | LEARN <id> | REST | WAIT *)
    print_endline !resultString;


    done;;
