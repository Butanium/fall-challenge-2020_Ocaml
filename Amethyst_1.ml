(* todo: add check if validpath for path of 3 or less spells *)
let print_liste_int t =
  let r = ref "[" in
  let rec aux l =
    match l with
    | [] -> ()
    | [ x ] -> r := !r ^ string_of_int x
    | x :: xs ->
        r := !r ^ string_of_int x ^ "; ";
        aux xs
  in
  aux t;
  prerr_endline (!r ^ "]")

let string_of_list_int t =
  let r = ref "[" in
  let rec aux l =
    match l with
    | [] -> ()
    | [ x ] -> r := !r ^ string_of_int x
    | x :: xs ->
        r := !r ^ string_of_int x ^ "; ";
        aux xs
  in
  aux t;
  !r ^ "]"

let arrayToList t =
  let r = ref [] in
  for i = Array.length t - 1 downto 0 do
    r := t.(i) :: !r
  done;
  !r

let print_array_int t = print_liste_int (arrayToList t)

type action = {
  id : int;
  atype : string;
  deltas : int array;
  price : int;
  tomeindex : int;
  taxcount : int;
  castable : bool;
  repeatable : bool;
}

let create_action actionid actiontype delta0 delta1 delta2 delta3 price
    tomeindex taxcount castable repeatable =
  {
    id = actionid;
    atype = actiontype;
    deltas = [| delta0; delta1; delta2; delta3 |];
    price;
    tomeindex;
    taxcount;
    castable;
    repeatable;
  }

let nullAc =
  {
    id = 0;
    atype = "";
    deltas = [| 0; 0; 0; 0 |];
    price = 0;
    tomeindex = 0;
    taxcount = 0;
    castable = false;
    repeatable = false;
  }

let print_ac_list l = print_liste_int @@ List.map (fun x -> x.id) l

let scoreDic = Hashtbl.create 30

let incrDic dic index y =
  let r = try Hashtbl.find dic index with Not_found -> 0 in
  Hashtbl.replace dic index @@ (y + r)

let ( +++ ) t1 t2 =
  let r = Array.make (min (Array.length t1) @@ Array.length t2) 0 in
  Array.mapi (fun i x -> t1.(i) + t2.(i)) r

let ( --- ) t1 t2 =
  let r = Array.make (min (Array.length t1) @@ Array.length t2) 0 in
  Array.mapi (fun i x -> t1.(i) - t2.(i)) r

let ( @-@ ) e l = List.mem e l

let ( >>= ) t x = Array.for_all (fun y -> y >= x) t

let isValidSpell inv spell =
  let s = inv +++ spell.deltas in
  s >>= 0 && Array.fold_left ( + ) 0 s <= 10

let maxLen = 6

let stabilize inventory gains =
  let q = Queue.create () in
  Queue.add (inventory, [], 0, 0, []) q;
  let explore (inv, path, len, effLen, learnedSp) =
    let i = ref 3 and t = ref true in
    while !i >= 0 && !t do
      if inv.(!i) < 0 then t := false else i := !i - 1
    done;
    if !t then (true, path, len, effLen)
    else (
      List.iter
        (fun x ->
          let r = inv +++ x.deltas in
          if x.atype = "LEARN" then r.(0) <- r.(0) - x.tomeindex;
          let spt = x.atype = "LEARN" && (not @@ x.id @-@ learnedSp) in
          Queue.add
            ( r,
              x :: path,
              len + 1,
              (effLen + if spt then 1 else 0),
              if spt then x.id :: learnedSp else learnedSp )
            q)
        gains.(!i);
      (false, path, len + 1, effLen))
  in
  let bestLen = ref @@ (3 * maxLen)
  and resPath = ref []
  and err = ref @@ -1
  and len = ref 0 in
  while !len <= maxLen && (not @@ Queue.is_empty q) && !bestLen > !len do
    let args = Queue.take q in
    let r, path, l, malus = explore args in
    (if r then (
     if l + malus < !bestLen then bestLen := l + malus;
     resPath := path;
     err := 0)
    else if !len = maxLen then
      let inv, _, _, _, _ = args in
      let new_err =
        Array.fold_left ( + ) 0
        @@ Array.mapi (fun i x -> if x >= 0 then 0 else i * -x) inv
      in
      if new_err < !err || !err = -1 then (
        err := new_err;
        resPath := path));
    len := l
  done;
  (!bestLen, !resPath, !err)

let value inv = Array.fold_left ( + ) 0 @@ Array.mapi (fun i x -> i * x) inv

let resultAc ac = Printf.sprintf "%s %d" ac.atype ac.id

let followPath path inv =
  let result = ref nullAc in
  let learns =
    List.filter (fun x -> x.atype = "LEARN" && x.tomeindex <= inv.(0)) path
  in
  let searchState = ref @@ if learns != [] then 0 else 2 in
  let pot =
    List.filter (fun x -> x.atype = "BREW" && x.deltas +++ inv >>= 0) path
  in
  (match pot with
  | [] -> ()
  | x :: xs ->
      searchState := 3;
      result := x);

  if !searchState = 0 then (
    let mini = ref 30 and ac = ref nullAc in
    List.iter
      (fun x ->
        if x.tomeindex < !mini then (
          mini := x.tomeindex;
          ac := x))
      learns;
    if !mini != 30 then result := !ac else searchState := 1);
  if !searchState = 1 then (
    let ing1L =
      List.filter
        (fun x -> x.deltas.(0) > 0 && x.atype = "CAST" && isValidSpell inv x)
        path
    in
    let maxi = ref 0 and ac = ref nullAc in
    List.iter
      (fun x ->
        let s = x.deltas.(0) + if x.castable then 100 else 0 in
        if s > !maxi then (
          maxi := s;
          ac := x)
        else if s = !maxi then
          if
            value
              (let t = x.deltas --- !ac.deltas in
               t.(0) <- 0;
               t)
            > 0
          then (
            maxi := s;
            ac := x))
      ing1L;
    if !maxi = 0 then searchState := 2 else result := !ac);
  if !searchState = 2 then (
    let doableSpells =
      List.filter (fun x -> isValidSpell inv x && x.atype = "CAST") path
    in
    print_liste_int @@ List.map (fun x -> x.id) doableSpells;
    let casts = List.filter (fun x -> x.castable) doableSpells in
    match (casts, doableSpells) with
    | _, [] -> failwith "No spell found"
    | x :: xs, _ -> result := x
    | [], x :: xs -> result := x);
  !result

let nFirst liste n =
  let rec aux l c acc =
    match (l, c) with
    | [], _ -> acc
    | x :: xs, y when y < n -> aux xs (c + 1) @@ (x :: acc)
    | _, _ -> acc
  in
  aux liste 0 []

let maxSpell = 5

let getOptimizedGains =
  Array.map (fun x ->
      let sorted =
        List.sort
          (fun y z ->
            let sy, sz =
              ( (try Hashtbl.find scoreDic y.id with Not_found -> 0),
                try Hashtbl.find scoreDic z.id with Not_found -> 0 )
            in
            if sy < sz then sy
            else if sy > sz then sz
            else if Random.bool () then sy
            else sz)
          x
      in
      nFirst sorted maxSpell)

let floating = float_of_int

let getScore x error len =
  floating ((if x.taxcount > 0 then x.tomeindex else 0) + x.price - (2 * error))
  /. floating (len + 1)
;;

while true do
  let actioncount = int_of_string (input_line stdin) in
  (* the number of spells and recipes in play *)
  let resultString = ref "WAIT gnégné" and startTime = Sys.time () in
  let actions = Array.make actioncount nullAc and potions = ref [] in
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
    let ( actionid,
          actiontype,
          delta0,
          delta1,
          delta2,
          delta3,
          price,
          tomeindex,
          taxcount,
          castable,
          repeatable ) =
      Scanf.sscanf (input_line stdin)
        " %d  %s  %d  %d  %d  %d  %d  %d  %d  %d  %d"
        (fun
          actionid
          actiontype
          delta0
          delta1
          delta2
          delta3
          price
          tomeindex
          taxcount
          castable
          repeatable
        ->
          ( actionid,
            actiontype,
            delta0,
            delta1,
            delta2,
            delta3,
            price,
            tomeindex,
            taxcount,
            castable = 1,
            repeatable = 1 ))
    in
    let ac =
      create_action actionid actiontype delta0 delta1 delta2 delta3 price
        tomeindex taxcount castable repeatable
    in
    actions.(i) <- ac;
    if actiontype = "BREW" then potions := ac :: !potions
  done;

  let inv0, inv1, inv2, inv3, score =
    Scanf.sscanf (input_line stdin) " %d  %d  %d  %d  %d"
      (fun inv0 inv1 inv2 inv3 score -> (inv0, inv1, inv2, inv3, score))
  in
  let selfInventory = [| inv0; inv1; inv2; inv3 |] in
  let _ = input_line stdin in
  let gains = Array.make 4 [] in
  Array.iter
    (fun x ->
      if x.atype = "CAST" || x.atype = "LEARN" then
        Array.iteri
          (fun i g ->
            if g > 0 && (i > 0 || x.atype = "CAST") then
              gains.(i) <- x :: gains.(i))
          x.deltas)
    actions;
  (* Array.iter (fun x -> List.iter (fun y -> print_array_int y.deltas) x; prerr_endline "") gains;*)
  let optimizedGains = getOptimizedGains gains in
  let (x :: xs) = !potions in
  let len, path, error =
    stabilize (selfInventory +++ x.deltas) optimizedGains
  in

  let bestScore = ref @@ getScore x error len
  and bestPath = ref @@ (x :: path)
  and errorRes = ref 0 in
  List.iter
    (fun x ->
      if Sys.time () -. startTime > 0.04 then prerr_endline "outOfTime"
      else
        let len, path, error =
          stabilize (selfInventory +++ x.deltas) optimizedGains
        in
        let newScore = getScore x error len in
        if newScore > !bestScore then (
          bestScore := newScore;
          bestPath := x :: path;
          errorRes := error))
    xs;

  print_liste_int @@ List.map (fun x -> x.id) !bestPath;
  let tryT = ref true in
  let todoSp =
    try followPath !bestPath selfInventory
    with none ->
      let spellFound = ref false in
      tryT := false;
      let max = ref 0 and ac = ref nullAc in
      Array.iter
        (fun l ->
          if not !spellFound then
            List.iter
              (fun x ->
                if
                  x.atype = "CAST"
                  && x.deltas.(0) >= 0
                  && isValidSpell selfInventory x
                then
                  let s = (if x.castable then 100 else 0) + x.deltas.(0) in
                  if s > !max then (
                    max := s;
                    ac := x;
                    spellFound := true))
              l
          else ())
        gains;
      !ac
  in

  resultString :=
    if todoSp.atype = "LEARN" || todoSp.castable || todoSp.atype = "BREW" then
      resultAc todoSp
    else "REST";
  if !tryT then List.iter (fun x -> incrDic scoreDic x.id 10) path;

  (* Write an action using print_endline *)
  (* To debug: prerr_endline "Debug message"; *)

  (* in the first league: BREW <id> | WAIT; later: BREW <id> | CAST <id> [<times>] | LEARN <id> | REST | WAIT *)
  print_endline
  @@ Printf.sprintf "%s err : %d, time : %f " !resultString !errorRes
  @@ (Sys.time () -. startTime)
done
