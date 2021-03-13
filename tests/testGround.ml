open Array;;

class c (d:int array) (i:int) = 
    object(self)
 val deltas = d
 val id = i
 method deltas = deltas
 method id = id
 method print = print_int(id); print_string(" : "); 
 print_arr(deltas); 
 end;;

let print_arr t = print_string("[");
    for i=0 to (length t) - 1 do
        print_int(t.(i)); print_string(", "); 
        done;print_string("]");;

    let spelList = [(new c [|1;(-3);0;0|] 0);
    (new c [|(-2);0;1;1|] 1); (new c [|0;(-3);2;0|]) 2];;

let rec print_clist l = match l with
|x::xs -> print_int(x#id);print_string(" ; "); print_clist xs;
|[] -> ();;


let gainList = make 4 [];;
let rec getGains spells = match spells with
|x::xs ->
    for i = 0 to 3 do
        if x#deltas.(i) > 0 then begin
            gainList.(i) <- x::gainList.(i);
            print_int(x#id); print_string(" has "); print_int(i);
            print_newline(); end
    done; 
    getGains xs;
    |[] -> (); in
getGains spelList;;
for i = 0 to 3 do
    print_clist gainList.(i);
    print_newline();
    done;;


type poids = Inf | P of int
let (++) a b = match a,b with
    |P(a),P(b) -> P(a+b)
    |_ -> Inf
let (>>) a b = match a,b with
    |P(a), P(b) -> P(a) > P(b)
    |Inf, _ -> true
    |_ -> false
let (++=) a b = a := !a ++ b;;

[1 ; 2 ; 1] -> [1]
[1 ; 2 ; 3 ; 2 ; 3] -> [1 ; 2 ; 3] (* on supprime en partant de la gauche et en cherchant la borne la plus eloignée*)
[1 ; 2 ; 1 ; 3 ; 4 ; 2 ; 6 ; 4] ->
    [1 ; 3 ; 4 ; 2 ; 6 ; 4] (* étape intermédiaire *) -> [1 ; 3 ; 4]