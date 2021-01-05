(* TODO: Write some tests for neighbours. Consider creating a graph first,
 and then writing your tests based on it *)

(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)
let g1  = {nodes = ["a"; "b"; "c"]; edges = [("a", "b", 1); ("a", "c", 2)]}
let g2  = {nodes = ["a"; "b"; "c"; "d"]; edges = [("a", "b", 1); ("a", "c", 2);("b","d",3)]}
(* We've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let neighbours_tests: ((string graph * string) * (string * weight) list) list = [
  ((g1, "a"),([("b", 1); ("c", 2)])) ;
  ((g2, "b"),([("d",3)])) ;
  ((g1, "c"),([])) 
]

(* TODO: Implement neighbours. *)
let neighbours (g: 'a graph) (vertex: 'a) : ('a * weight) list =
  let h x y = match x with 
    |(c, d, e) -> if vertex = c then (d, e)::y else y
  in List.fold_right h g.edges []

(* TODO: Implement find_path. *)
let find_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node node visited = match visited with 
    |[]-> false 
    |x::xs -> if x != node then aux_node node xs else true
  and aux_list nodes visited =
    match nodes with 
    |[]-> raise Fail 
    |(x, w)::xs -> 
        if aux_node x visited then aux_list xs visited else 
        if x=b then ([x], w) 
        else 
          try aux_list xs (x::visited) with Fail ->
            match aux_list (neighbours g x) (x::visited) with 
            |(a, b) -> (x::a, w+b)
                         (*((x::(List.map (fun (x, y)-> x) (aux_list (neighbours g x) (x::visited)))) , w)
else if x = b then ([x], w)
else try aux_list xs (x::visited) with Fail ->
  ((x:: aux_list (neighbours g x) (x::visited)), w)*) 
  in
  aux_list ([(a, 0)]) []

    (*TODO: Implement find_path'. *)
let find_path' (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node node visited fc sc =
    match visited with 
    |[]-> fc () 
    |x::xs -> if x != node then aux_node node xs fc sc else sc ()
  and aux_list nodes visited fc sc =
    match nodes with 
    |[]-> fc () 
    |(x, w)::xs -> 
        if aux_node x visited (fun ()-> false) (fun()-> true) then aux_list xs visited fc sc else 
        if x=b then  sc ([x], w) 
        else 
          aux_list xs (x::visited) (fun ()-> (aux_list (neighbours g x) (x::visited) fc (fun (a, b) -> sc (x::a, w+b)))) sc
            (*try aux_list xs (x::visited) with Fail ->
               match aux_list (neighbours g x) (x::visited) with 
               |(a, b) -> (x::a, w+b)*)
  in
  aux_list ([(a, 0)]) [] (fun ()-> raise Fail) (fun (x, y)->(x, y)) 


(* TODO: Implement find_all_paths *)
let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list = 
  let rec aux_node node visited =
    match visited with 
    |[]-> false 
    |x::xs -> if x != node then aux_node node xs else true  
  and aux_one_list nodes visited = 
    match nodes with 
    |[]-> ([], 0)
    |(x, w)::xs -> 
        if aux_node x visited then aux_one_list xs visited else 
        if x=b then ([x], w) 
        else 
          match aux_one_list xs (x::visited) with 
          |(c, d)  -> 
              match aux_one_list (neighbours g x) (x::visited) with 
              |(a, b) -> (x::a, w+b)
                         
  and aux_list nodes visited fc sc = 
    match nodes with 
    |[]-> fc () 
    |(x, w)::xs -> 
        if aux_node x visited then aux_list xs visited fc sc else 
        if x=b then  sc ([x], w) 
        else 
          let n = neighbours g x in match n with 
          |[] -> fc ()
          |h::t -> 
              aux_list xs (x::visited) (fun ()-> (aux_list (neighbours g x) (x::visited) fc (fun (a, b) -> sc (x::a, w+b)))) sc
            (*List.map (fun (a,b) -> aux_one_list [(a, b)] (x::visited)) (neighbours g x)(*match aux_list xs (x::visited) cont with*) 
            |[] ->*)(* List.concat (aux_one_list xs (x::visited)) *) 
            (*|a -> aux_list xs (x::visited) (fun xs -> cont (List.map (fun (a, b) -> (x::a, w+b)) xs))*)
            
            (*match aux_list c (x::visited) with 
             |(a, b)::xs -> (x::a, w+b)) [n]))*)
            (*match aux_list xs (x::visited) with 
             |[]-> List.map (fun (a, b) -> (x::a, w+b)) (aux_list ((neighbours g x)) (x::visited))
             |(a, b)::ls-> (x::a, w+b)::(aux_list ls (x::visited))*)
                  (* List.map (fun (a, b) -> (x::a, w+b)) (aux_list ((neighbours g x)) (x::visited))*)
            (*let n = neighbours g x in 
             List.map (fun c -> aux_list (c::xs) (x::visited)) n*)
            (* List.map (fun (a, b) -> (x::a, w+b)) (List.map (fun c -> aux_list (c::xs) (x::visited)) n)*)
          (*(List.map (fun (a, b) -> (x::a, w+b)) (aux_list ((neighbours g x)) (x::visited)))@(aux_list xs (x::visited))*)
          (*List.map (fun (a, b) -> (x::a, w+b)) ((aux_list (neighbours g x) (x::visited))@(aux_list xs (x::visited)))*)
                         (* match aux_list xs (x::visited) with 
                           |[]-> List.map (fun (a, b) -> (x::a, w+b)) (aux_list ((neighbours g x)) (x::visited))
                           |a -> a*)
                  (*(List.map (fun (a, b) -> (x::a, w+b)) (aux_list ((neighbours g x)) (x::visited)))@(List.map (fun (a, b) -> (x::a, w+b)) (aux_list (xs) (x::visited)))*)
            (* match aux_list xs (x::visited) with 
              |[]-> (List.map (fun (a, b) -> (x::a, w+b) ) (List.map (fun c -> aux_list c (x::visited)) [neighbours g x]))
              |a ->a *) 
                  (*match aux_list (neighbours g x) (x::visited) with 
                   |(a, b) -> (x::a, w+b)
                   let n = neighbours g x in 
                   List.concat ((List.map (fun c -> 
                       match aux_list c (x::visited) with 
                       |(a, b)::xs -> (x::a, w+b)) [n]))
                 |(a,b)::_ -> [(x::a, w+b)] *)  
          (*try aux_list xs (x::visited) with Fail ->
             match aux_list (neighbours g x) (x::visited) with 
             |(a, b) -> (x::a, w+b) *)
  in
  aux_list ([(a, 0)]) []   
    (*let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list = 
       let rec neighbors g a cond =
         let h x y = match x with 
           |(c, d, e) -> if a = c && cond c then (d, e)::y else y
         in List.fold_right h g.edges []
      
       and list_path g a to_b = match to_b with
         | [] -> [] (* [to_b] contains the path to [b]. *)
         | (a', w) :: _ ->
             if a' = a then [([to_b], w)]
             else
               let n = neighbors g a' (fun c -> not(List.mem c to_b)) in
               List.concat(List.map (fun c -> list_path g a (c :: to_b)) n)
       in list_path g a [b]*)
let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list =
  let rec aux_node node visited =
    match visited with 
    |[]-> false 
    |x::xs -> if x != node then aux_node node xs else true  
  and rec aux_node nodes visited = 
    

(* TODO: Implement find_shortest_path *)
    let find_shortest_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) option = 
      raise NotImplemented    