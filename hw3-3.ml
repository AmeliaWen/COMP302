(* TODO: Write some tests for tabulate. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let tabulate_tests: (((int -> int) * int) * int list) list = [
  (((fun x -> x+x), 0),[0]);
  (((fun x -> x*x), 1),[0;1]);
  (((fun x -> x/2), 2),[0;0;1]);
  ((( * ) 2, 3),[0;2;4;6]);
  (* Remember: your test cases should have this form:
     ((f, n), output)
     The following test case asserts that:
       tabulate (fun x -> x) (-1)
     should have the output []
  *)
  (((fun x -> x), -1), [])
]

(* TODO: Implement dist_table: (int * int) -> int -> float list *)
let dist_table ((marblesTotal, marblesDrawn): (int * int)) (x: int) : float list =
  tabulate (fun a -> dist_black a x (marblesTotal, marblesDrawn)) marblesTotal 

(* TODO: Write some test cases for is_empty. *)
let is_empty_tests: (float list list * bool) list = [
  ([[]], true);
  ([[0.0];[];[]], false);
  ([[1.0];[1.0];[1.0]], false);
  ([[];[]], true)
]

(* TODO: Implement is_empty: 'a list list -> bool *)
let is_empty (matrix: 'a list list) : bool =
  List.for_all (fun f -> f=[]) matrix

(* TODO: Implement dist_matrix: int * int -> int list -> float list list *)
let dist_matrix ((total, drawn): int * int) (resultList: int list) : float list list =
  List.map (dist_table (total, drawn)) resultList

(* TODO: Implement combined_dist_table: float list list -> float list *)
let rec combined_dist_table (matrix: float list list) = 
  if is_empty matrix then []
  else (List.fold_left ( *. ) 1.0 (List.map (List.hd) matrix) ):: (combined_dist_table (List.map (List.tl) matrix))

(* Once you have implemented all the above functions, you can
   use this function to compute the maximum likelihood.
   You can try it on the given example by running:
     max_likelihood (6, 3) [2; 0; 1]
*)
let max_likelihood (total, drawn) resultList =
  max_in_list
    (combined_dist_table
       (dist_matrix (total, drawn) resultList))


(* TODO: Implement all: (ingredients list -> bool) -> cake -> bool *)
let rec all (p: (ingredients list -> bool)) (c: cake) : bool = 
  match c with 
  |Slice a -> p a
  |Cake (c1,c2) -> all p c1 && all p c2

(* TODO: Write some test cases for is_chocolate_cake. *)
let is_chocolate_cake_tests = [
  (Slice[Chocolate],true);
  (Cake (Slice [Chocolate ; Flour], Cake (Slice [Chocolate ; Almonds] , Slice [Chocolate ; BlackBeans])), true);
  (Slice[], false);
  (Slice[Orange], false);
  (Cake(Slice[Orange], Slice[Chocolate]), false)
  
]

(* TODO: Implement is_chocolate_cake: cake -> bool *)
let is_chocolate_cake (c: cake) : bool = 
  let rec check l = match l with 
    |[]-> false
    |x::xs -> (x=Chocolate) ||check xs
  in all check c 
    


(* TODO: Implement map: (ingredients list -> ingredients list) -> cake -> cake *)
let rec map (p: (ingredients list -> ingredients list)) (c: cake) = 
  match c with 
  |Slice a -> Slice (p a) 
  |Cake (c1, c2) -> Cake (map p c1, map p c2) 

(* TODO: Write some test cases for add_ingredient. *)
let add_ingredient_tests = [
  ((Orange, Slice [Chocolate]), Slice [Chocolate;Orange]);
  ((Chocolate, Slice [Chocolate]), Slice [Chocolate]);
  ((Chocolate, Slice []), Slice [Chocolate]);
  ((Almonds, Cake(Slice [Chocolate], Slice [Almonds])), Cake(Slice [Chocolate;Almonds], Slice [Almonds]))
]

(* TODO: Implement add_ingredient: ingredients -> cake -> cake *)
let add_ingredient (x: ingredients) (c: cake) : cake = 
  let rec check x l = match l with 
    |[]-> [x]
    |h::t -> if h=x then l else h::(check x t) 
  in map (check x) c 

(* TODO: Implement fold_cake: (ingredients list -> 'a -> 'a) -> 'a -> cake -> 'a  *)
let rec fold_cake (f: (ingredients list -> 'a -> 'a)) (base: 'a) (c: cake) : 'a = 
  match c with 
  |Slice a -> f a base 
  |Cake(c1, c2) -> fold_cake f (fold_cake f base c1) c2


(* TODO: Implement get_all_ingredients: cake -> ingredients list *)
let c = Cake (Slice([Chocolate]), Slice([Almonds])) ;;
let get_all_ingredients (c: cake) : ingredients list = 
  fold_cake union [] c
  
                                                                               
                                                                               

