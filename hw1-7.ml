(* Question 1 *)
(* TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)
(* TODO: Correct these tests for the fact function. *)
let fact_tests = [
  (0, 1.0);
  (1, 1.0);
  (2, 2.0);
  (5, 120.0);
  (3, 6.0);
  (4, 24.0)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int): float = match n with
  | 0 -> 1.0
  | _ -> float_of_int n *. fact (n - 1)


(* TODO: Write your own tests for the binomial function.
         See the provided tests for fact, above, for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((0, 0), 1.0);
  ((1, 0), 1.0);
  ((2, 0), 1.0);
  ((10,1), 10.0);
  ((10,2), 45.0);
  ((5, 0), 1.0);
  ((4, 3), 4.0);
  ((6, 1), 6.0);
  ((5, 2), 10.0);
  ((3, 1), 3.0)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let binomial (n: int) (k:int) =
  if n < 0 then domain ()
  else (if k = n then 1.0
        else fact n /. (fact k  *. fact (n-k)))


(* TODO: Write a good set of tests for ackerman. *)
let ackerman_tests = [
  ((0, 0), 1);
  ((0, 1), 2);
  ((0, 2), 3);
  ((1, 0), 2);
  ((1, 2), 4);
  ((0, 3), 4); 
  ((3, 0), 5);
  ((1,1), 3);
  ((2,2), 7);
  ((1,3),5);
  ((1, 4), 6);
  ((1,5),7)
  (* Your test cases go here *)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let ackerman (n, k)  =
  if n < 0 || k < 0 then domain ()
  else (let rec ack n k = match (n,k) with
      | (0 , _ ) -> k + 1 
      | (_ , 0 ) when n>=1 -> ack (n-1) 1
      | (_ , _ ) when n>=1 && k>=1 -> ack (n-1) (ack n (k-1))
     in ack n k)


(* Question 2: is_prime *)

(* TODO: Write a good set of tests for is_prime. *)
let is_prime_tests = [
  (2, true);
  (3, true);
  (4, false);
  (5, true);
  (6, false);
  (7, true);
  (8, false);
  (9, false);
  (10, false)
(* Your tests go here *)
  
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
exception Domain;;
let is_prime n : bool= 
  match n with
  | 0  -> false
  | 1 -> false
  | _ -> let a= 2 in 
      let rec checkZero a n =
        if (a*a<=n) then
          match n mod a with
            0 -> false
          |_ -> checkZero (a + 1) n 
        else 
          true
      in
      checkZero a n
        
        
          


(* Question 3: Newton-Raphson method for computing the square root
*)

let square_root_tests = [
  (1.0, 1.0);
  
  (4.0, 2.0);
  
  (9.0, 3.0);
  
  (36.0, 6.0)
]

let square_root a =
  let rec findroot x acc =
    if abs_float(x-.(a/.x+.x)/.2.0)<=acc then x
    else findroot ((a/.x+.x)/.2.0) acc 
  in
  if a > 0.0 then
    findroot 1.0 epsilon_float
  else domain ()

 
(* Question 4: Fibonacci*)

(* TODO: Write a good set of tests for fib_tl. *)
let fib_tl_tests = [
  (1, 1);
  (0, 1); 
  (3, 3);
  (2, 2);
  (5, 8); 
]

(* TODO: Implement a tail-recursive helper fib_aux. *)
let rec fib_aux n a b =
  if n==0 then a
  else if n==1 then b
  else fib_aux (n-1) b (a+b)

(* TODO: Implement fib_tl using fib_aux. *)
let fib_tl n =
  fib_aux n 1 1 
  
    
 