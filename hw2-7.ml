(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* Q1 *)
(* TODO: Write a good set of tests for compress *)
let compress_tests = [
  ([A;A;A;A],[(4,A)]);
  ([A;T;A;A],[(1,A);(1,T);(2,A)]);
  ([T;T;C;C],[(2,T);(2,C)]);
  ([A;A;A;T],[(3,A);(1,T)]);
  ([A],[(1,A)]);
  ([],[])
]

(* TODO: Implement compress. *)
let compress (l : nucleobase list) : (int * nucleobase) list =
  match l with 
  |[]->[]
  |h::t -> let rec helper (i, h, t) =
             match t with 
             |[]-> [(i, h)] 
             |t::tail -> if (h=t) then (helper (i+1,t, tail))
                 else  (i, h)::(helper (1,t, tail)) 
      in helper (1, h,t)
          
        

(* TODO: Write a good set of tests for decompress *)
let decompress_tests = [
  ([(4,A)],[A;A;A;A]);
  ([(1,A);(1,T);(2,A)],[A;T;A;A]);
  ([(2,T);(2,C)],[T;T;C;C]);
  ([(3,A);(1,T)],[A;A;A;T]);
  ([(1,A)],[A]);
  ([],[])
]

(* TODO: Implement decompress. *) 
let rec decompress (l : (int * nucleobase) list) : nucleobase list =
  match l with 
  |[]->[]
  |(a,b)::t -> match (a,b,t) with 
    |(0, _, [])-> []
    |(0, _, t) -> decompress t
    |(_, _, [])-> b::(decompress [(a-1,b)])
    |(_,_,t) -> b:: (decompress ((a-1, b)::t))
                  
        


(* Q2 *)
(* TODO: Write a good set of tests for eval *)
let eval_tests = [
  (MULT (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0), 27.5);
  (PLUS (FLOAT 2.0, FLOAT 3.0), 5.0);
  (FLOAT 1.0, 1.0);
  (SIN (FLOAT 2.0), 0.909297426825681709);
  (EXP (FLOAT 3.0), 20.0855369231876679)
]

(* TODO: Implement eval. *)
let rec eval e =
  match e with 
  |FLOAT a -> a 
  |PLUS (a, b) -> (eval a)+.(eval b)
  |MINUS (a, b) -> (eval a)-.(eval b)
  |MULT (a, b) -> (eval a)*.(eval b)
  |DIV (a, b) -> (eval a)/.(eval b)
  |SIN a -> sin(eval a)
  |COS a -> cos(eval a)
  |EXP a -> exp(eval a)

(* TODO: Write a good set of tests for to_instr *)
let to_instr_tests = [
  (MULT (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0),[Float 2.2; Float 3.3; Plus; Float 5.; Mult]);
  (PLUS (FLOAT 2.0, FLOAT 3.0), [Float 2.0; Float 3.0; Plus]);
  (FLOAT 1.0, [Float 1.0]);
  (SIN (FLOAT 2.0), [Float 2.0; Sin]);
  (EXP (FLOAT 3.0), [Float 3.0; Exp])
]

(* TODO: Implement to_instr. *)
let rec to_instr e = 
  match e with 
  |FLOAT a-> [Float a]
  |MULT (a, b) -> (to_instr a)@(to_instr b)@[Mult]
  |PLUS (a, b) -> (to_instr a)@(to_instr b)@[Plus]
  |MINUS (a, b) -> (to_instr a)@(to_instr b)@[Minus]
  |DIV (a, b) -> (to_instr a)@(to_instr b)@[Div]
  |SIN a -> (to_instr a)@[Sin]
  |COS a -> (to_instr a)@[Cos]
  |EXP a -> (to_instr a)@[Exp]
                         
                                             
                                            


(* TODO: Write a good set of tests for instr *)
let instr_tests = [
  ((Mult, [5.0;5.5]), (Some [27.5]));
  ((Plus, [2.2;3.3;5.0]), (Some [5.5;5.0]));
  ((Float 4.2, [5.0;5.5]), (Some [4.2;5.0;5.5]));
  ((Mult, [5.5]), (None)); 
  ((Sin, [2.0;5.5]), (Some [0.909297426825681709;5.5]));
  ((Exp, [3.0;5.5]), (Some [20.0855369231876679;5.5]));
  
]


(* TODO: Implement to_instr. *)               
let instr i s = 
  match (i,s) with 
  |(Mult, s) -> (match s with 
      |a::b::t -> Some ((a*.b)::t)
      |_ -> None )
  |(Plus, s) -> (match s with 
      |a::b::t -> Some ((a+.b)::t)
      |_ -> None )
  |(Minus, s) -> (match s with 
      |a::b::t -> Some ((a-.b)::t)
      |_ -> None )
  |(Div, s) -> (match s with 
      |a::b::t -> Some ((a/.b)::t)
      |_ -> None )
  |(Sin, s)-> (match s with 
      |a::b -> Some ((sin a)::b) 
      |_ -> None )
  |(Cos, s)-> (match s with 
      |a::b -> Some ((cos a)::b)
      |_ -> None )
  |(Exp, s)-> (match s with 
      |a::b -> Some ((exp a)::b) 
      |_ -> None )
  |(Float a, s) -> Some (a::s)
                     
                   


(* TODO: Write a good set of tests for prog *)
let prog_tests = [
  ([Float 2.2; Float 3.3; Plus; Float 5.; Mult],Some 27.5);
  ([Float 2.2; Plus], None);
  ([Float 2.0;Sin],Some 0.909297426825681709) 
]

let prog instrs = 
  match instrs with 
  |[]->Some 0.0 
  |lst -> let rec helper (lst , stack) = 
            match (lst, stack) with 
            |([], [])-> Some 0.0
            |([], s)-> (match s with 
                |[z]-> Some z
                | _ -> None) 
            |(a::b, s) -> (match a with 
                |Float x-> helper (b , x::s)
                |i -> match (instr i s) with 
                  |None -> None
                  |Some y -> helper (b, y))
      in helper (lst, [])
                   
