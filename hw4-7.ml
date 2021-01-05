(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points)                                     *)
(* ------------------------------------------------------------------------*)

let new_account (p: passwd) : bank_account = 
  let pwd = ref p in 
  let balance = ref 0 in 
  let x = ref 0 in 
  
  { 
    update_passwd = (fun oldp-> fun newp -> 
        if (!pwd = oldp) then ((pwd := newp);(x:=0))  else 
          ((x:=!x+1); raise wrong_pass;) );
    deposit = (fun newp -> fun money -> 
        if (!x>=3) then raise too_many_attempts
        else if (!pwd = newp) then ((balance := !balance + money);(x:=0)) else ((x:=!x+1); raise wrong_pass;));
    retrieve = (fun newp -> fun money -> 
        if (!x >= 3) then raise too_many_attempts
        else if (!pwd= newp)&&(!balance>=money) then ((balance := !balance - money);(x:=0)) 
        else if (!pwd= newp)&&(!balance<money) then ((x:=0);raise no_money;)
        else ((x:=!x+1); raise wrong_pass;)); 
    print_balance = (fun newp -> 
        if (!pwd= newp)&&(!x<3) then ((x:=0);(!balance;)) else if 
          (!pwd != newp) &&(!x<3) then ((x:=!x+1); raise wrong_pass;) else  raise too_many_attempts);
  }
;;



(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* Q 2.1 : Counting how many function calls are made *)
(*let rec fib n count = if n = 0 then 0
   else (if n = 1 then 1  else count := !count+1;fib (n-1) + fib (n-2)) ;;*)
let rec fib_I (n: int) : fib_result =
  {num_rec =  (let x= ref 0 in 
               let rec fib' n = 
                 
                 if n = 0 then (();1)
                 else (if n=1 then (();1) else 
                         ((x := 1+ fib' (n-2)+fib' (n-1););!x;)) 
               in fib' n );
   result = (let rec fib n = if n = 0 then 0
               else (if n = 1 then 1 else fib (n-2) + fib (n-1))
             in fib n)
       
  }
     
;;


(* Q 2.2 : Memoization with a global store *)

let fib_memo (n: int) : int =
  
  let rec fib n = 
    match Hashtbl.find_opt store n with 
    |Some v -> v 
    |None -> 
        let fib' n = if n = 0 then 0
          else (if n = 1 then 1 else fib (n-2) + fib (n-1))
        in 
        let x = fib' n in ((Hashtbl.add store n x);x;)
  in
  fib n
;;


(* Q 2.3 : General memoization function *)

let memo (f: (('a -> 'b) -> 'a -> 'b)) (stats: stats) : ('a -> 'b) =
  
  let store = Hashtbl.create 1000 in 
  let rec g a = 
    match Hashtbl.find_opt store a with 
    |Some v -> 
        incr stats.lkp;
        (v)
    |None ->
        let h = f g a in 
        incr stats.entries;
        Hashtbl.add store a h;
        h
  in g 
;;



(* Q 2.4 : Using memo to efficiently compute the Fibonacci number *)
(* We also accept let fibM = raise NotImplemented ;; *)
let fibM = 
  let stats = { entries = ref 0; lkp = ref 0} in 
  let fibM' g n1 = if n1 = 0 then 0
    else (if n1 = 1 then 1 else g (n1 - 2) + g (n1 - 1))  in
  let g' = memo fibM' stats in 
  fun n -> ((g' n), stats)
      (*let res = g' n in (res,stats)*)
    (*let fibM = 
       let rec f g n = if n = 0 then 0
         else (if n = 1 then 1 else g (n-2) + g (n-1)) in 
       let h f = (memo f) in fun n -> 
         let x : stats = {entries = ref 0; lkp= ref 0} in 
         ((h f x n), x) 
     ;;*)
(*
  let fibM (n: int) : (int * stats) =
    let x : stats = {entries = ref 0; lkp= ref 0} in 
    ((memo (fun g n -> if n = 0 then 0
             else (if n = 1 then 1 else g (n-2) + g (n-1))) x n), x)*)
  
  
  

