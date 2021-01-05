(*------------------ Q1------------------ *)
let rec parseExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  parseSExp
    toklist
    (fun toklist' exp -> match toklist' with
       | SEMICOLON :: toklist'' -> sc toklist'' exp
       | _ -> raise (Error "Expected a single semicolon"))

and parseSExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a = 
  parsePExp 
    toklist
    (fun toklist' exp -> match toklist' with 
       |PLUS:: toklist'' -> parseSExp toklist'' (fun list exp' -> sc list (Sum (exp, exp')))
       |SUB::toklist'' -> parseSExp toklist'' (fun list exp' -> sc list (Minus (exp, exp'))) 
       |a -> sc a exp
               
               
    )

and parsePExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  parseAtom 
    toklist 
    (fun toklist' exp -> match toklist' with 
       |TIMES:: toklist'' -> parsePExp toklist'' (fun list exp' -> sc list (Prod (exp, exp')))
       |DIV::toklist'' -> parsePExp toklist'' (fun list exp' -> sc list (Div (exp, exp')))
       |a -> sc a exp
               
    )

and parseAtom (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  match toklist with 
  |INT n ::xs -> sc xs (Int n)(*parseAtom xs (fun list exp -> sc xs (Int n))*)
                   (*|a -> parseSExp a sc*)(*parseAtom xs (fun list exp -> sc xs (Int n))*)
  |LPAREN::xs ->
      parseSExp xs (fun list exp -> match list with 
          |RPAREN::tl -> sc tl exp 
          |_ -> sc [] exp )
        (*(match xs with 
            |LPAREN::xs -> parseAtom xs sc
                       (*|INT n ::xs ->parseAtom xs (fun list exp -> sc xs (Int n))*)
            |a -> 
                parseSExp a (fun list exp -> match list with 
                    |RPAREN::tl -> (let rec filter list = match list with 
|RPAREN::tll -> filter tll
|a -> a
in sc (filter tl) exp )
|_ -> sc [] exp))*)(*(fun toklist' exp -> match toklist' with 
|RPAREN :: tl -> (match tl with 
|RPAREN::t -> sc t exp 
|_-> sc tl exp)
|_ -> sc [] exp))*)
  |_ -> sc [] (Int 0)

(* parse : string -> exp *)
let parse string =
  parseExp
    (lex string)
    (fun s e -> match s with
       | [] -> e
       | _ -> raise (Error "Incomplete expression"))

(* eval : string -> int *)
let eval e = eval' (parse e)

(* ---------- Hamming Numbers ----------- *)

let rec merge s1 s2 = 
  {hd = if s1.hd < s2.hd then s1.hd else s2.hd ;
   tl = Susp (fun ()-> if s1.hd < s2.hd then merge (force s1.tl) s2 
               else if s1.hd = s2.hd then merge (force s1.tl) (force s2.tl)
               else merge s1 (force s2.tl))
  }

let rec hamming_series = 
  let rec numsFrom n = 
    {hd = n;
     tl = Susp (fun ()-> numsFrom (n+1))
    } in 
  merge (merge  (times 2 (numsFrom 1)) (times 3 (numsFrom 1))) (times 5 (numsFrom 1))
  (*{hd = 1;
    tl = Susp (fun ()-> merge (merge  (times 2 (numsFrom 1)) (numsFrom 1)) (times 5 (numsFrom 1)))
   }*)