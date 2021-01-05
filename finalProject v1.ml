(* Q0  : Get familiar with the external syntax of MiniML *)
let parse_tests : (string * (string, exp) either) list = [
    (* Provide your tests for the parser *)
  ("1;", Right (Int 1)) 
] 
let e1 = (Let
            ([Val
                (Rec
                   ("apply", TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt)),
                    Fn
                      ("f", Some (TArrow (TInt, TInt)),
                       Fn ("x", Some TInt, Apply (Var "f", Var "x")))),
                 "apply")],
             Apply
               (Apply (Var "apply", Fn ("x", None, Primop (Times, [Var "x"; Int 3]))),
                Int 100)));;
let e7 =          
  Let ([Val (Int 1, "x"); Val (Int 1, "y")], Primop (Plus, [Var "y"; Var "x"]))
;;
let e8 =          
  Let ( [Val (Int 1, "x")], Primop (Plus, [Var "x"; Var "x"]))
;;
let e2 =          
  Let ([Val (Primop (Plus, [Var "y"; Var "x"]), "x")], Primop (Plus, [Var "y"; Var "x"]))
;;
let e3 = 
  (Let
     ([Val (Int 3, "x")],
      Let ([Val (Int 4, "x")], Primop (Plus, [Var "x"; Var "x"]))))
;;
let e4 = 
  (Let
     ([Val (Rec ("test", TArrow (TInt, TInt), Fn ("x", Some TInt, Int 3)),
            "test")],
      Int 4))
;;
let e5 = 
  (Let
     ([Valtuple
         (Tuple [Primop (Plus, [Int 2; Int 1]); Primop (Times, [Int 2; Int 50])],
          ["x"; "y"])],
      Primop (Times, [Primop (Times, [Var "x"; Var "x"]); Var "y"])))
;;
let e6 = 
  (Let
     ([Valtuple
         (Tuple [ Primop (Plus, [Var "y"; Var "x"]); Primop (Times, [Int 2; Int 50])],
          ["x"; "y"])],
      Primop (Times, [Primop (Times, [Var "x"; Var "x"]); Var "y"])))
;;
let e9 = Primop (Plus, [Primop (Times, [Int 10; Int 10]); Int 33]);;
let e10 = Let ([Val (Int 3, "x"); Valtuple (Tuple [Var "x"; Int 3], ["y1"; "y2"])],
               Var "w")
;;
let e11= Let ([Val (Let ([Val (Int 3, "y")], Var "z"), "x")], Var "w");;
let e12 = (Let
             ([Val
                 (Rec ("repeat",
                       TArrow (TInt, TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt))),
                       Fn
                         ("n", Some TInt,
                          Fn
                            ("f", Some (TArrow (TInt, TInt)),
                             Fn
                               ("x", Some TInt,
                                If (Primop (Equals, [Var "n"; Int 0]), Var "x",
                                    Apply
                                      (Apply
                                         (Apply (Var "repeat", Primop (Minus, [Var "n"; Int 1])),
                                          Var "f"),
                                       Apply (Var "f", Var "x"))))))),
                  "repeat")],
              Apply
                (Apply (Apply (Var "repeat", Int 4),
                        Fn ("z", Some TInt, Primop (Times, [Var "z"; Int 2]))),
                 Int 100)))
;;
let e13 = (Let
             ([Val
                 (Let ([Val (Int 10, "ten")],
                       Anno (Fn ("y", None, Var "ten"), TArrow (TInt, TInt))),
                  "f")],
              Apply (Var "f", Int 55)))
;;
let e14 = (Let ([ByName (Int 3, "x")], Primop (Plus, [Var "x"; Int 1])))
;;
let free_vars_tests : (exp * name list) list = [
  (Int 10, []);
  (e10, ["x"; "w"]);
  (e1, []);
  (e2,["x";"y"]);
  (e3, []);
  (e4, []);
  (e5, []);
  (e6, ["y";"x"]); 
  (e11, ["z";"w"])
]
(* Q1  : Find the free variables in an expression *)
let rec free_vars (e : exp) : name list = 
  match e with 
  |Int n -> []
  |Bool b -> []
  |If (e, e1, e2) -> union_list [free_vars e; free_vars e1; free_vars e2]
  |Primop (op, args) -> 
      List.fold_right (fun e1 fv -> union_list [free_vars e1; fv]) args []
  |Tuple l -> List.flatten (List.map free_vars l)
  |Var y -> [y]
  |Let (lst, e2) -> 
      let rec listToDelete l = 
        match l with 
        |Val (e1, x) ::xs -> x:: listToDelete xs
        |Valtuple (e1, l) ::xs -> l@(listToDelete xs)
        |ByName(e1, x) :: xs -> x:: listToDelete xs
        |_ -> []
      in 
      let rec freeVar lst = 
        match lst with 
        |Val (e1, x) ::xs -> union_list [free_vars e1; freeVar xs ]
        |Valtuple (e1, l) ::xs -> union_list [free_vars e1; freeVar xs]
        |ByName (e1, x) ::xs-> union_list [free_vars e1; freeVar xs]
        |_ -> []
      in union_list [freeVar lst; (delete (listToDelete lst) (free_vars e2))]
  |Anno (e1, t) -> free_vars e1
  |Fn (x, _, e) -> delete [x] (free_vars e)
  |Rec (x, _, e) -> delete [x] (free_vars e)
  |Apply (e1, e2) -> union_list [free_vars e1; free_vars e2]
                                   
;;            



let unused_vars_tests : (exp * name list) list = [
  (e3, ["x"]);
  (e10, ["x"; "y1"; "y2"]);
  (e11, ["x"; "y"]);
  (e3, ["x"]);
  (e6, []);
  (e4, ["test";"x"]);
  (e2, [])
]

(* Q2  : Check variables are in use *)
let rec unused_vars (e : exp) : name list = 
  match e with 
  |Int n -> []
  |Bool b -> []
  |If (e, e1, e2) -> union_list [unused_vars e; unused_vars e1; unused_vars e2]
  |Primop (op, args)-> 
      List.fold_right (fun e1 fv -> union_list [unused_vars e1; fv]) args []
  |Tuple l -> List.flatten (List.map unused_vars l)
  |Var y -> []
  |Let (lst, e2) -> 
      let rec var_list lst = 
        match lst with 
        |Val (e1, x) :: xs -> union_list [ [x];(var_list xs); (unused_vars e1)]
        |Valtuple (e1, l):: xs -> union_list [l; (var_list xs);(unused_vars e1)]
        |ByName (e1, x)::xs -> union_list[[x];(var_list xs);(unused_vars e1)]
        |_ -> []
      in 
      let rec g l = 
        match l with 
        |[]-> []
        |x::xs -> if List.mem x (free_vars e2) then g xs else x:: (g xs) 
      in union_list [(g (var_list lst));(unused_vars e2)]
  |Anno (e, t) -> unused_vars e
  |Fn (x, _, e) -> if List.mem x (free_vars e) then unused_vars e else x::(unused_vars e) 
  |Rec (x, _, e) -> if List.mem x (free_vars e) then unused_vars e else x::(unused_vars e) 
  |Apply (e1, e2) -> union_list [ unused_vars e1;unused_vars e2]
;;


let subst_tests : (((exp * name) * exp) * exp) list = [
] 
(* Q3  : Substitute a variable *)
let rec subst ((e', x) : exp * name) (e : exp) : exp =
  match e with
  | Var y ->
      if x = y then
        e'
      else
        Var y

  | Int _ | Bool _ -> e
  | Primop (po, es) -> Primop (po, List.map (subst (e', x)) es)
  | If (e1, e2, e3) -> If (subst (e', x) e1, subst (e', x) e2, subst (e', x) e3)
  | Tuple es -> Tuple (List.map (subst (e', x)) es)
  | Anno (e, t) -> Anno (subst (e', x) e, t)

  | Let (ds, e2) -> 
      (let rename = subst in 
       match ds with 
       |[]-> Let([], subst (e', x) e2)
       |Val(e1, y) :: xs -> 
           (let e1'  = subst (e', x) e1 in
            if x = y || (member y (free_vars e')) then 
              let y' = fresh_var y in 
              let e2' = subst (Var y', y) (Let (xs, e2)) in
              (match subst (e', x) e2' with 
               |Let(decs, e2) -> Let(Val(e1', y')::decs, e2)
               |_ -> assert false
              ) 
            else 
              (match subst (e', x) (Let (xs, e2)) with 
               |Let(decs, e2) -> Let(Val(e1', y)::decs, e2)
               |_ -> assert false
              )
           ) 
       |Valtuple(e1, names):: xs -> 
           let e1' = subst (e', x) e1 in 
           let (names', rest) = renameList names e' (Let(xs, e2)) in 
           if member x names then 
             Let(Valtuple(e1', names)::xs, e2)
           else 
             (match subst (e', x) rest with 
              |Let(decs, e2) -> Let(Valtuple(e1', names')::xs, e2) 
              |_ -> assert false) 
       |ByName (e1, y)::xs -> 
           (let e1'  = subst (e', x) e1 in
            if x = y || (member y (free_vars e')) then 
              let y' = fresh_var y in 
              let e2' = rename (Var y', y) (Let (xs, e2)) in
              (match subst (e', x) e2' with 
               |Let(decs, e2) -> Let(ByName(e1', y')::decs, e2)
               |_ -> assert false
              ) 
            else 
              (match subst (e', x) e2 with 
               |Let(decs, e2) -> Let(ByName(e1', y)::decs, e2)
               |_ -> assert false
              )
           )
      ) 
  | Apply (e1, e2) -> Apply ((subst (e', x) e1), (subst (e', x) e2))
  | Fn (y, t, e) -> 
      if x = y then Fn (y, t, e)
      else 
      if member y (free_vars e') then
        let rename = subst in
        let y'= fresh_var y in
        let e1 = rename (Var y', y) e in
        Fn(y', t, subst (e', x) e1)
      else Fn (y, t, subst (e', x) e)
  | Rec (y, t, e) -> 
      if x = y then Rec (y, t, e) else
      if  member y (free_vars e') then
        let rename = subst in
        let y'= fresh_var y in
        let e1 = rename (Var y', y) e in
        Rec(y', t, subst (e', x) e1)
      else Rec (y, t, subst (e', x) e)
and renameList names e' exp = 
  if List.exists (fun x -> member x (free_vars e')) names then 
    let rec renameList2 (names, exp) =
      match (names, exp) with 
      |([], e) -> ([], e) 
      |(x::xs, e) -> 
          let (x', e) = 
            let x'= fresh_var x in 
            (x', subst (Var x', x') e) in 
          let (xs', e) = renameList2 (xs, e) in 
          (x'::xs', e) 
    in renameList2 (names, exp) 
  else (names, exp) 
;;

let eval_tests : (exp * exp) list = [
  (e1, Int 300);
  (e3, Int 8);
  (e4, Int 4);
  (e5, Int 900);
  (e7, Int 2);
  (e8, Int 2);
  (e9, Int 133);
  (e12, Int 1600);
  (e13, Int 10);
  (e14, Int 4)
]

(* Q4  : Evaluate an expression in big-step *)
let rec eval : exp -> exp =
  (* do not change the code from here *)
  let bigstep_depth = ref 0 in
  fun e ->
    if !debug >= 1 then
      print_endline
        (String.make (!bigstep_depth) ' '
         ^ "eval (" ^ Print.exp_to_string e ^ ")\n");
    incr bigstep_depth;
  (* to here *)
    let result =
      match e with
      | Int _ | Bool _ -> e
      | Tuple es -> Tuple (List.map eval es)
      | If (e1, e2, e3) ->
          begin match eval e1 with
            | Bool b ->
                if b then
                  eval e2
                else
                  eval e3
            | _ -> stuck "Condition for if expression should be of the type bool"
          end
      | Anno (e, _) -> eval e     (* types are ignored in evaluation *)
      | Var x -> stuck ("Free variable \"" ^ x ^ "\" during evaluation")

      | Fn (x, t, e) -> Fn (x, t, e)
      | Apply (e1, e2) -> 
          (begin match eval e1 with 
             |Fn(x, _, e) -> 
                 let v2 = eval e2 in 
                 eval (subst (v2, x) e)
             |_ -> stuck "Bad arguments for apply"
           end)
      | Rec (f, t, e) -> eval (subst (Rec(f, t, e), f) e) 

      | Primop (And, es) ->
          (begin match es with 
             |x::xs -> (match eval x with 
                 |Bool true -> (match xs with 
                     |t::ts -> (match ts with 
                         |[]-> eval t
                         |_ -> stuck "Bad arguments for AND")
                     |[]-> stuck "Bad arguments for AND")
                 |Bool false -> Bool false
                 |_ -> stuck "Bad arguments for AND")
             |[]-> stuck "Bad arguments for AND"
           end)
      | Primop (Or, es) -> 
          begin match es with 
            |x::xs -> (match eval x with 
                |Bool false -> (match xs with 
                    |t::ts -> (match ts with 
                        |[]-> eval t
                        |_ -> stuck "Bad arguments for OR")
                    |[]-> stuck "Bad arguments for OR")
                |Bool true -> Bool true
                |_ -> stuck "Bad arguments for OR")
            |[]-> stuck "Bad arguments for OR"
          end 
      | Primop (op, es) ->
          let vs = List.map eval es in
          begin match eval_op op vs with
            | None -> stuck "Bad arguments to primitive operation"
            | Some v -> v
          end

      | Let (ds, e) -> 
          match ds with 
          |[]-> eval e 
          |Val(e1, x)::xs -> 
              let v1 = eval e1 in 
              eval (subst (v1, x) (Let (xs, e)))
          |ByName (e1, x)::xs -> 
              eval (subst (e1, x) (Let (xs, e)))
          |Valtuple (e1, l)::xs -> 
              (let vs = eval e1 in 
               match vs with 
               |Tuple lst -> 
                   let rec sublist l e = 
                     match l with 
                     |[]-> e
                     |(x, e') :: xs -> subst (x, e') (sublist xs e) in 
                   eval (sublist (List.combine lst l) (Let(xs, e)))
               |_ -> stuck "Bad arguments for tuplelet"
              ) 
    in
  (* do not change the code from here *)
    decr bigstep_depth;
    if !debug >= 1 then
      print_endline
        (String.make (!bigstep_depth) ' '
         ^ "result of eval (" ^ Print.exp_to_string e ^ ") = "
         ^ Print.exp_to_string result ^ "\n");
  (* to here *)
    result


let infer_tests : ((context * exp) * typ) list = [
  ((Ctx[], e4), TInt);
  ((Ctx[], e3), TInt);
  ((Ctx[], e7), TInt);
  ((Ctx[], e8), TInt);
  ((Ctx[], e9), TInt);
  ((Ctx[], e5), TInt) 
]
;;


let rec unify (ty1 : typ) (ty2 : typ) : unit = 
  let rec occurs ch t = 
    begin match t with 
      |TInt |TBool -> false
      |TArrow (t1, t2) -> (occurs ch t1)||(occurs ch t2)
      |TProduct l -> List.exists (occurs ch) l 
      |TVar v -> (match !v with 
          |None -> (ch == v) 
          |Some t -> (ch == v) || (occurs ch t))
    end in 
  match (ty1, ty2) with 
  |(TInt, TInt) -> ()
  |(TBool, TBool) -> ()
  |(TArrow (t1, t2), TArrow (t3, t4)) -> if (unify t1 t3 = ()) && (unify t2 t4 = ()) then () else type_fail "fail function type"
  |(TProduct l1, TProduct l2) -> 
      let rec checklist l1 l2 = 
        match (l1, l2) with 
        |([], []) -> ()
        |(h1::t1, h2::t2) -> if (unify h1 h2 = ()) then checklist t1 t2 else type_fail "fail product type"
        |(_, _) -> type_fail "fail product type"
      in checklist l1 l2 
  |(TVar v1, _) -> 
      (match ty2 with 
       |TVar v2 -> 
           begin match !v1, !v2 with 
             |None, None -> ()
             |Some tp1, Some tp2 -> if tp1 = tp2 then () else type_fail "var type does not match "
             |None, Some tp2 -> if (occurs v1 tp2) then type_fail "fail occurrance check" else v1 := Some ty2
             |Some tp1, None -> if (occurs v2 tp1) then type_fail "fail occurrance check " else v2 := Some ty1
           end 
       |_ -> (match !v1 with 
           |None -> if (occurs v1 ty2) then type_fail "fail occurrance" else v1 := Some ty2
           |Some t -> unify t ty2))
  |(_, TVar v2) -> 
      begin match !v2 with 
        |None -> if (occurs v2 ty1) then type_fail "fail occurrance" else v2 := Some ty1
        |Some t -> unify t ty1 
      end
  |_ -> type_fail "other cases"
;;

(* Q5  : Type an expression *)
(* Q7* : Implement the argument type inference
         For this question, move this function below the "unify". *)
let rec infer (ctx : context) (e : exp) : typ = 
  let primoType p = 
    begin match p with 
      |Equals |LessThan | NotEquals | GreaterThan | GreaterEqual | LessEqual -> ([TInt; TInt], TBool) 
      |And | Or -> ([TBool; TBool], TBool)
      |Plus | Minus | Times | Div  -> ([TInt; TInt], TInt) 
      |Negate -> ([TInt], TInt) 
    end in
  match e with 
  |Int _ -> TInt 
  |Bool _ -> TBool 
  |If (e, e1, e2) -> 
      let re = infer ctx e in 
      (match re with 
       |TBool -> let t1 = infer ctx e1 in 
           let t2 = infer ctx e2 in 
           if (*t1 = t2*) unify t1 t2 = () then t2 else type_fail "fail IF"
       |_ -> type_fail "fail IF") 
  |Primop (op, es) -> 
      let (expected, result) = primoType op in 
      let inferred = List.map (infer ctx) es in 
      let rec compare tlist1 tlist2 = match tlist1, tlist2 with 
        |[], []->  result 
        |t::tlist, s::slist -> if (unify t s)=() (*t = s*) then compare tlist slist else type_fail "fail op"
        |_, _-> type_fail "failop2"
      in compare expected inferred 
  |Tuple es -> let list = List.map (infer ctx) es in 
      TProduct list 
  |Fn (x, t, e) -> 
      let Ctx l = ctx in 
      (match t with 
       |Some tp -> TArrow (tp, (infer (Ctx ((x, tp)::l)) e))
       |_ -> let tv = fresh_tvar () in 
           if (unify tv (infer (Ctx ((x, tv)::l)) e)) = () then 
             match tv with 
             |TVar value -> 
                 (match !value with 
                  |Some v ->
                      TArrow (TVar (ref (Some v)), (infer (Ctx ((x, v)::l)) e))
                  |_ -> type_fail "fail funtion")
             |_ -> type_fail "fail funtion"
                         
           else type_fail "fail funtion"
           (*if unify (infer (Ctx ((x, tv)::l)) Fn (x, tv, e)) (TArrow (tv,infer (Ctx ((x, tv)::l) e))) = () then *)
             
               
      ) 
  |Rec (f, t, e) -> infer (extend ctx (f, t)) e 
  |Let (dec, e) -> 
      let rec tylist dec acc = 
        begin match dec with 
          |[]-> acc
          |h::t -> 
              begin match h with 
                |Val(e, x) -> tylist t (extend acc (x, (infer acc e)))
                |ByName(e, x) -> tylist t (extend acc (x, (infer acc e)))
                |Valtuple(e, vl) -> (match infer acc e with 
                    |TProduct tl -> 
                        let rec matchlist vl tl acc = 
                          match (vl, tl) with 
                          |([], []) -> acc
                          |(h1::t1, h2::t2) -> matchlist t1 t2 (extend acc (h1, h2)) 
                          |(_, _) -> type_fail "valtuple does not match "
                        in 
                        let result = matchlist vl tl (acc) in 
                        tylist t result
                    |_ -> type_fail "valtuple does not match"
                  )
                  
              end
        end 
      in infer (tylist dec ctx) e 
  |Apply (e1, e2) -> (match infer ctx e1 with 
      |TArrow(t1, t2) -> let tp = infer ctx e2 in 
          if (*tp = t1*) unify tp t1 = () then t2 else type_fail "fail function application"
      |_ -> type_fail "fail function application")
  |Var x -> ctx_lookup ctx x 
  |Anno (e, tp) -> tp
;;


let unify_tests : ((typ * typ) * unit) list = [ 
]



    (*
(* find the next function for Q5 *)
(* Q6  : Unify two types *)
      let rec unify (ty1 : typ) (ty2 : typ) : unit = 
        let rec occurs ch t = 
          begin match t with 
            |TInt |TBool -> false
            |TArrow (t1, t2) -> (occurs ch t1)||(occurs ch t2)
            |TProduct l -> List.exists (occurs ch) l 
            |TVar v -> (match !v with 
                |None -> (ch == v) 
                |Some t -> (ch == v) || (occurs ch t))
          end in 
        match (ty1, ty2) with 
        |(TInt, TInt) -> ()
        |(TBool, TBool) -> ()
        |(TArrow (t1, t2), TArrow (t3, t4)) -> if (unify t1 t3 = ()) && (unify t2 t4 = ()) then () else type_fail "fail function type"
        |(TProduct l1, TProduct l2) -> 
            let rec checklist l1 l2 = 
              match (l1, l2) with 
              |([], []) -> ()
              |(h1::t1, h2::t2) -> if (unify h1 h2 = ()) then checklist t1 t2 else type_fail "fail product type"
              |(_, _) -> type_fail "fail product type"
            in checklist l1 l2 
        |(TVar v1, _) -> 
            (match ty2 with 
             |TVar v2 -> 
                 begin match !v1, !v2 with 
                   |None, None -> ()
                   |Some tp1, Some tp2 -> if tp1 = tp2 then () else type_fail "var type does not match "
                   |None, Some tp2 -> if (occurs v1 tp2) then type_fail "fail occurrance check" else v1 := Some ty2
                   |Some tp1, None -> if (occurs v2 tp1) then type_fail "fail occurrance check " else v2 := Some ty1
                 end 
             |_ -> (match !v1 with 
                 |None -> if (occurs v1 ty2) then type_fail "fail occurrance" else v1 := Some ty2
                 |Some t -> unify t ty2))
        |(_, TVar v2) -> 
            begin match !v2 with 
              |None -> if (occurs v2 ty1) then type_fail "fail occurrance" else v2 := Some ty1
              |Some t -> unify t ty1 
            end
        |_ -> type_fail "other cases"*)
          
        
                     

(* Now you can play with the language that you've implemented! *)
let execute (s: string) : unit =
  match P.parse s with
  | Left s -> print_endline ("parsing failed: " ^ s)
  | Right e ->
      try
       (* first we type check the program *)
        ignore (infer (Ctx []) e);
        let result = eval e in
        print_endline ("program is evaluated to: " ^ Print.exp_to_string result)
      with
      | NotImplemented -> print_endline "code is not fully implemented"
      | Stuck s -> print_endline ("evaluation got stuck: " ^ s)
      | NotFound -> print_endline "variable lookup failed"
      | TypeError s -> print_endline ("type error: " ^ s)
      | e -> print_endline ("unknown failure: " ^ Printexc.to_string e)


(************************************************************
 *             Do not change these functions.               *
 *               They are needed for tests.                 *
 ************************************************************)
let list_to_string el_to_string l : string =
  List.fold_left
    begin fun acc el ->
      if acc = "" then
        el_to_string el
      else
        acc ^ "; " ^ el_to_string el
    end
    ""
    l
  |> fun str -> "[" ^ str ^ "]"

let run_test name f ts stringify : unit =
  List.iteri
    begin fun idx (input, expected_output) ->
      try
        let output = f input in
        if output <> expected_output then
          begin
            print_string (name ^ " test #" ^ string_of_int idx ^ " failed\n");
            print_string (stringify output ^ " <> " ^ stringify expected_output)
          end
      with
      | exn ->
          print_string (name ^ " test #" ^ string_of_int idx ^ " raised an exception:\n");
          print_string (Printexc.to_string exn)
    end
    ts

let run_free_vars_tests () : unit =
  run_test "free_vars" free_vars free_vars_tests (list_to_string (fun x -> x))

let run_unused_vars_tests () : unit =
  run_test "unused_vars" unused_vars unused_vars_tests (list_to_string (fun x -> x))

let run_subst_tests () : unit =
  run_test "subst" (fun (s, e) -> subst s e) subst_tests Print.exp_to_string

let run_eval_tests () : unit =
  run_test "eval" eval eval_tests Print.exp_to_string

let run_infer_tests () : unit =
  run_test "infer" (fun (ctx, e) -> infer ctx e) infer_tests Print.typ_to_string

let run_unify_tests () : unit =
  run_test "unify" (fun (ty1, ty2) -> unify ty1 ty2) unify_tests (fun () -> "()")

let run_all_tests () : unit =
  run_free_vars_tests ();
  run_unused_vars_tests ();
  run_subst_tests ();
  run_eval_tests ();
  run_infer_tests ();
  run_unify_tests ()
