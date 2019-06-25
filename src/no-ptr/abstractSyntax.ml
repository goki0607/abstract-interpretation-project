(* file abstractSyntax.ml  © P. Cousot 2018 *)

open AbstractSyntaxExpressions
open AbstractTree

let at_stmt s = match s with 
    | Prog (sl, (at,atP,af,afP,es,br,brP)) -> at
    | Assign (v, a, (at,atP,af,afP,es,br,brP)) -> at
    | Emptystmt (at,atP,af,afP,es,br,brP) -> at
    | If (b, st, (at,atP,af,afP,es,br,brP)) -> at
    | Ifelse (b, st, se, (at,atP,af,afP,es,br,brP)) -> at
    | While (b, sb, (at,atP,af,afP,es,br,brP)) -> at
    | Break (at,atP,af,afP,es,br,brP) -> at
    | Stmtlist (sl, (at,atP,af,afP,es,br,brP)) -> at

let select_at (at,atP,af,afP,es,br,brP) = atP
let property_at s = match s with 
    | Prog (sl, att) -> select_at att
    | Assign (v, a, att) -> select_at att
    | Emptystmt att -> select_at att
    | If (b, st, att)-> select_at att
    | Ifelse (b, st, se, att) -> select_at att
    | While (b, sb, att) -> select_at att
    | Break att -> select_at att
    | Stmtlist (sl, att) -> select_at att

let after_stmt s = match s with
    | Prog (sl, (at,atP,af,afP,es,br,brP)) -> af
    | Assign (v, a, (at,atP,af,afP,es,br,brP)) -> af
    | Emptystmt (at,atP,af,afP,es,br,brP) -> af
    | If (b, st, (at,atP,af,afP,es,br,brP))-> af
    | Ifelse (b, st, se, (at,atP,af,afP,es,br,brP)) -> af
    | While (b, sb, (at,atP,af,afP,es,br,brP)) -> af
    | Break (at,atP,af,afP,es,br,brP) -> af
    | Stmtlist (sl, (at,atP,af,afP,es,br,brP)) -> af

let select_after (at,atP,af,afP,es,br,brP) = afP
let property_after s = match s with 
    | Prog (sl, att) -> select_after att
    | Assign (v, a, att) -> select_after att
    | Emptystmt att -> select_after att
    | If (b, st, att)-> select_after att
    | Ifelse (b, st, se, att) -> select_after att
    | While (b, sb, att) -> select_after att
    | Break att -> select_after att
    | Stmtlist (sl, att) -> select_after att

let escape_stmt s = match s with 
    | Prog (sl, (at,atP,af,afP,es,br,brP)) -> es
    | Assign (v, a, (at,atP,af,afP,es,br,brP)) -> es
    | Emptystmt (at,atP,af,afP,es,br,brP) -> es
    | If (b, st, (at,atP,af,afP,es,br,brP))-> es
    | Ifelse (b, st, se, (at,atP,af,afP,es,br,brP)) -> es
    | While (b, sb, (at,atP,af,afP,es,br,brP)) -> es
    | Break (at,atP,af,afP,es,br,brP) -> es
    | Stmtlist (sl, (at,atP,af,afP,es,br,brP)) -> es

(* where the break statements go to if escape_stmt is true *)
let break_stmt s = match s with 
    | Prog (sl, (at,atP,af,afP,es,br,brP)) -> br
    | Assign (v, a, (at,atP,af,afP,es,br,brP)) -> br
    | Emptystmt (at,atP,af,afP,es,br,brP) -> br
    | If (b, st, (at,atP,af,afP,es,br,brP))-> br
    | Ifelse (b, st, se, (at,atP,af,afP,es,br,brP)) -> br
    | While (b, sb, (at,atP,af,afP,es,br,brP)) -> br
    | Break (at,atP,af,afP,es,br,brP) -> br
    | Stmtlist (sl, (at,atP,af,afP,es,br,brP)) -> br
    
(* join of the properties at the break statements if escape_stmt is true *)
let select_break (at,atP,af,afP,es,br,brP) = brP
let property_break s = match s with 
    | Prog (sl, att) -> select_break att
    | Assign (v, a, att) -> select_break att
    | Emptystmt att -> select_break att
    | If (b, st, att)-> select_break att
    | Ifelse (b, st, se, att) -> select_break att
    | While (b, sb, att) -> select_break att
    | Break att -> select_break att
    | Stmtlist (sl, att) -> select_break att

let rec is_in_stmt l s = match s with 
    | Prog (sl, (at,atP,af,afP,es,br,brP)) -> (l=at || l=af || is_in_stmt_list l sl)
    | Assign (v, a, (at,atP,af,afP,es,br,brP)) -> (l=at)
    | Emptystmt (at,atP,af,afP,es,br,brP) -> (l=at)
    | If (b, st, (at,atP,af,afP,es,br,brP))-> (l=at || is_in_stmt l st)
    | Ifelse (b, st, se, (at,atP,af,afP,es,br,brP)) -> (l=at || is_in_stmt l st || is_in_stmt l se)
    | While (b, sb, (at,atP,af,afP,es,br,brP)) -> (l=at || is_in_stmt l sb)
    | Break (at,atP,af,afP,es,br,brP) -> (l=at)
    | Stmtlist (sl, (at,atP,af,afP,es,br,brP)) -> (l=at || is_in_stmt_list l sl)
and is_in_stmt_list l sl = match sl with
    | [] -> false
    | s' :: sl' -> (is_in_stmt l s') || (is_in_stmt_list l sl');;
   
let void_label = 0

(* void_label the bare parse tree with a void labelling *)
let rec void_label_tree (s:bare_tree) : labelled_tree =  
   let void_labelling = let bot = AbstractDomain.bot () in
      (void_label, bot, void_label, bot, false, void_label, bot)  
   in match s with
    | Prog (sl, _) -> Prog (void_label_tree_list sl, void_labelling)
    | Assign (v, a, _) ->  Assign (v, a,  void_labelling)
    | Emptystmt _ -> Emptystmt  void_labelling
    | If (b, st, _) -> If (b, void_label_tree st,  void_labelling)
    | Ifelse (b, st, se, _) -> Ifelse (b, void_label_tree st, void_label_tree se,  void_labelling)
    | While (b, sb, _) -> While (b, void_label_tree sb, void_labelling)
    | Break _ -> Break void_labelling
    | Stmtlist (sl, _) -> Stmtlist (void_label_tree_list sl, void_labelling)
and void_label_tree_list sl = match sl with 
    | [] -> []
    | [s'] -> [void_label_tree s']
     | s' :: sl' -> (* trees in inverse order *)
                    (void_label_tree s') :: (void_label_tree_list sl') ;;


(* set the at label in the void_labeld parse tree statement s, from l *)
let rec set_at_tree s l =  match s with (* l is the last label set *)
    | Prog (sl, (at,atP,af,afP,es,br,brP)) -> let (sl', next) = set_at_tree_list sl l in
                                      (Prog (sl', (l,atP,af,afP,es,br,brP)), next)
    | Assign (v, a, (at,atP,af,afP,es,br,brP)) -> (Assign (v, a, (l,atP,af,afP,es,br,brP)), l+1)
    | Emptystmt (at,atP,af,afP,es,br,brP) -> (Emptystmt (l,atP,af,afP,es,br,brP), l+1)
    | If (b, st, (at,atP,af,afP,es,br,brP))-> let (st', next) = set_at_tree st (l+1) in
                                       (If (b, st', (l,atP,af,afP,es,br,brP)), next)
    | Ifelse (b, st, se, (at,atP,af,afP,es,br,brP)) -> let (st', next) = set_at_tree st (l+1) in
                                               let (se', next') = set_at_tree se next in
                                                  (Ifelse (b, st', se', (l,atP,af,afP,es,br,brP)), next')
    | While (b, sb, (at,atP,af,afP,es,br,brP)) -> let (sb', next) = set_at_tree sb (l+1) in
                                           ( While (b, sb', (l,atP,af,afP,es,br,brP)), next)
    | Break (at,atP,af,afP,es,br,brP) -> (Break (l,atP,af,afP,es,br,brP), l+1)
    | Stmtlist (sl, (at,atP,af,afP,es,br,brP)) -> let (sl', next) = set_at_tree_list sl l in 
                                 (Stmtlist (sl', (l,atP,af,afP,es,br,brP)), next)
and set_at_tree_list sl l = match sl with 
    | [] -> ([], l)
    | [s'] -> let (s'', next) = set_at_tree s' l in
                 ([s''], next)
    | s' :: sl' -> (* trees in inverse order *)
                   let (sl'', next) = set_at_tree_list sl' l in 
                     let (s'', next') = set_at_tree s' next in
                        (s'' :: sl'', next');;
                        
(* set the after, escape, and break label in the void_labeld parse tree statement s, from after break *)
let rec set_labelling_tree s after break = (* set the af, es, and br attributes *)
    match s with
    | Prog (sl, (at,atP,af,afP,es,br,brP)) -> 
         let (sl', es') = set_labelling_tree_list sl after break in
            if es' then print_string "Error: no break allowed out of a program\n";
               (Prog (sl', (at,atP,after,afP,false,void_label,brP)), false)
    | Assign (v, a, (at,atP,af,afP,es,br,brP)) ->  
         (Assign (v, a, (at,atP,after,afP,false,void_label,brP)), false)
    | Emptystmt (at,atP,af,afP,es,br,brP) -> 
         (Emptystmt (at,atP,after,afP,false,void_label,brP), false)
    | If (b, st, (at,atP,af,afP,es,br,brP))-> 
         let (st',es') = set_labelling_tree st after break in
            if es' then
               (If (b, st', (at,atP,after,afP,es',break,brP)),es')
            else
               (If (b, st', (at,atP,after,afP,es',void_label,brP)),es')
     | Ifelse (b, st, se, (at,atP,af,afP,es,br,brP)) -> 
          let (st',es') = set_labelling_tree st after break in
             let (se',es'') = set_labelling_tree se after break in
                let es''' = es' || es'' in
                   if es''' then
                      (Ifelse (b, st', se', (at,atP,after,afP,es''',break,brP)), es''')
                   else
                      (Ifelse (b, st', se', (at,atP,after,afP,es''',void_label,brP)), es''')
    | While (b, sb, (at,atP,af,afP,es,br,brP)) -> 
         let (sb',es') = set_labelling_tree sb at after in
            (While (b, sb', (at,atP,after,afP,false,void_label,brP)),false)
    | Break (at,atP,af,afP,es,br,brP) -> 
         (Break (at,atP,after,afP,true,break,brP)), true
    | Stmtlist (sl, (at,atP,af,afP,es,br,brP)) -> 
         let (sl', es') = set_labelling_tree_list sl after break in
            if es' then
               (Stmtlist (sl', (at,atP,after,afP,es',break,brP)), es')
            else
               (Stmtlist (sl', (at,atP,after,afP,es',void_label,brP)), es')
and set_labelling_tree_list sl after break = match sl with
    | [] -> ([], false)
    | [s'] -> let (s'', es') = set_labelling_tree s' after break in
                ([s''], es')
    | s' :: sl' -> let (sl'', es') = set_labelling_tree_list sl' (at_stmt s') break in
                      let (s'', es'') = set_labelling_tree s' after break in
                         let es''' = es' || es'' in
                            (s'' :: sl'', es''');;
    
let built_abstract_syntax p =
   AbstractDomain.initialize (AbstractTree.vars p);
   let p' = void_label_tree p in
      let first_label = 1 in
         let (p'', next) = set_at_tree p' first_label in
            let (p''', b) = (set_labelling_tree p'' next next) in
               p''';;
