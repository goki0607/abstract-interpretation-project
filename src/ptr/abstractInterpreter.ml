(* file abstractInterpreter.ml *)
(* implemented by Goktug Saatcioglu *)
(* flow sensitive pointer analysis *)

open AbstractDomain
open AbstractTree
open AbstractSyntax
open Printer

open Fixpoint

let while_leq w1 w2 =
  match w1, w2 with
  | While (b, sb, (at,atP,af,afP,es,br,brP)),
    While (b', sb', (at',atP',af',afP',es',br',brP')) ->
      leq atP atP' && leq afP afP' && leq brP brP'
  | _ -> failwith "Issue with While statement!"

let widen_while w1 w2 =
  match w1, w2 with
  | While (b, sb, (at,atP,af,afP,es,br,brP)),
    While (b', sb', (at',atP',af',afP',es',br',brP')) ->
      let atP'' = widen atP atP' in 
      (*let afP'' = widen afP afP' in
      let brP'' = widen brP brP' in*)
      While (b', sb', (at',atP'',af',afP',es',br',brP'))
  | _ -> failwith "Issue with While statement!"

let narrow_while w1 w2 =
  match w1, w2 with
  | While (b, sb, (at,atP,af,afP,es,br,brP)),
    While (b', sb', (at',atP',af',afP',es',br',brP')) ->
      let atP'' = narrow atP atP' in 
      (*let afP'' = narrow afP afP' in
      let brP'' = narrow brP brP' in*)
      While (b, sb, (at,atP'',af,afP',es,br,brP'))
  | _ -> failwith "Issue with While statement!"

let rec interpret s r =
  match s with
  | Prog (sl, (at,atP,af,afP,es,br,brP)) ->
      let sl', atP', afP', brP' = interpret_list sl r in
      Prog (sl', (at,atP',af,afP',es,br,(bot ())))
  | Assign (v, a, (at,atP,af,afP,es,br,brP)) ->
      let afP' = assign v a r in
      Assign (v, a, (at,r,af,afP',es,br,(bot ())))
  | AssignP (p, a, (at,atP,af,afP,es,br,brP)) ->
      let afP' = assignP p a r in
      AssignP (p, a, (at,r,af,afP',es,br,(bot ())))
  | Emptystmt (at,atP,af,afP,es,br,brP) ->
      Emptystmt (at,r,af,r,es,br,(bot ()))
  | If (b, st, (at,atP,af,afP,es,br,brP)) ->
      let tt = test b r in
      let st' = interpret st tt in
      let afPt = property_after st' in
      let brP' = property_break st' in
      let ff = nottest b r in
      let afP' = join afPt ff in
      If (b, st', (at,r,af,afP',es,br,brP'))
  | Ifelse (b, st, sf, (at,atP,af,afP,es,br,brP)) ->
      let tt = test b r in
      let st' = interpret st tt in
      let ff = nottest b r in
      let sf' = interpret sf ff in
      let afPt = property_after st' in
      let brPt = property_break st' in
      let afPf = property_after sf' in
      let brPf = property_break sf' in
      let afP' = join afPt afPf in
      let brP' = join brPt brPf in
      Ifelse (b, st', sf', (at,r,af,afP',es,br,brP'))
  | While (b, sb, (at,atP,af,afP,es,br,brP)) ->
      let wp = While (b, sb, (at,(bot ()),af,(bot ()),es,br,(bot ()))) in
      let fw = interpret_while r in
      let fp = lfp wp while_leq fw widen_while narrow_while in
      (match fp with
      | While (b', sb'', (at,atP,af,afP,es,br,brP)) ->
          let tt = test b' atP in
          let sb' = interpret sb'' tt in
          While (b', sb', (at,atP,af,afP,es,br,brP))
      | _ -> failwith "Issue with While statement!")
  | Break (at,atP,af,afP,es,br,brP) ->
      Break (at,r,af,(bot ()),es,br,r)
  | Stmtlist (sl, (at,atP,af,afP,es,br,brP)) ->
      let sl', atP', afP', brP' = interpret_list sl r in
      Stmtlist (sl', (at,atP',af,afP',es,br,brP'))
and
interpret_while r s =
  match s with
  | While (b, sb, (at,atP,af,afP,es,br,brP)) ->
      let tt = test b atP in
      let sb' = interpret sb tt in
      let afPb = property_after sb' in
      let brPb = property_break sb' in
      let atP' = join r afPb in 
      let ff = nottest b atP' in
      let afP' = join ff brPb in
      While (b, sb', (at,atP',af,afP',es,br,(bot ())))
      (*let res = While (b, sb', (at,atP',af,afP',es,br,(bot ()))) in
      print_string "\n->\n"; print_labelled_node res 0; print_string "\n]\n";
      res*)
  | _ -> failwith "Issue with While statement!"
and
interpret_list sl r =
  match sl with
  | [] -> [], r, r, (bot ())
  | [s] -> 
      let s' = interpret s r in
      let atP', afP', brP' = property_at s', property_after s', property_break s' in
      [s'], atP', afP', brP'
  | s :: stl ->
      let sl', atP', afPtl, brPtl = interpret_list stl r in
      let s' = interpret s afPtl in
      let afP' = property_after s' in
      let brPs' = property_break s' in 
      let brP' = join brPs' brPtl in
      let res = s' :: sl' in
      res, atP', afP', brP'