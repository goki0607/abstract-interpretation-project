open AbstractSyntaxExpressions
open AbstractDomain

type 'a tree =
  | Prog of 'a tree list * 'a
  | Assign of variable * aexpr * 'a
  | Emptystmt of 'a
  | If of bexpr * 'a tree * 'a
  | Ifelse of bexpr * 'a tree * 'a tree * 'a
  | While of bexpr * 'a tree * 'a
  | Break of 'a
  | Stmtlist of 'a tree list * 'a (* trees in inverse order *)
type program_label = int
type labelling =
    program_label * AbstractDomain.t * program_label * AbstractDomain.t *
    bool * program_label * AbstractDomain.t
    (* escape, where to break, abstract property when breaking *)
type bare_tree = unit tree (* without labeling attributes *)
type labelled_tree = labelling tree (* with labeling attributes *)

let vars s = 
   let rec collect s = match s with 
      | Prog (sl, _) -> collect_stmt_list sl
      | Assign (v, a, _) -> v::(varsa a)
      | Emptystmt _ -> []
      | If (b, st, _)-> (varsb b)@(collect st)
      | Ifelse (b, st, se, _) -> (varsb b)@(collect st)@(collect se)
      | While (b, sb, _) -> (varsb b)@(collect sb)
      | Break _ -> []
      | Stmtlist (sl, _) -> collect_stmt_list sl
   and collect_stmt_list sl = match sl with
      | [] -> []
      | s' :: sl' -> (collect s')@(collect_stmt_list sl')
   in List.sort_uniq Pervasives.compare (collect s)
