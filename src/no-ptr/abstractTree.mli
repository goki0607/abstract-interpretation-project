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
val vars : 'a tree -> AbstractSyntaxExpressions.variable list
