/* File parser.mly  © P. Cousot 2017 */

%{ (* header *)
  
open AbstractSyntaxExpressions
open AbstractTree
open AbstractSyntax

%} /* declarations */

%token PLUS MINUS LT EQ NEQ GT NAND LPAREN RPAREN ASSIGN /* lexer tokens */
%token SEMICOLON IF ELSE WHILE BREAK LBRACKET RBRACKET END
%token <string> IDENT
%token <int> NUM

%nonassoc NO_ELSE    /* evaluated fifth */
%nonassoc ELSE       /* evaluated fourth */
%left NAND           /* evaluated third */
%left PLUS MINUS     /* evaluated second */
%nonassoc UMINUS     /* evaluated first */

%start prog          /* the grammar entry point */
%type  <AbstractTree.bare_tree> prog
%type  <AbstractSyntaxExpressions.aexpr> aexpr
%type  <AbstractSyntaxExpressions.bexpr> bexpr
%type  <AbstractTree.bare_tree> stmt
%type  <AbstractTree.bare_tree list> stmtlist

%% /* rules */

prog:
  | l = stmtlist END                       { Prog (l, ()) } 
		
stmt:
  | x = IDENT ASSIGN a = aexpr SEMICOLON   { Assign (x, a, ()) }
  | SEMICOLON                              { Emptystmt () }
  | IF LPAREN b = bexpr RPAREN s = stmt %prec NO_ELSE   { If (b, s, ()) } 
  | IF LPAREN b = bexpr RPAREN s1 = stmt 
    ELSE s2 = stmt                         { Ifelse (b, s1, s2, ()) }
  | WHILE LPAREN b = bexpr RPAREN s = stmt { While (b, s, ()) }
  | BREAK SEMICOLON                        { Break () }
  | LBRACKET l = stmtlist RBRACKET         { Stmtlist (l, ()) } 
  
stmtlist: 
  | l = stmtlist s = stmt                  { s :: l (* nodes in inverse order *) }
  |                                        { [] } 

aexpr:
  | n = NUM                                { Num n }
  | x = IDENT                              { Var x }
  | a1 = aexpr PLUS a2 = aexpr             { Plus (a1, a2) }
  | a1 = aexpr MINUS a2 = aexpr            { Minus (a1, a2) }
  | MINUS a = aexpr                        { Minus ((Num 0), a) } %prec UMINUS
  | LPAREN a = aexpr RPAREN                { a } 

bexpr:
  | a1 = aexpr LT a2 = aexpr               { Lt (a1, a2) }
  | a1 = aexpr EQ a2 = aexpr               { Eq (a1, a2) }
  | a1 = aexpr NEQ a2 = aexpr              { Neq (a1, a2) }
  | a1 = aexpr GT a2 = aexpr               { Gt (a1, a2) }
  | b1 = bexpr NAND b2 = bexpr             { Nand (b1, b2) }
  | LPAREN b = bexpr RPAREN                { b } 

%% (* trailer *)
