# file "makefile  © P. Cousot 2018"
# changes made by Goktug Saatcioglu

.PHONY: all 
all: analyseTop \
		 analyseBotTop \
		 analyseParity \
		 analyseConstancy \
		 analyseSign \
		 analyseCongruence \
		 analyseOctagonApron \
		 analysePolyElina \
		 analyseReducedSignParity \
		 analyseReducedCongruenceInterval \
		 analysePotentialPointsTo \
		 analysePotentialPointsToInterval

.PHONY: help 
help: 
	@echo "make analyseTop: {_|_} analysis (for variables x,y,z only)"
	@echo "make analyseBotTop: {_|_, T} analysis (for variables x,y,z only)"
	@echo "make analyseParity: parity analysis (for variables x,y,z only)"
	@echo "make analyseSign: sign analysis (for variables x,y,z only)"
	@echo "make analyseConstancy: constancy analysis (for variables x,y,z only)"
	@echo "make analyseCongruence: congruence analysis (for variables x,y,z only)"
	@echo "make analyseInterval: interval analysis (for variables x,y,z only)"
	@echo "make analyseOctagonApron: octagon analysis using Apron (for variables x,y,z only)"
	@echo "make analysePolyElina: polyhedral analysis using Elina (for variables x,y,z only)"
	@echo "make analyseReducedSignParity: reduced product of signs and parity analysis (for variables x,y,z only)"
	@echo "make analyseReducedCongruenceInterval: reduced product of congruence and interval analysis (for variables x,y,z only)"
	@echo "make analysePotentialPointsTo: potential points to analysis (for variables x,y,z only)"
	@echo "make analysePotentialPointsToInterval: combined analysis of potential points to analysis and interval analysis (for variables x,y,z only)"

.PHONY: delete
delete: 
	@/bin/rm -f main *~ .*~ *.cmo *.cmi lexer.ml parser.ml parser.mli parser.conflicts
	@find . -lname '*.*' -delete

.PHONY: clean
clean: 
	@/bin/rm -f *~ .*~ *.cmo *.cmi lexer.ml parser.ml parser.mli parser.conflicts

.PHONY: removeSyms
removeSyms:
	@find . -lname '*.*' -delete

.PHONY: removeMain
removeMain:
	@/bin/rm -f main
	@find . -lname '*.*' -delete

.PHONY: analyseTop \
				analyseBotTop \
				analyseParity \
				analyseSign \
				analyseConstancy \
				analyseCongruence \
				analyseInterval \
				analyseOctagonApron \
				analysePolyElina \
				analyseReducedSignParity \
				analyseReducedCongruenceInterval \
				analysePotentialPointsTo \
				analysePotentialPointsToInterval

analyseTop : delete
	@echo "*** top domain"
	@/bin/rm -f abstractDomain.ml
	ln -s -f domains/top/abstractDomainTop.ml abstractDomain.ml
	ln -s -f domains/top/printerTop.ml printer.ml
	@make --always-make src.no.ptr
	@make --always-make main.none
	@make --always-make examples
	@make --always-make clean

analyseBotTop : delete
	@echo "*** bot/top domain"
	@/bin/rm -f abstractDomain.ml
	ln -s -f domains/bottop/abstractDomainBotTop.ml abstractDomain.ml
	ln -s -f domains/bottop/printerBotTop.ml printer.ml
	@make --always-make src.no.ptr
	@make --always-make main.none
	@make --always-make examples
	@make --always-make clean
	
analyseParity : delete
	@echo "*** parity domain"
	@/bin/rm -f abstractDomain.ml
	ln -s -f domains/parity/abstractDomainParity.ml abstractDomain.ml
	ln -s -f domains/parity/printerParity.ml printer.ml
	@make --always-make src.no.ptr
	@make --always-make main.none
	@make --always-make examples
	@make --always-make clean

analyseSign : delete
	@echo "*** sign domain"
	@/bin/rm -f abstractDomain.ml
	ln -s -f domains/sign/abstractDomainSign.ml abstractDomain.ml
	ln -s -f domains/sign/printerSign.ml printer.ml
	@make --always-make src.no.ptr
	@make --always-make main.none
	@make --always-make examples
	@make --always-make clean

analyseConstancy : delete
	@echo "*** constancy domain"
	@/bin/rm -f abstractDomain.ml
	ln -s -f domains/constancy/abstractDomainConstancy.ml abstractDomain.ml
	ln -s -f domains/constancy/printerConstancy.ml printer.ml
	@make --always-make src.no.ptr
	@make --always-make main.none
	@make --always-make examples
	@make --always-make clean

analyseCongruence : delete
	@echo "*** congruence domain"
	@/bin/rm -f abstractDomain.ml
	ln -s -f domains/congruence/abstractDomainCongruence.ml abstractDomain.ml
	ln -s -f domains/congruence/printerCongruence.ml printer.ml
	@make --always-make src.no.ptr
	@make --always-make main.none
	@make --always-make examples
	@make --always-make clean

analyseInterval : delete
	@echo "*** interval domain"
	@/bin/rm -f abstractDomain.ml
	ln -s -f domains/interval/abstractDomainInterval.ml abstractDomain.ml
	ln -s -f domains/interval/printerInterval.ml printer.ml
	@make --always-make src.no.ptr
	@make --always-make main.none
	@make --always-make examples
	@make --always-make clean

analyseOctagonApron : delete
	@echo "*** octagon domain (apron)"
	@/bin/rm -f abstractDomain.ml
	ln -s -f domains/octagon-apron/abstractDomainOctagonApron.ml abstractDomain.ml
	ln -s -f domains/octagon-apron/printerOctagon.ml printer.ml
	@make --always-make src.no.ptr
	@make --always-make main.oct
	@make --always-make examples
	@make --always-make clean

analysePolyElina : delete
	@echo "*** octagon domain (elina)"
	@/bin/rm -f abstractDomain.ml
	ln -s -f domains/poly-elina/abstractDomainPolyElina.ml abstractDomain.ml
	ln -s -f domains/poly-elina/printerPoly.ml printer.ml
	@make --always-make src.no.ptr
	@make --always-make main.poly
	@make --always-make examples
	@make --always-make clean

analyseReducedSignParity : delete
	@echo "*** reduced product of sign and parity domain"
	@/bin/rm -f abstractDomain.ml
	ln -s -f domains/red-prod-parity-signs/abstractDomainSign.ml abstractDomainSign.ml
	ln -s -f domains/red-prod-parity-signs/abstractDomainParity.ml abstractDomainParity.ml
	ln -s -f domains/red-prod-parity-signs/abstractDomainRedProdParitySign.ml abstractDomain.ml
	ln -s -f domains/red-prod-parity-signs/printerRedProdParitySign.ml printer.ml
	@make --always-make src.no.ptr
	@make --always-make main.redprodsp
	@make --always-make examples
	@make --always-make clean

analyseReducedCongruenceInterval : delete
	@echo "*** reduced product of congruence and interval domain"
	@/bin/rm -f abstractDomain.ml
	ln -s -f domains/red-prod-congruences-intervals/abstractDomainCongruence.ml abstractDomainCongruence.ml
	ln -s -f domains/red-prod-congruences-intervals/abstractDomainInterval.ml abstractDomainInterval.ml
	ln -s -f domains/red-prod-congruences-intervals/abstractDomainRedProdCongruencesInterval.ml abstractDomain.ml
	ln -s -f domains/red-prod-congruences-intervals/printerRedProdCongruenceInterval.ml printer.ml
	@make --always-make src.no.ptr
	@make --always-make main.redprodci
	@make --always-make examples
	@make --always-make clean

analysePotentialPointsTo : delete
	@echo "*** potential points to domain"
	ln -s -f domains/potential-points-to/abstractDomainPotentialPointsTo.ml abstractDomain.ml
	ln -s -f domains/potential-points-to/printerPotentialPointsTo.ml printer.ml
	@make --always-make src.ptr
	@make --always-make main.none
	@make --always-make examples.pt
	@make --always-make clean

analysePotentialPointsToInterval : delete
	@echo "*** potential points to domain with interval domain"
	ln -s -f domains/potential-points-to-interval/abstractDomainInterval.ml abstractDomainInterval.ml
	ln -s -f domains/potential-points-to-interval/abstractDomainPotentialPointsToInterval.ml abstractDomain.ml
	ln -s -f domains/potential-points-to-interval/printerPotentialPointsToInterval.ml printer.ml
	@make --always-make src.ptr
	@make --always-make main.pointi
	@make --always-make examples.pt
	@make --always-make clean

# build:
#	ln -s -f ../V1-17--reachability/abstractSyntaxExpressions.ml .
#	ln -s -f ../V1-17--reachability/lexer.mll .
build:

src.ptr:
	ln -s -f src/ptr/abstractDomain.mli abstractDomain.mli
	ln -s -f src/ptr/abstractInterpreter.ml abstractInterpreter.ml
	ln -s -f src/ptr/abstractSyntax.ml abstractSyntax.ml
	ln -s -f src/ptr/abstractSyntaxExpressions.ml abstractSyntaxExpressions.ml
	ln -s -f src/ptr/abstractTree.ml abstractTree.ml
	ln -s -f src/ptr/abstractTree.mli abstractTree.mli
	ln -s -f src/ptr/fixpoint.ml fixpoint.ml
	ln -s -f src/ptr/lexer.mll lexer.mll
	ln -s -f src/ptr/main.ml main.ml
	ln -s -f src/ptr/parser.mly parser.mly

src.no.ptr:
	ln -s -f src/no-ptr/abstractDomain.mli abstractDomain.mli
	ln -s -f src/no-ptr/abstractInterpreter.ml abstractInterpreter.ml
	ln -s -f src/no-ptr/abstractSyntax.ml abstractSyntax.ml
	ln -s -f src/no-ptr/abstractSyntaxExpressions.ml abstractSyntaxExpressions.ml
	ln -s -f src/no-ptr/abstractTree.ml abstractTree.ml
	ln -s -f src/no-ptr/abstractTree.mli abstractTree.mli
	ln -s -f src/no-ptr/fixpoint.ml fixpoint.ml
	ln -s -f src/no-ptr/lexer.mll lexer.mll
	ln -s -f src/no-ptr/main.ml main.ml
	ln -s -f src/no-ptr/parser.mly parser.mly

main.pointi : build
	@ocamllex -q lexer.mll
	@ocamlc -c abstractSyntaxExpressions.ml
	@ocamlc -c abstractDomain.mli
	@ocamlc -c abstractDomainInterval.ml
	@ocamlc -c abstractDomain.ml
	@ocamlc -c abstractTree.mli
	@ocamlc -c abstractTree.ml
	@ocamlc -c abstractSyntax.ml
	@ocamlc -c fixpoint.ml
	@ocamlc -c printer.ml
	@menhir --infer --explain parser.mly
	@ocamlc -c parser.mli
	@ocamlc -c lexer.ml
	@ocamlc -c parser.ml
	@ocamlc -c abstractInterpreter.ml
	@ocamlc -c main.ml
	@ocamlc abstractSyntaxExpressions.cmo abstractDomainInterval.cmo abstractDomain.cmo abstractTree.cmo abstractSyntax.cmo fixpoint.cmo printer.cmo parser.cmo lexer.cmo abstractInterpreter.cmo main.cmo -o main

main.redprodci : build
	@ocamllex -q lexer.mll
	@ocamlc -c abstractSyntaxExpressions.ml
	@ocamlc -c abstractDomain.mli
	@ocamlc -c abstractDomainCongruence.ml
	@ocamlc -c abstractDomainInterval.ml
	@ocamlc -c abstractDomain.ml
	@ocamlc -c abstractTree.mli
	@ocamlc -c abstractTree.ml
	@ocamlc -c abstractSyntax.ml
	@ocamlc -c fixpoint.ml
	@ocamlc -c printer.ml
	@menhir --infer --explain parser.mly
	@ocamlc -c parser.mli
	@ocamlc -c lexer.ml
	@ocamlc -c parser.ml
	@ocamlc -c abstractInterpreter.ml
	@ocamlc -c main.ml
	@ocamlc abstractSyntaxExpressions.cmo abstractDomainCongruence.cmo abstractDomainInterval.cmo abstractDomain.cmo abstractTree.cmo abstractSyntax.cmo fixpoint.cmo printer.cmo parser.cmo lexer.cmo abstractInterpreter.cmo main.cmo -o main

main.redprodsp : build
	@ocamllex -q lexer.mll
	@ocamlc -c abstractSyntaxExpressions.ml
	@ocamlc -c abstractDomain.mli
	@ocamlc -c abstractDomainSign.ml
	@ocamlc -c abstractDomainParity.ml
	@ocamlc -c abstractDomain.ml
	@ocamlc -c abstractTree.mli
	@ocamlc -c abstractTree.ml
	@ocamlc -c abstractSyntax.ml
	@ocamlc -c fixpoint.ml
	@ocamlc -c printer.ml
	@menhir --infer --explain parser.mly
	@ocamlc -c parser.mli
	@ocamlc -c lexer.ml
	@ocamlc -c parser.ml
	@ocamlc -c abstractInterpreter.ml
	@ocamlc -c main.ml
	@ocamlc abstractSyntaxExpressions.cmo abstractDomainSign.cmo abstractDomainParity.cmo abstractDomain.cmo abstractTree.cmo abstractSyntax.cmo fixpoint.cmo printer.cmo parser.cmo lexer.cmo abstractInterpreter.cmo main.cmo -o main

main.poly : build
	@ocamllex -q lexer.mll
	@ocamlc -c abstractSyntaxExpressions.ml
	@ocamlc -c abstractDomain.mli
	@ocamlfind ocamlc -package apron -package apron.octD -package elina -c abstractDomain.ml
	@ocamlc -c abstractTree.mli
	@ocamlc -c abstractTree.ml
	@ocamlc -c abstractSyntax.ml
	@ocamlc -c fixpoint.ml
	@ocamlc -c printer.ml
	@menhir --infer --explain parser.mly
	@ocamlc -c parser.mli
	@ocamlc -c lexer.ml
	@ocamlc -c parser.ml
	@ocamlc -c abstractInterpreter.ml
	@ocamlc -c main.ml
	@ocamlfind ocamlc -package apron -package apron.octD -package elina -linkpkg abstractSyntaxExpressions.cmo abstractDomain.cmo abstractTree.cmo abstractSyntax.cmo fixpoint.cmo printer.cmo parser.cmo lexer.cmo abstractInterpreter.cmo main.cmo -o main

main.oct : build
	@ocamllex -q lexer.mll
	@ocamlc -c abstractSyntaxExpressions.ml
	@ocamlc -c abstractDomain.mli
	@ocamlfind ocamlc -package apron -package apron.octD -c abstractDomain.ml
	@ocamlc -c abstractTree.mli
	@ocamlc -c abstractTree.ml
	@ocamlc -c abstractSyntax.ml
	@ocamlc -c fixpoint.ml
	@ocamlc -c printer.ml
	@menhir --infer --explain parser.mly
	@ocamlc -c parser.mli
	@ocamlc -c lexer.ml
	@ocamlc -c parser.ml
	@ocamlc -c abstractInterpreter.ml
	@ocamlc -c main.ml
	@ocamlfind ocamlc -package apron -package apron.octD -linkpkg abstractSyntaxExpressions.cmo abstractDomain.cmo abstractTree.cmo abstractSyntax.cmo fixpoint.cmo printer.cmo parser.cmo lexer.cmo abstractInterpreter.cmo main.cmo -o main

main.none : build
	@ocamllex -q lexer.mll
	@ocamlc -c abstractSyntaxExpressions.ml
	@ocamlc -c abstractDomain.mli
	@ocamlc -c abstractDomain.ml
	@ocamlc -c abstractTree.mli
	@ocamlc -c abstractTree.ml
	@ocamlc -c abstractSyntax.ml
	@ocamlc -c fixpoint.ml
	@ocamlc -c printer.ml
	@menhir --infer --explain parser.mly
	@ocamlc -c parser.mli
	@ocamlc -c lexer.ml
	@ocamlc -c parser.ml
	@ocamlc -c abstractInterpreter.ml
	@ocamlc -c main.ml
	@ocamlc abstractSyntaxExpressions.cmo abstractDomain.cmo abstractTree.cmo abstractSyntax.cmo fixpoint.cmo printer.cmo parser.cmo lexer.cmo abstractInterpreter.cmo main.cmo -o main

.PHONY: examples.pt
.IGNORE: examples.pt
examples.pt: main
	@echo "# using the abstract interpreter:"
	@echo "x=&z; y=&z; z=&z; *z=&x;" | ./main
	@echo "x = 3; y = &x; z = &x; *y = *y + *z;" | ./main 
	@echo "x = 1; y = &x; while (x < 10) { *y = 10 + *y; }" | ./main # tough one!
	@echo "x = 1; y = &x; if (x < 10) { x = x + 1; }" | ./main 
	@echo "x = 1; y = &x; while (x < 10) { x = x + 1; break; }" | ./main 
	@echo "x=1; x=x+1; while (x > 1) { if (x > 10) {break;} x = x+1; }" | ./main
	@echo "x=1; y=&x; while (x < 10) { x = x+1; } z=*y;" | ./main 
	@echo "# end"

.PHONY: examples
.IGNORE: examples
examples: main
	@echo "# using the abstract interpreter:"
	@echo ";"| ./main
	@echo "x = 42;"| ./main
	@echo "break;"| ./main
	@echo "break; x = 7;"| ./main
	@echo "x = 7; ; break; "| ./main
	@echo "{}" | ./main
	@echo "x=-10-20--40;" | ./main
	@echo "x=1; y=2;" | ./main
	@echo "{x=10; ; y=20;}" | ./main
	@echo "if (0<1) x=1;" | ./main
	@echo "if (1-2<3-4-5) x=-x;" | ./main
	@echo "if (x<1) if (x<0) x=1; else if (x<0) { x=2; x=3; } else { x=4; x=5; x=6; }" | ./main
	@echo "while (x<1) {}" | ./main
	@echo "while (x<1) x = x + 1;" | ./main
	@echo "while (x<10) x = x + 1;" | ./main
	@echo "while (0<1){x = x - 1;} x = 42;" | ./main
	@echo "while (0<1){}" | ./main
	@echo "x=x-1;while (0<1){x=x-1;if(x<2)break;};" | ./main
	@echo "x=-10; while (x<0) if (x<0) if (0<x) x=-x;" | ./main
	@echo "x=-10; while (x<0) { x=x-1; break; }; x= 10;" | ./main
	@echo "x=0; while (x<0) { while (x<0) x=x-1; x= 10; }; x= 100;" | ./main
	@echo "x=0; while (x<0) { while (x<0) x=x-1; break; }; x= 100;" | ./main
	@echo "x=x-1; while (0<1) { x=x-1; if (2 < x) break; };" | ./main
	@echo "x=10; while (x>0) x=x-1;" | ./main
	@echo "while (0<1){ break; x=1; }" | ./main
	@echo "while (x!=2) { if (x==0) break; if (x==1) break; }" | ./main
	@echo "while (0<1) x=x+2;" |./main
	@echo "x=1;while (x<100){x=(x+1);};" | ./main
	@echo "while (x<100){ if (y==0) x=(x+3); else x=(x+6); y=(y+6); }" | ./main
	@echo "x = 1; y = 7; if (y==7) x = x+y+x+y+x+y;" | ./main
	@echo "x = 1; while (0 < 1) x = x + 2;" | ./main
	@echo "if (((x + x) != y) nand ((y + y) != x)) x = 1; else x = 3;" | ./main
	@echo "while (x < 100) { if (y == 0) x = (x + 3); else x = (x + 6); y = (y + 6); }" | ./main
	@echo "x = 1; while (x < 10000) x = (x + 1); " | ./main
	@echo "x=1; while (x < 1000) x = x+2;" | ./main
	@echo "x=1; if (0 < 0) x = x-1; else x = x+1;" | ./main 
	@echo "if ((x<y nand y<x) nand (x<y nand y<x)) z=1; else z=3;" | ./main
	@echo "x = 0; while (x < 1001) x = (x + 1); " | ./main 
	@echo "# end"
